import OrderedCollections
import Utils

/// A generic type bound to arguments.
public struct BoundGenericType: TypeProtocol {

  /// The arguments of a bound generic type.
  public typealias Arguments = OrderedDictionary<GenericParameterDecl.ID, any CompileTimeValue>

  /// The underlying generic type.
  public let base: AnyType

  /// The type and value arguments of the base type.
  public let arguments: Arguments

  public let flags: TypeFlags

  /// Creates a bound generic type binding `base` to the given `arguments`.
  ///
  /// - Requires: `arguments` is not empty.
  public init<T: TypeProtocol>(_ base: T, arguments: Arguments) {
    precondition(!arguments.isEmpty)
    self.base = ^base
    self.arguments = arguments

    var flags: TypeFlags = base.flags
    for (_, a) in arguments {
      if let t = a as? AnyType {
        flags.merge(t.flags)
      } else {
        fatalError("not implemented")
      }
    }
    self.flags = flags
  }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> Self {
    BoundGenericType(
      base.transform(transformer),
      arguments: arguments.mapValues({ (a) -> any CompileTimeValue in
        if let t = a as? AnyType {
          return t.transform(transformer)
        } else {
          fatalError("not implemented")
        }
      }))
  }

}

extension BoundGenericType: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    guard l.base == r.base else { return false }
    return l.arguments.elementsEqual(r.arguments) { (a, b) in
      (a.key == b.key) && a.value.equals(b.value)
    }
  }

}

extension BoundGenericType: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(base)
    for (k, v) in arguments {
      hasher.combine(k)
      hasher.combine(v)
    }
  }

}

extension BoundGenericType: CustomStringConvertible {

  public var description: String {
    switch base.base {
    case is ProductType, is TypeAliasType:
      return "\(base)<\(list: arguments.values)>"
    default:
      return "<\(list: arguments.values)>(\(base))"
    }
  }

}

extension BoundGenericType.Arguments {

  /// Returns this argument list appended with `suffix`.
  ///
  /// - Requires: `self` does not define a value for any of the values defined in `suffix`.
  public func appending(_ suffix: Self) -> Self {
    // Note: `merging` perserves order.
    self.merging(suffix, uniquingKeysWith: { (_, _) in unreachable() })
  }

}
