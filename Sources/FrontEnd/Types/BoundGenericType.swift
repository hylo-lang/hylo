import OrderedCollections
import Utils

/// A generic type bound to arguments.
public struct BoundGenericType: TypeProtocol {

  public typealias Arguments = OrderedDictionary<GenericParameterDecl.ID, CompileTimeValue>

  /// The underlying generic type.
  public let base: AnyType

  /// The type and value arguments of the base type.
  public let arguments: Arguments

  public let flags: ValueFlags

  /// Creates a bound generic type binding `base` to the given `arguments`.
  ///
  /// - Requires: `arguments` is not empty.
  public init<T: TypeProtocol>(_ base: T, arguments: Arguments) {
    precondition(!arguments.isEmpty)
    self.base = ^base
    self.arguments = arguments

    var flags = arguments.reduce(base.flags) { (fs, a) in
      fs | a.value.flags
    }

    // The type isn't canonical if `base` is structural.
    switch self.base.base {
    case let t as GenericTypeParameterType where arguments[t.decl] == nil:
      break
    case is ProductType:
      break
    default:
      flags.insert(.hasNonCanonical)
    }

    self.flags = flags
  }

  /// Applies `TypeProtocol.transform(mutating:_:)` on `m` and the types that are part of `self`.
  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    BoundGenericType(
      base.transform(mutating: &m, transformer),
      arguments: arguments.mapValues({ (a) -> CompileTimeValue in
        if case .type(let t) = a {
          return .type(t.transform(mutating: &m, transformer))
        } else {
          UNIMPLEMENTED()
        }
      }))
  }

  /// Applies `transform` on `m` and the generic arguments that are part of `self`.
  public func transformArguments<M>(
    mutating m: inout M, _ transform: (inout M, CompileTimeValue) -> CompileTimeValue
  ) -> Self {
    let transformed = arguments.mapValues({ (a) in transform(&m, a) })
    return .init(base, arguments: transformed)
  }

}

extension BoundGenericType: Equatable {

  /// Returns `true` iff `l` is syntactically equal to `r`.
  public static func == (l: Self, r: Self) -> Bool {
    guard l.base == r.base else { return false }
    return l.arguments.elementsEqual(r.arguments) { (a, b) in
      (a.key == b.key) && (a.value == b.value)
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
