import Utils

/// A box wrapping a type.
private protocol TypeBox {

  /// Hashes the salient parts of the wrapped value into `hasher`.
  func hash(into hasher: inout Hasher)

  /// Returns whether the value wrapped inside `self` is equal to that wrapped inside `other`.
  func equals<Other: TypeBox>(_ other: Other) -> Bool

  /// Returns the value wrapped inside `self` with its type erased.
  func unwrap() -> any TypeProtocol

  /// Returns the value wrapped inside `self` as an instance of `T` or `nil` if that value has a
  /// different type.
  func unwrap<T: TypeProtocol>(as: T.Type) -> T?

  /// Applies `TypeProtocol.transform(_:)` on the types that are part of `self`.
  func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> any TypeProtocol

}

/// A box wrapping an instance of `Base`.
private struct ConcreteTypeBox<Base: TypeProtocol>: TypeBox {

  /// The value wrapped by this instance.
  let base: Base

  func hash(into hasher: inout Hasher) {
    base.hash(into: &hasher)
  }

  func equals<Other: TypeBox>(_ other: Other) -> Bool {
    base == other.unwrap(as: Base.self)
  }

  func unwrap() -> any TypeProtocol {
    base
  }

  func unwrap<T: TypeProtocol>(as: T.Type) -> T? {
    base as? T
  }

  func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> any TypeProtocol {
    base.transformParts(transformer)
  }

}

/// The (static) type of an entity.
public struct AnyType: TypeProtocol {

  /// Val's `Any` type.
  public static let any = ^ExistentialType(traits: [], constraints: [])

  /// Val's `Never` type.
  public static let never = ^SumType([])

  /// Val's `Void` type.
  public static let void = ^TupleType([])

  /// A shorthand for `^ErrorType()`.
  public static let error = ^ErrorType()

  /// Returns the given built-in type symbol wrapped in a type-erased container.
  public static func builtin(_ type: BuiltinType) -> AnyType { ^type }

  /// The value wrapped by this instance.
  private var wrapped: TypeBox

  /// Creates a type-erased container wrapping the given instance.
  ///
  /// - Parameter base: A type to wrap.
  public init<T: TypeProtocol>(_ base: T) {
    if let t = base as? AnyType {
      self.wrapped = t.wrapped
    } else {
      self.wrapped = ConcreteTypeBox(base: base)
    }
  }

  /// Accesses value wrapped by this instance.
  ///
  /// The `base` property can be cast back to its original type using one of the type casting
  /// operators (`as?`, `as!`, or `as`).
  public var base: any TypeProtocol {
    get { wrapped.unwrap() }
    set { wrapped = AnyType(newValue).wrapped }
  }

  /// Indicates whether `self` is a leaf type.
  ///
  /// A leaf type is a type whose only subtypes are itself and `Never`.
  public var isLeaf: Bool {
    switch base {
    case is ExistentialType, is LambdaType, is TypeVariable:
      return false
    case let type as SumType:
      return type.elements.isEmpty
    default:
      return true
    }
  }

  /// Indicates whether `self` is the error type.
  public var isError: Bool {
    base is ErrorType
  }

  /// Indicates whether `self` is Val's `Void` or `Never` type.
  ///
  /// - Requires: `self` is canonical.
  public var isVoidOrNever: Bool {
    (self == .void) || (self == .never)
  }

  /// Indicates whether `self` is a generic type parameter or associated type.
  public var isTypeParam: Bool {
    (base is AssociatedTypeType) || (base is GenericTypeParameterType)
  }

  /// Indicates whether `self` has a record layout.
  public var hasRecordLayout: Bool {
    switch base {
    case is ProductType, is TupleType:
      return true
    case let type as BoundGenericType:
      return type.base.hasRecordLayout
    default:
      return false
    }
  }

  public var flags: TypeFlags { base.flags }

  public var skolemized: AnyType { base.skolemized }

  public func transformParts(_ transformer: (AnyType) -> TypeTransformAction) -> AnyType {
    AnyType(wrapped.transformParts(transformer))
  }

}

extension AnyType: CompileTimeValue {

  public var staticType: AnyType { ^MetatypeType(of: self) }

}

extension AnyType: Equatable {

  /// Returns whether `l` is equal to `r`.
  public static func == (l: Self, r: Self) -> Bool {
    l.wrapped.equals(r.wrapped)
  }

  /// Returns whether `l` is equal to `r`.
  public static func == <T: TypeProtocol>(l: Self, r: T) -> Bool {
    l.wrapped.unwrap(as: T.self) == r
  }

  /// Returns whether `l` is not equal to `r`.
  public static func != <T: TypeProtocol>(l: Self, r: T) -> Bool {
    !(l == r)
  }

  /// Returns whether `l` is equal to `r`.
  public static func == <T: TypeProtocol>(l: T, r: Self) -> Bool {
    l == r.wrapped.unwrap(as: T.self)
  }

  /// Returns whether `l` is not equal to `r`.
  public static func != <T: TypeProtocol>(l: T, r: Self) -> Bool {
    !(l == r)
  }

  /// Returns whether `subject` matches `pattern`.
  ///
  /// This operator is used in switch statements to match the wrapped value of an `AnyType`.
  ///
  ///     func foo(_ x: AnyType) {
  ///       switch x.base {
  ///       case AnyType.any:
  ///         print("type is 'Any'")
  ///       case AnyType.never:
  ///         print("type is 'Never'")
  ///       default:
  ///         print("type is neither 'Any' nor 'Never'")
  ///       }
  ///     }
  public static func ~= (pattern: Self, subject: any TypeProtocol) -> Bool {
    pattern == subject
  }

  /// Returns whether `subject` matches `pattern`.
  public static func ~= (pattern: Self, subject: Self) -> Bool {
    pattern == subject
  }

  /// Returns whether `subject` matches `pattern`.
  public static func ~= <T: TypeProtocol>(pattern: T, subject: Self) -> Bool {
    pattern == subject
  }

}

extension AnyType: Hashable {

  public func hash(into hasher: inout Hasher) {
    wrapped.hash(into: &hasher)
  }

}

extension AnyType: CustomStringConvertible {

  public var description: String { String(describing: base) }

}

/// Creates a type-erased container wrapping the given instance.
public prefix func ^ <T: TypeProtocol>(_ base: T) -> AnyType {
  AnyType(base)
}
