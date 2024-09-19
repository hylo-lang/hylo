import Utils

/// A box wrapping a term.
private protocol TermBox {

  /// Hashes the salient parts of the wrapped value into `hasher`.
  func hash(into hasher: inout Hasher)

  /// Returns whether the value wrapped inside `self` is equal to that wrapped inside `other`.
  func equals<Other: TermBox>(_ other: Other) -> Bool

  /// Returns the value wrapped inside `self` with its type erased.
  func unwrap() -> any TermProtocol

  /// Returns the value wrapped inside `self` as an instance of `T` or `nil` if that value has a
  /// different type.
  func unwrap<T: TermProtocol>(as: T.Type) -> T?

}

/// A box wrapping an instance of `Base`.
private struct ConcreteTermBox<Base: TermProtocol>: TermBox {

  /// The value wrapped by this instance.
  let base: Base

  func hash(into hasher: inout Hasher) {
    base.hash(into: &hasher)
  }

  func equals<Other: TermBox>(_ other: Other) -> Bool {
    base == other.unwrap(as: Base.self)
  }

  func unwrap() -> any TermProtocol {
    base
  }

  func unwrap<T: TermProtocol>(as: T.Type) -> T? {
    base as? T
  }

}

/// The compile-time representation of the value of an expression.
public struct AnyTerm {

  /// A shorthand for `^ErrorTerm()`.
  public static let error = ^ErrorTerm()

  /// The value wrapped by this instance.
  private var wrapped: TermBox

  /// Creates a type-erased container wrapping the given instance.
  ///
  /// - Parameter base: A type to wrap.
  public init<T: TermProtocol>(_ base: T) {
    if let t = base as? AnyTerm {
      self.wrapped = t.wrapped
    } else {
      self.wrapped = ConcreteTermBox(base: base)
    }
  }

  /// Accesses value wrapped by this instance.
  ///
  /// The `base` property can be cast back to its original type using one of the type casting
  /// operators (`as?`, `as!`, or `as`).
  public var base: any TermProtocol {
    wrapped.unwrap()
  }

}

extension AnyTerm: TermProtocol {

  public var flags: ValueFlags { base.flags }

}

extension AnyTerm: Equatable {

  /// Returns whether `l` is syntactically equal to `r`.
  public static func == (l: Self, r: Self) -> Bool {
    l.wrapped.equals(r.wrapped)
  }

}

extension AnyTerm: Hashable {

  public func hash(into hasher: inout Hasher) {
    wrapped.hash(into: &hasher)
  }

}

extension AnyTerm: CustomStringConvertible {

  public var description: String { String(describing: base) }

}

/// Creates a type-erased container wrapping the given instance.
public prefix func ^ <T: TermProtocol>(_ base: T) -> AnyTerm {
  AnyTerm(base)
}
