/// A value computed at compile-time.
public protocol CompileTimeValue: Hashable {

  /// The type of this value determined at compile-time.
  ///
  /// This property denotes the type given to the value for the purpose of type checking and code
  /// generation. Its actual type of the might differ at run-time (e.g., if this value inhabits a
  /// union type).
  var staticType: AnyType { get }

}

extension CompileTimeValue {

  /// `true` if `self` is a `TypeVariable`.
  public var isTypeVariable: Bool {
    (self as? AnyType)?.base is TypeVariable
  }

  /// Returns `true` if `self` is equal to `other`.
  public func equals(_ other: any CompileTimeValue) -> Bool {
    if let r = other as? Self {
      return self == r
    } else {
      return false
    }
  }

}
