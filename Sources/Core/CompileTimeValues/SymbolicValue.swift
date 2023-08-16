/// A placeholder for value using in compile-time expressions.
public struct SymbolicValue: CompileTimeValue {

  /// The type of this value determined at compile-time.
  public let staticType: AnyType

  /// Creates an instance with given type.
  public init(staticType: AnyType) {
    self.staticType = staticType
  }

}
