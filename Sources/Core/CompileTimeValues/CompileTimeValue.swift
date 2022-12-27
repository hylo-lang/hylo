/// A value computed at compile-time.
public protocol CompileTimeValue {

  /// The type of this value determined at compile-time.
  ///
  /// This property denotes the type given to the value for the purpose of type checking and code
  /// generation. Its actual type of the might differ at run-time (e.g., if this value inhabits a
  /// sum type).
  var staticType: AnyType { get }

}
