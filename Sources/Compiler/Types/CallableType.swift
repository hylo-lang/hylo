/// A type that denotes a callable entity.
public protocol CallableType: TypeProtocol {

  /// The environment of the type.
  var environment: Type { get }

  /// The inputs of the type.
  var inputs: [CallableTypeParameter] { get }

  /// The output of the type.
  var output: Type { get }
  
}
