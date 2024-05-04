/// The result of a call to a closure passed to `TypeProtocol.transform(_:)`.
public enum TypeTransformAction {

  case stepInto(AnyType)

  case stepOver(AnyType)

}
