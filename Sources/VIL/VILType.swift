import AST

/// A Val type that has been lowered to its VIL representation.
public enum VILType {

  case object(ValType)

  case address(ValType)

}
