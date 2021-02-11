import AST

/// The representation of runtime value.
public protocol Value {}

/// A constant "unit" value.
public struct UnitValue: Value {

  public init(context: AST.Context) {
    self.type = context.unitType
  }

  /// The type of the value.
  public let type: ValType

}

/// A constant integer literal.
public struct IntLiteralValue: Value {

  public init(type: ValType) {
    self.type = type
  }

  /// The type of the value.
  public let type: ValType

}

/// The incoming argument of a function.
public struct ArgumentValue: Value {

  /// The type of the argument.
  public let type: ValType

  /// The function to which the argument belongs.
  public unowned let function: Function

}

