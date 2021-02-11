import AST

/// The representation of runtime value.
public protocol Value: AnyObject {

  /// The type of the value.
  var type: VILType { get }

}

// MARK: Constants

/// A constant "unit" value.
public final class UnitValue: Value {

  /// The type of the value.
  public let type: VILType

  public init(context: AST.Context) {
    self.type = .object(context.unitType)
  }

}

/// A constant integer literal.
public final class IntLiteralValue: Value {

  public let type: VILType

  public init(type: ValType) {
    self.type = .object(type)
  }

}

/// An error value.
public final class ErrorValue: Value {

  public let type: VILType

  public init(context: AST.Context) {
    self.type = .object(context.errorType)
  }

}

// MARK: Other values

/// The incoming argument of a block or function.
public final class ArgumentValue: Value {

  public let type: VILType

  /// The function to which the argument belongs.
  public unowned let function: Function

  init(type: VILType, function: Function) {
    self.type = type
    self.function = function
  }

}
