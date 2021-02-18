import AST

/// The representation of runtime value.
public protocol Value: AnyObject {

  /// The type of the value.
  var type: VILType { get }

}

// MARK: Constants

/// A literal value.
public class LiteralValue: Value {

  /// The type of the constant.
  public let type: VILType

  fileprivate init(type: VILType) {
    self.type = type
  }

}

/// A constant "unit" value.
public final class UnitValue: LiteralValue, CustomStringConvertible {

  public init(context: AST.Context) {
    super.init(type: .object(context.unitType))
  }

  public var description: String { "unit" }

}

/// A constant integer value.
public final class IntLiteralValue: LiteralValue, CustomStringConvertible {

  /// The literal's value.
  public let value: Int

  public init(value: Int, context: AST.Context) {
    self.value = value
    super.init(type: .object(context.getBuiltinType(named: "IntLiteral")!))
  }

  public var description: String {
    return String(describing: value)
  }

}

/// A reference to a built-in function.
public final class BuiltinFunRef: LiteralValue, CustomStringConvertible {

  /// The built-in function declaration that is being referred.
  public let decl: FunDecl

  public init(decl: FunDecl) {
    precondition(decl.isBuiltin)
    self.decl = decl
    super.init(type: .object(decl.type))
  }

  public var description: String {
    return "b\"\(decl.name)\""
  }

}

/// A reference to a VIL function.
public final class FunRef: LiteralValue, CustomStringConvertible {

  /// The function being referenced.
  public unowned let function: Function

  init(function: Function) {
    self.function = function
    super.init(type: .object(function.type))
  }

  public var description: String {
    return "@\(function.name)"
  }

}

/// An error value.
public final class ErrorValue: LiteralValue, CustomStringConvertible {

  public init(context: AST.Context) {
    super.init(type: .object(context.errorType))
  }

  public var description: String { "error" }

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
