import AST
import Basic

/// The representation of runtime value.
public class Value {

  /// The type of the value.
  public let type: VILType

  /// The uses of this value.
  public var uses: [Use] = []

  init(type: VILType) {
    self.type = type
  }

}

/// A pair representing the use of a value in an instruction.
public struct Use: Hashable {

  /// The path of the user that contains this use.
  public let userPath: InstPath

  /// The index of this use in `user`'s operands.
  public let index: Int

}

// MARK: Constants

/// A literal value.
public class LiteralValue: Value {}

/// A poison value.
public final class PoisonValue: LiteralValue, CustomStringConvertible {

  public var description: String { "poison" }

}

/// A constant "unit" value.
public final class UnitValue: LiteralValue, CustomStringConvertible {

  public var description: String { "unit" }

  private static var _instance: UnitValue?

}

/// A constant integer value.
public final class IntLiteralValue: LiteralValue, CustomStringConvertible {

  /// The literal's value.
  public let value: Int

  init(value: Int, context: Context) {
    self.value = value
    super.init(type: .lower(context.getBuiltinType(named: "IntLiteral")!))
  }

  public var description: String {
    return String(describing: value)
  }

}

/// A reference to a built-in function.
public final class BuiltinFunRef: LiteralValue, CustomStringConvertible {

  /// The built-in function declaration that is being referred.
  public let decl: FunDecl

  init(decl: FunDecl) {
    precondition(decl.isBuiltin)
    self.decl = decl
    super.init(type: .lower(decl.type))
  }

  public var description: String {
    return "b\"\(decl.name)\""
  }

}

/// A reference to a VIL function.
public final class FunRef: LiteralValue, CustomStringConvertible {

  /// The name of the function being referenced.
  public let name: VILName

  init(function: VILFun) {
    self.name = function.name
    super.init(type: function.type)
  }

  public var description: String {
    return "@\(name)"
  }

}

/// A null location.
public final class NullAddr: LiteralValue, CustomStringConvertible {

  override init(type: VILType) {
    assert(type.isAddress, "type must be an address type")
    super.init(type: type)
  }

  public var description: String { "null_addr" }

}

// MARK: Arguments

/// The formal argument (a.k.a. parameter) of a block or function.
public final class ArgumentValue: Value {

  /// A back reference to the basic block in which the formal argument is defined.
  public let parentBlockID: BasicBlock.ID

  init(type: VILType, parentBlockID: BasicBlock.ID) {
    self.parentBlockID = parentBlockID
    super.init(type: type)
  }

}
