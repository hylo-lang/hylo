import AST

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The result of the instruction.
  var result: Value? { get }

}

/// Allocates a block of uninitalized memory on the stack.
public final class AllocStackInst: Inst, Value {

  /// The type of the allocated object.
  public let allocatedType: ValType

  public var type: VILType { .address(allocatedType) }

  public var result: Value? { self }

  init(allocatedType: ValType) {
    self.allocatedType = allocatedType
  }

}

/// Allocates the memory necessary to pack an existential package into the specified container.
public final class AllocExistentialInst: Inst, Value {

  /// The address of the existential container.
  public let container: Value

  /// The type of the package's witness.
  public let witness: ValType

  public var type: VILType { .address(witness) }

  public var result: Value? { self }

  init(container: Value, witness: ValType) {
    self.container = container
    self.witness = witness
  }

}

/// Creates a reference to the implementation of a view method for the witness of an existential
/// package.
public final class WitnessFunInst: Inst, Value {

  /// The type of an existential container that conforms to the view declaring the referred method.
  public let base: ValType

  /// The declaration of a view method.
  ///
  /// This should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  public var type: VILType { .object(decl.unappliedType) }

  public var result: Value? { self }

  init(base: ValType, decl: BaseFunDecl) {
    self.base = base
    self.decl = decl
  }

}

/// Applies a function.
public final class ApplyInst: Inst, Value {

  /// The function being applied.
  public let fun: Value

  /// The arguments of the function application.
  public let args: [Value]

  public let type: VILType

  public var result: Value? { self }

  init(fun: Value, args: [Value], type: VILType) {
    self.fun = fun
    self.args = args
    self.type = type
  }

}

/// Creates a record value (i.e., an instance of a product type).
public final class RecordInst: Inst, Value {

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  public var type: VILType { .object(typeDecl.instanceType) }

  public var result: Value? { self }

  init(typeDecl: NominalTypeDecl) {
    self.typeDecl = typeDecl
  }

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Inst, Value {

  /// The record value whose member is extracted.
  public let record: Value

  /// The declaration of the extracted member.
  public let memberDecl: VarDecl

  public var type: VILType { .object(memberDecl.type) }

  public var result: Value? { self }

  init(record: Value, memberDecl: VarDecl) {
    self.record = record
    self.memberDecl = memberDecl
  }

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Inst, Value {

  /// The record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  public var type: VILType { .address(memberDecl.type) }

  public var result: Value? { self }

  init(record: Value, memberDecl: VarDecl) {
    self.record = record
    self.memberDecl = memberDecl
  }

}

/// Creates a tuple value.
public final class TupleInst: Inst, Value {

  /// The type of the tuple.
  public let tupleType: TupleType

  /// The value of the tuple's elements.
  public let elems: [Value]

  public var type: VILType { .object(tupleType) }

  public var result: Value? { self }

  init(type: TupleType, elems: [Value]) {
    self.tupleType = type
    self.elems = elems
  }

}

/// Stores a value at the specified address.
public final class StoreInst: Inst {

  /// The location (or target) of the store.
  public let lvalue: Value

  /// The value being stored.
  public let rvalue: Value

  public var result: Value? { nil }

  init(lvalue: Value, rvalue: Value) {
    self.lvalue = lvalue
    self.rvalue = rvalue
  }

}

/// Loads a value from the specified address.
public final class LoadInst: Inst, Value {

  /// The location to load.
  public let lvalue: Value

  public var type: VILType {
    guard case .address(let valType) = lvalue.type else { fatalError("unreachable") }
    return .object(valType)
  }

  public var result: Value? { self }

  init(lvalue: Value) {
    precondition(lvalue.type.isAddress)
    self.lvalue = lvalue
  }

}

/// Branches unconditionally to the start of a basic block.
public final class BranchInst: Inst {

  /// The block to which the execution should jump.
  public unowned let dest: BasicBlock

  /// The arguments of the destination block.
  public let args: [Value]

  public var result: Value? { nil }

  init(dest: BasicBlock, args: [Value]) {
    precondition(dest.arguments.count == args.count, "invalid arguments")
    // FIXME: Check for argument types.

    self.dest = dest
    self.args = args
  }

}

/// Branches conditionally to the start of a basic block.
public final class CondBranchInst: Inst {

  /// The condition.
  ///
  /// This must be a Boolean value.
  public let cond: Value

  /// The block to which the execution should jump if the condition holds.
  public unowned let thenDest: BasicBlock

  /// The arguments of the "then" destination block.
  public let thenArgs: [Value]

  /// The block to which the execution should jump if the condition does not hold.
  public unowned let elseDest: BasicBlock

  /// The arguments of the "else" destination block.
  public let elseArgs: [Value]

  public var result: Value? { nil }

  init(
    cond: Value,
    thenDest: BasicBlock, thenArgs: [Value],
    elseDest: BasicBlock, elseArgs: [Value]
  ) {
    self.cond = cond
    self.thenDest = thenDest
    self.thenArgs = thenArgs
    self.elseDest = elseDest
    self.elseArgs = elseArgs
  }

}

/// Returns from a function.
public final class RetInst: Inst {

  /// The value being returned.
  public let value: Value

  public var result: Value? { nil }

  init(value: Value) {
    self.value = value
  }

}
