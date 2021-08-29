import AST

/// A VIL instruction.
public protocol Inst: AnyObject {}

/// Allocates a block of uninitalized memory on the stack, large enough to contain a value of the
/// specified type.
public final class AllocStackInst: Inst, Value {

  /// The type of the allocated value.
  public let allocatedType: VILType

  public var type: VILType { allocatedType.address }

  init(allocatedType: VILType) {
    assert(allocatedType.isObject, "allocated type must be an object type")
    self.allocatedType = allocatedType
  }

}

/// Allocates the memory necessary to pack an existential package into the specified container.
///
/// `alloc_existential` returns the address of an uninitialized memory block, large enough to store
/// an instance of the specified witness.
public final class AllocExistentialInst: Inst, Value {

  /// The address of the existential container.
  public let container: Value

  /// The type of the package's witness.
  public let witness: VILType

  public var type: VILType { witness.address }

  init(container: Value, witness: VILType) {
    assert(witness.isObject, "type witness must be an object type")
    assert(!witness.isExistential, "type witness cannot be existential")

    self.container = container
    self.witness = witness
  }

}

/// Extracts the value packed inside an existential container.
public final class OpenExistentialInst: Inst, Value {

  /// The existential container.
  public let container: Value

  public var type: VILType

  init(container: Value, type: VILType) {
    assert(container.type.isObject, "container must have an object type")

    self.container = container
    self.type = type
  }

}

/// Obtains the address of the concrete value packaged inside an existential container.
public final class OpenExistentialAddrInst: Inst, Value {

  /// The address of the existential container to open.
  public let container: Value

  /// The type of the opened address.
  public let type: VILType

  init(container: Value, type: VILType) {
    assert(container.type.isAddress, "container must have an address type")
    assert(type.isAddress, "type must be an address type")

    self.container = container
    self.type = type
  }

}

/// Copies the contents located at the given source address to another location.
public final class CopyAddrInst: Inst {

  /// The target address of the copy.
  public let dest: Value

  /// The address of the object to copy.
  public let source: Value

  init(dest: Value, source: Value) {
    self.dest = dest
    self.source = source
  }

}

/// Converts an address to a different type.
///
/// `unsafe_cast_addr` checks whether the conversion is legal and fails at runtime if `source` does
/// not have a layout that matches the requested type.
public final class UnsafeCastAddrInst: Inst, Value {

  /// The address to convert.
  public let source: Value

  /// The type to which the address is converted.
  ///
  /// This must be an address type.
  public let type: VILType

  init(source: Value, type: VILType) {
    assert(type.isAddress, "type must be an address type")

    self.source = source
    self.type = type
  }

}

/// Attempts to convert an address to a different type.
///
/// `checked_cast_addr` produces an address suitable to load an object of the requested type if the
/// conversion is legal, or a null location otherwise.
public final class CheckedCastAddrInst: Inst, Value {

  /// The address to convert.
  public let source: Value

  /// The type to which the address is converted.
  ///
  /// This must be an address type.
  public let type: VILType

  init(source: Value, type: VILType) {
    assert(type.isAddress, "type must be an address type")

    self.source = source
    self.type = type
  }

}

/// Creates a reference to the implementation of a view method for the witness of an existential
/// package.
public final class WitnessMethodInst: Inst, Value {

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Value

  /// The declaration of a view method.
  ///
  /// This should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  public var type: VILType { .lower(decl.unappliedType) }

  init(container: Value, decl: BaseFunDecl) {
    self.container = container
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

  init(fun: Value, args: [Value], type: VILType) {
    self.fun = fun
    self.args = args
    self.type = type
  }

}

/// Creates the partial application of a function.
public final class PartialApplyInst: Inst, Value {

  /// The function being partially applied.
  public let fun: Value

  /// The partial list of arguments of the function application (from left to right).
  public let args: [Value]

  public let type: VILType

  init(fun: Value, args: [Value]) {
    assert(args.count > 0)
    self.fun = fun
    self.args = args

    let context = fun.type.valType.context
    let baseValType = fun.type.valType as! FunType
    let partialValType = context.funType(
      paramType: context.tupleType(types: baseValType.paramTypeList.dropLast(args.count)),
      retType: baseValType.retType)

    self.type = .lower(partialValType)
  }

}

/// Wraps a bare function reference into a thick function container with an empty environment.
///
/// Bare function references can only appear as operands. This instruction serves to wrap them into
/// a thick container so that they have the same layout as partially applied functions.
public final class ThinToThickInst: Inst, Value {

  /// A bare reference to a VIL function.
  public let ref: FunRef

  public init(ref: FunRef) {
    self.ref = ref
  }

  public var type: VILType { ref.type }

}

/// Creates an uninitialized record value (i.e., the instance of a product type).
public final class RecordInst: Inst, Value {

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  public let type: VILType

  init(typeDecl: NominalTypeDecl, type: VILType) {
    assert(type.isObject, "instruction must have an object type")

    self.typeDecl = typeDecl
    self.type = type
  }

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Inst, Value {

  /// The record value whose member is being extracted.
  public let record: Value

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  /// The type of the record member.
  public let type: VILType

  init(record: Value, memberDecl: VarDecl, type: VILType) {
    assert(type.isObject, "instruction must have an object type")

    self.record = record
    self.memberDecl = memberDecl
    self.type = type
  }

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Inst, Value {

  /// The address of the the record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  /// The type of an address to the record member.
  public var type: VILType

  init(record: Value, memberDecl: VarDecl, type: VILType) {
    assert(type.isAddress, "instruction must have an address type")

    self.record = record
    self.memberDecl = memberDecl
    self.type = type
  }

}

/// Creates a tuple value.
public final class TupleInst: Inst, Value {

  /// The type of the tuple.
  public let tupleType: TupleType

  /// The value of the tuple's elements.
  public let elems: [Value]

  public var type: VILType { .lower(tupleType) }

  init(type: TupleType, elems: [Value]) {
    self.tupleType = type
    self.elems = elems
  }

}

/// Creates an asynchronous value.
public final class AsyncInst: Inst, Value {

  /// The function that represents the asynchronous execution.
  public let fun: Function

  /// The values captured by the asynchronous expression.
  ///
  /// These values are the arguments that are passed to `function`.
  public let args: [Value]

  public var type: VILType {
    let valType = fun.type.retType.valType
    let context = valType.context
    return .lower(context.asyncType(of: valType))
  }

  init(fun: Function, args: [Value] = []) {
    self.fun = fun
    self.args = args
  }

}

/// Awaits an asynchronous value.
public final class AwaitInst: Inst, Value {

  /// The value being awaited.
  public let value: Value

  public var type: VILType { .lower((value.type.valType as! AsyncType).base) }

  init(value: Value) {
    assert(value.type.valType is AsyncType, "awaited value must have an asynchronous type")
    self.value = value
  }

}

/// Stores a value at the specified address.
public final class StoreInst: Inst {

  /// The location (or target) of the store.
  public let lvalue: Value

  /// The value being stored.
  public let rvalue: Value

  init(lvalue: Value, rvalue: Value) {
    assert(lvalue.type.isAddress, "l-value must have an address type")

    self.lvalue = lvalue
    self.rvalue = rvalue
  }

}

/// Loads a value from the specified address.
public final class LoadInst: Inst, Value {

  /// The location to load.
  public let lvalue: Value

  public var type: VILType { lvalue.type.object }

  init(lvalue: Value) {
    assert(lvalue.type.isAddress, "l-value must have an address type")
    self.lvalue = lvalue
  }

}

/// Determines whether two addresses are equal.
public final class EqualAddrInst: Inst, Value {

  /// An address.
  public let lhs: Value

  /// Another address.
  public let rhs: Value

  public let type: VILType

  init(lhs: Value, rhs: Value) {
    assert(lhs.type.isAddress, "lhs must have an address type")
    assert(rhs.type.isAddress, "rhs must have an address type")

    self.lhs = lhs
    self.rhs = rhs

    let context = lhs.type.valType.context
    self.type = .lower(context.getBuiltinType(named: "i1")!)
  }

}

/// Branches unconditionally to the start of a basic block.
public final class BranchInst: Inst {

  /// The block to which the execution should jump.
  public let dest: BasicBlock.ID

  /// The arguments of the destination block.
  public let args: [Value]

  init(dest: BasicBlock.ID, args: [Value]) {
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
  public let thenDest: BasicBlock.ID

  /// The arguments of the "then" destination block.
  public let thenArgs: [Value]

  /// The block to which the execution should jump if the condition does not hold.
  public let elseDest: BasicBlock.ID

  /// The arguments of the "else" destination block.
  public let elseArgs: [Value]

  init(
    cond: Value,
    thenDest: BasicBlock.ID, thenArgs: [Value],
    elseDest: BasicBlock.ID, elseArgs: [Value]
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

  init(value: Value) {
    self.value = value
  }

}

/// Halts the execution of the program.
public final class HaltInst: Inst {}
