import AST

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The instruction's operands.
  var operands: [Value] { get }

}

// MARK: Stack Allocations

/// Allocates a block of uninitalized memory on the stack, large enough to contain a value of the
/// specified type.
public final class AllocStackInst: Value, Inst {

  /// The type of the allocated value.
  public let allocatedType: VILType

  init(allocatedType: VILType) {
    self.allocatedType = allocatedType
    super.init(type: allocatedType.address)
  }

  public var operands: [Value] { [] }

}

// MARK: Memory Access

/// Copies the contents located at the given source address to another location.
public final class CopyAddrInst: Inst {

  /// The target address of the copy.
  public let target: Value

  /// The address of the object to copy.
  public let source: Value

  init(target: Value, source: Value) {
    self.target = target
    self.source = source
  }

  public var operands: [Value] { [target, source] }

}

/// Determines whether two addresses are equal.
public final class EqualAddrInst: Value, Inst {

  /// An address.
  public let lhs: Value

  /// Another address.
  public let rhs: Value

  init(lhs: Value, rhs: Value) {
    self.lhs = lhs
    self.rhs = rhs

    let context = lhs.type.valType.context
    super.init(type: .lower(context.getBuiltinType(named: "i1")!))
  }

  public var operands: [Value] { [lhs, rhs] }

}

/// Loads a value from the specified address.
public final class LoadInst: Value, Inst {

  /// The location to load.
  public let location: Value

  init(location: Value) {
    self.location = location
    super.init(type: location.type.object)
  }

  public var operands: [Value] { [location] }

}

/// Marks a location as being uninitialized.
///
/// The location must be explicitly initialized before it can be access for read or modify, and
/// before the current function returns. Write accesses are allowed.
///
/// This instruction is meant to support definite assignment analysis in raw VIL and should be
/// eliminated from sound VIL.
public final class MarkUninitializedInst: Value, Inst {

  /// The location that is marked uninitialized.
  public let location: Value

  init(location: Value) {
    self.location = location
    super.init(type: location.type)
  }

  /// The location that is assumed uninitialized.
  public var operands: [Value] { [location] }

}

/// Stores a value at the specified address.
public final class StoreInst: Inst {

  /// The location at which the value must be stored.
  public let target: Value

  /// The value being stored.
  public let value: Value

  init(target: Value, value: Value) {
    self.target = target
    self.value = value
  }

  public var operands: [Value] { [target, value] }

}

// MARK: Aggregate Types

/// Creates an uninitialized record value (i.e., the instance of a product type).
public final class RecordInst: Value, Inst {

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  init(typeDecl: NominalTypeDecl, type: VILType) {
    self.typeDecl = typeDecl
    super.init(type: type)
  }

  public var operands: [Value] { [] }

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Value, Inst {

  /// The record value whose member is being extracted.
  public let record: Value

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType) {
    self.record = record
    self.memberDecl = memberDecl
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Value, Inst {

  /// The address of the the record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType) {
    self.record = record
    self.memberDecl = memberDecl
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

}

/// Creates a tuple value.
public final class TupleInst: Value, Inst {

  /// The type of the tuple.
  public let tupleType: TupleType

  /// The value of the tuple's elements.
  public let operands: [Value]

  init(type: TupleType, operands: [Value]) {
    self.tupleType = type
    self.operands = operands
    super.init(type: .lower(tupleType))
  }

}

// MARK: Existential Types

/// Allocates the memory necessary to pack an existential package into the specified container.
///
/// `alloc_existential` returns the address of an uninitialized memory block, large enough to store
/// an instance of the specified witness.
public final class AllocExistentialInst: Value, Inst {

  /// The address of the existential container.
  public let container: Value

  /// The type of the package's witness.
  public let witness: VILType

  init(container: Value, witness: VILType) {
    self.container = container
    self.witness = witness
    super.init(type: witness.address)
  }

  public var operands: [Value] { [container] }

}

/// Extracts the value packed inside an existential container.
public final class OpenExistentialInst: Value, Inst {

  /// The existential container to open.
  public let container: Value

  init(container: Value, type: VILType) {
    self.container = container
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

}

/// Obtains the address of the concrete value packaged inside an existential container.
public final class OpenExistentialAddrInst: Value, Inst {

  /// The address of the existential container to open.
  public let container: Value

  init(container: Value, type: VILType) {
    self.container = container
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

}

/// Creates a function reference to the implementation matching a view method in the witness of an
/// existential package.
public final class WitnessMethodInst: Value, Inst {

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Value

  /// The declaration of a view method.
  ///
  /// This should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Value, decl: BaseFunDecl) {
    self.container = container
    self.decl = decl
    super.init(type: .lower(decl.unappliedType))
  }

  public var operands: [Value] { [container] }

}

// MARK: Cast Operations

/// Attempts to convert an address to a different type.
///
/// `checked_cast_addr` produces an address suitable to load an object of the requested type if
/// the conversion is legal, or a null location otherwise.
public final class CheckedCastAddrInst: Value, Inst {

  /// The address to convert.
  public let source: Value

  init(source: Value, type: VILType) {
    self.source = source
    super.init(type: type)
  }

  public var operands: [Value] { [source] }

}

/// Converts an address to a different type.
///
/// `unsafe_cast_addr` checks whether the conversion is legal and fails at runtime if `source` does
/// not have a layout that matches the requested type.
public final class UnsafeCastAddrInst: Value, Inst {

  /// The address to convert.
  public let source: Value

  init(source: Value, type: VILType) {
    self.source = source
    super.init(type: type)
  }

  public var operands: [Value] { [source] }

}

// MARK: Functions

/// Applies a function.
public final class ApplyInst: Value, Inst {

  /// The function being applied.
  public let callee: Value

  /// The arguments of the function application.
  public let args: [Value]

  init(callee: Value, args: [Value], type: VILType) {
    self.callee = callee
    self.args = args
    super.init(type: type)
  }

  public var operands: [Value] { [callee] + args }

}

/// Creates the partial application of a function.
public final class PartialApplyInst: Value, Inst {

  /// The function being partially applied.
  public let delegator: Value

  /// The partial list of arguments of the function application (from left to right).
  public let partialArgs: [Value]

  init(delegator: Value, partialArgs: [Value]) {
    self.delegator = delegator
    self.partialArgs = partialArgs

    let context = delegator.type.valType.context
    let baseValType = delegator.type.valType as! FunType
    let partialValType = context.funType(
      paramType: context.tupleType(types: baseValType.paramTypeList.dropLast(partialArgs.count)),
      retType: baseValType.retType)
    super.init(type: .lower(partialValType))
  }

  public var operands: [Value] { [delegator] + partialArgs }

}

/// Wraps a bare function reference into a thick function container with an empty environment.
///
/// Bare function references can only appear as operands. This instruction serves to wrap them into
/// a thick container so that they have the same layout as partially applied functions.
public final class ThinToThickInst: Value, Inst {

  /// A bare reference to a VIL function.
  public let ref: FunRef

  public init(ref: FunRef) {
    self.ref = ref
    super.init(type: ref.type)
  }

  public var operands: [Value] { [ref] }

}

// MARK: Async Expressions

/// Creates an asynchronous value.
public final class AsyncInst: Value, Inst {

  /// A bare reference to the function that represents the asynchronous execution.
  public let ref: FunRef

  /// The values captured by the asynchronous expression, representing the arguments passed to the
  /// underlying function.
  public let captures: [Value]

  init(ref: FunRef, captures: [Value] = []) {
    self.ref = ref
    self.captures = captures
    super.init(type: (ref.type as! VILFunType).retType)
  }

  public var operands: [Value] { [ref] + captures }

}

/// Awaits an asynchronous value.
public final class AwaitInst: Value, Inst {

  /// The value being awaited.
  public let value: Value

  init(value: Value) {
    self.value = value
    super.init(type: .lower((value.type.valType as! AsyncType).base))
  }

  public var operands: [Value] { [value] }

}

// MARK: Terminators

/// Branches unconditionally to the start of a basic block.
public final class BranchInst: Inst {

  /// The block to which the execution should jump.
  public let dest: BasicBlock.ID

  /// The arguments of the destination block.
  public let operands: [Value]

  init(dest: BasicBlock.ID, args: [Value]) {
    self.dest = dest
    self.operands = args
  }

}

/// Branches conditionally to the start of a basic block.
public final class CondBranchInst: Inst {

  /// A Boolean condition.
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

  public var operands: [Value] { [cond] + thenArgs + elseArgs }

}

/// Halts the execution of the program.
public final class HaltInst: Inst {

  public var operands: [Value] { [] }

}

/// Returns from a function.
public final class RetInst: Inst {

  /// The value being returned.
  public let value: Value

  init(value: Value) {
    self.value = value
  }

  public var operands: [Value] { [value] }

}
