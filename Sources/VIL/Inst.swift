import AST

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The instruction's operands.
  var operands: [Value] { get }

}

/// The absolute path of an instruction.
///
/// The path is stable: it is not invalidated by the insertion or removal of other instructions at
/// any position, in any basic block.
public struct InstPath: Hashable {

  /// The name of the function in which the instruction resides.
  var funName: VILName

  /// The basic block in which the instruction resides.
  var blockID: BasicBlock.ID

  /// The index of the instruction in the containing basic block.
  var instIndex: BasicBlock.Index

}

/// The kind of an ownership use.
public enum OwnershipUseKind {

  case copy

  case move

}

// MARK: Stack Allocations

/// Allocates a block of uninitalized memory on the stack, large enough to contain a value of the
/// specified type.
public final class AllocStackInst: Value, Inst {

  /// The type of the allocated value.
  public let allocatedType: VILType

  /// The Val declaration related to the allocation, for debugging.
  public private(set) unowned var decl: ValueDecl?

  /// A flag indicating whether the allocated value is `self` in a constructor.
  ///
  /// This flag should only be set for the `alloc_stack` representing the allocation of `self` in
  /// the constructor of a product type.
  public let isSelf: Bool

  init(allocatedType: VILType, decl: ValueDecl?, isSelf: Bool) {
    self.allocatedType = allocatedType
    self.decl = decl
    self.isSelf = isSelf
    super.init(type: allocatedType.address)
  }

  public var operands: [Value] { [] }

}

/// Deallocates memory previously allocated by `alloc_stack`.
///
/// This instruction formally terminates the lifetime of the allocation. Accessing the referenced
/// memory after `dealloc_stack` causes undefined behavior.
///
/// Deallocation must be in first-in last-out order: the operand must denote the last `alloc_stack`
/// preceeding the deallocation.
public final class DeallocStackInst: Inst {

  /// The corresponding stack allocation.
  public let alloc: AllocStackInst

  init(alloc: AllocStackInst) {
    self.alloc = alloc
  }

  public var operands: [Value] { [alloc] }

}

// MARK: Memory Access

/// Assigns a copy of the contents located at a source address to another location.
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

/// Destroys the contents located at the specified address, calling its destructor and leaving the
/// memory uninitialized.
///
/// The specified address must be initialized. If the referenced memory is bound to an existential
/// type, it must hold an initialized container.
public final class DeleteAddrInst: Inst {

  /// The address of the object to delete.
  public let target: Value

  init(target: Value) {
    self.target = target
  }

  public var operands: [Value] { [target] }

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

/// Loads the value at the specified address.
///
/// The instruction operates on the source location directly. Hence, if it is assigned to an
/// existential container, the entire container is loaded, not only the packaged value.
public final class LoadInst: Value, Inst {

  /// The semantics of a `load` instruction.
  public enum Semantics {

    /// The value is loaded by copy. The value at the source location must have a copyable type.
    case copy

    /// The value is loaded by move.
    case move

  }

  /// The location load.
  public let location: Value

  /// The semantics of the load.
  public let semantics: Semantics

  init(location: Value, semantics: Semantics) {
    self.location = location
    self.semantics = semantics
    super.init(type: location.type.object)
  }

  public var operands: [Value] { [location] }

}

/// Stores a consumable value at the specified address, transferring its ownership.
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
///
/// FIXME: Currently, the only use of this intruction is to create `Nil` instances. If there are no
/// other use cases, then it should be removed for something more specific.
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

  /// The kind of the ownership use of the record value.
  public let useKind: OwnershipUseKind

  /// The record value whose member is being extracted.
  public let record: Value

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  init(useKind: OwnershipUseKind, record: Value, memberDecl: VarDecl, type: VILType) {
    self.useKind = useKind
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

/// Copies the value packaged inside of an existential container.
public final class CopyExistentialInst: Value, Inst {

  /// The existential container from which the pacakged value is copied.
  public let container: Value

  init(container: Value, type: VILType) {
    self.container = container
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

}

/// Initializes the value packaged inside of an existential container.
public final class InitExistentialAddrInst: Inst {

  /// The address of the existential container to initialize.
  public let container: Value

  /// The value that initializes the container.
  public let value: Value

  init(container: Value, value: Value) {
    self.container = container
    self.value = value
  }

  public var operands: [Value] { [container, value] }

}

/// Projects the address of the concrete value packaged inside of an existential container.
public final class ProjectExistentialAddrInst: Value, Inst {

  /// The address of the existential container to project.
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
