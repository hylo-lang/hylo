import AST
import Basic

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The instruction's operands.
  var operands: [Value] { get }

  /// The range in Val source corresponding to the instruction, if any.
  var range: SourceRange? { get }

}

/// The absolute path of an instruction.
///
/// An instruction path is stable: it is not invalidated by the insertion or removal of other
/// instructions at any position, in any basic block.
public struct InstPath: Hashable {

  /// The name of the function in which the instruction resides.
  var funName: VILName

  /// The basic block in which the instruction resides.
  var blockID: BasicBlock.ID

  /// The index of the instruction in the containing basic block.
  var instIndex: BasicBlock.Index

}

// MARK: Stack Allocations

/// Allocates a block of uninitalized memory on the stack, large enough to contain a value of the
/// specified type.
public final class AllocStackInst: Value, Inst {

  public let range: SourceRange?

  /// The type of the allocated value.
  public let allocatedType: VILType

  /// The Val declaration related to the allocation, for debugging.
  public private(set) unowned var decl: ValueDecl?

  /// A flag indicating whether the allocated value is `self` in a constructor.
  ///
  /// This flag should only be set for the `alloc_stack` representing the allocation of `self` in
  /// the constructor of a product type.
  public let isSelf: Bool

  init(allocatedType: VILType, decl: ValueDecl?, isSelf: Bool, range: SourceRange?) {
    self.allocatedType = allocatedType
    self.decl = decl
    self.isSelf = isSelf
    self.range = range ?? decl?.range
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

  public let range: SourceRange?

  /// The corresponding stack allocation.
  public let alloc: AllocStackInst

  init(alloc: AllocStackInst, range: SourceRange?) {
    self.alloc = alloc
    self.range = range
  }

  public var operands: [Value] { [alloc] }

}

// MARK: Memory Access

/// Assigns a copy of the contents located at a source address to another location.
public final class CopyAddrInst: Inst {

  public let range: SourceRange?

  /// The target address of the copy.
  public let target: Value

  /// The address of the object to copy.
  public let source: Value

  init(target: Value, source: Value, range: SourceRange?) {
    self.target = target
    self.source = source
    self.range = range
  }

  public var operands: [Value] { [target, source] }

}

/// Destroys the contents located at the specified address, calling its destructor and leaving the
/// memory uninitialized.
///
/// The specified address must be initialized. If the referenced memory is bound to an existential
/// type, it must hold an initialized container.
public final class DeleteAddrInst: Inst {

  public let range: SourceRange?

  /// The address of the object to delete.
  public let target: Value

  init(target: Value, range: SourceRange?) {
    self.target = target
    self.range = range
  }

  public var operands: [Value] { [target] }

}

/// Determines whether two addresses are equal.
public final class EqualAddrInst: Value, Inst {

  public let range: SourceRange?

  /// An address.
  public let lhs: Value

  /// Another address.
  public let rhs: Value

  init(lhs: Value, rhs: Value, range: SourceRange?) {
    self.lhs = lhs
    self.rhs = rhs
    self.range = range

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

  public let range: SourceRange?

  /// The location load.
  public let location: Value

  init(location: Value, range: SourceRange?) {
    self.location = location
    self.range = range
    super.init(type: location.type.object)
  }

  public var operands: [Value] { [location] }

}

/// Stores a consumable value at the specified address, transferring its ownership.
public final class StoreInst: Inst {

  public let range: SourceRange?

  /// The location at which the value must be stored.
  public let target: Value

  /// The value being stored.
  public let value: Value

  init(target: Value, value: Value, range: SourceRange?) {
    self.target = target
    self.value = value
    self.range = range
  }

  public var operands: [Value] { [target, value] }

}

// MARK: Aggregate Types

/// Creates an uninitialized record value (i.e., the instance of a product type).
///
/// FIXME: Currently, the only use of this intruction is to create `Nil` instances. If there are no
/// other use cases, then it should be removed for something more specific.
public final class RecordInst: Value, Inst {

  public let range: SourceRange?

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  init(typeDecl: NominalTypeDecl, type: VILType, range: SourceRange?) {
    self.typeDecl = typeDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [] }

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Value, Inst {

  public let range: SourceRange?

  /// The record value whose member is being extracted.
  public let record: Value

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType, range: SourceRange?) {
    self.record = record
    self.memberDecl = memberDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address of the the record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType, range: SourceRange?) {
    self.record = record
    self.memberDecl = memberDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

}

/// Creates a tuple value.
public final class TupleInst: Value, Inst {

  /// The values of the tuple's elements.
  public let operands: [Value]

  public let range: SourceRange?

  /// The type of the tuple.
  public let tupleType: TupleType

  init(type: TupleType, operands: [Value], range: SourceRange?) {
    self.tupleType = type
    self.operands = operands
    self.range = range
    super.init(type: .lower(tupleType))
  }

}

// MARK: Existential Types

/// Allocates the memory necessary to pack an existential package into the specified container.
///
/// `alloc_existential` returns the address of an uninitialized memory block, large enough to store
/// an instance of the specified witness.
public final class AllocExistentialInst: Value, Inst {

  public let range: SourceRange?

  /// The address of the existential container.
  public let container: Value

  /// The type of the package's witness.
  public let witness: VILType

  init(container: Value, witness: VILType, range: SourceRange?) {
    self.container = container
    self.witness = witness
    self.range = range
    super.init(type: witness.address)
  }

  public var operands: [Value] { [container] }

}

/// Copies the value packaged inside of an existential container.
public final class CopyExistentialInst: Value, Inst {

  public let range: SourceRange?

  /// The existential container from which the pacakged value is copied.
  public let container: Value

  init(container: Value, type: VILType, range: SourceRange?) {
    self.container = container
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

}

/// Initializes the value packaged inside of an existential container.
public final class InitExistentialAddrInst: Inst {

  public let range: SourceRange?

  /// The address of the existential container to initialize.
  public let container: Value

  /// The value that initializes the container.
  public let value: Value

  init(container: Value, value: Value, range: SourceRange?) {
    self.container = container
    self.value = value
    self.range = range
  }

  public var operands: [Value] { [container, value] }

}

/// Projects the address of the concrete value packaged inside of an existential container.
public final class ProjectExistentialAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address of the existential container to project.
  public let container: Value

  init(container: Value, type: VILType, range: SourceRange?) {
    self.container = container
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

}

/// Creates a function reference to the implementation matching a view method in the witness of an
/// existential package.
public final class WitnessMethodInst: Value, Inst {

  public let range: SourceRange?

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Value

  /// The declaration of a view method.
  ///
  /// This should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Value, decl: BaseFunDecl, range: SourceRange?) {
    self.container = container
    self.decl = decl
    self.range = range
    super.init(type: .lower(decl.unappliedType))
  }

  public var operands: [Value] { [container] }

}

// MARK: Cast Operations

/// Attempts to convert an address to a different type.
///
/// `checked_cast_addr` produces an address suitable to load an object of the requested type if
/// the conversion is legal, or a null location otherwise.
@available(*, deprecated, message: "Use CheckedCastBranchInst instead")
public final class CheckedCastAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address to convert.
  public let source: Value

  init(source: Value, type: VILType, range: SourceRange?) {
    self.source = source
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [source] }

}

/// Converts an address to a different type.
///
/// `unsafe_cast_addr` checks whether the conversion is legal and fails at runtime if `source` does
/// not have a layout that matches the requested type.
public final class UnsafeCastAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address to convert.
  public let source: Value

  init(source: Value, type: VILType, range: SourceRange?) {
    self.source = source
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [source] }

}

// MARK: Functions

/// Applies a function.
public final class ApplyInst: Value, Inst {

  public let range: SourceRange?

  /// The function being applied.
  public let callee: Value

  /// The arguments of the function application.
  public let args: [Value]

  init(callee: Value, args: [Value], type: VILType, range: SourceRange?) {
    self.callee = callee
    self.args = args
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [callee] + args }

}

/// Copies a value using its copy-copy constructor (if any).
public final class CopyInst: Value, Inst {

  public let range: SourceRange?

  /// The value being copied.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    self.value = value
    self.range = range
    super.init(type: value.type)
  }

  public var operands: [Value] { [value] }

}

/// Destroys the specified value, calling its destructor.
public final class DeleteInst: Inst {

  public let range: SourceRange?

  /// The value to delete.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    self.value = value
    self.range = range
  }

  public var operands: [Value] { [value] }

}

/// Creates the partial application of a function.
public final class PartialApplyInst: Value, Inst {

  public let range: SourceRange?

  /// The function being partially applied.
  public let delegator: Value

  /// The partial list of arguments of the function application (from left to right).
  public let partialArgs: [Value]

  init(delegator: Value, partialArgs: [Value], range: SourceRange?) {
    self.delegator = delegator
    self.partialArgs = partialArgs
    self.range = range

    let context = delegator.type.valType.context
    let baseValType = delegator.type.valType as! FunType
    let partialValType = context.funType(
      params: baseValType.params.dropLast(partialArgs.count),
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

  public let range: SourceRange?

  /// A bare reference to a VIL function.
  public let ref: FunRef

  public init(ref: FunRef, range: SourceRange?) {
    self.ref = ref
    self.range = range
    super.init(type: ref.type)
  }

  public var operands: [Value] { [ref] }

}

// MARK: Async Expressions

/// Creates an asynchronous value.
public final class AsyncInst: Value, Inst {

  public let range: SourceRange?

  /// A bare reference to the function that represents the asynchronous execution.
  public let ref: FunRef

  /// The values captured by the asynchronous expression, representing the arguments passed to the
  /// underlying function.
  public let captures: [Value]

  init(ref: FunRef, captures: [Value], range: SourceRange?) {
    self.ref = ref
    self.captures = captures
    self.range = range
    super.init(type: ref.type.retType!)
  }

  public var operands: [Value] { [ref] + captures }

}

/// Awaits an asynchronous value.
public final class AwaitInst: Value, Inst {

  public let range: SourceRange?

  /// The value being awaited.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    self.value = value
    self.range = range
    super.init(type: .lower((value.type.valType as! AsyncType).base))
  }

  public var operands: [Value] { [value] }

}

// MARK: Terminators

/// Branches unconditionally to the start of a basic block.
public final class BranchInst: Inst {

  public let operands: [Value]

  public let range: SourceRange?

  /// The block to which the execution should jump.
  public let dest: BasicBlock.ID

  init(dest: BasicBlock.ID, args: [Value], range: SourceRange?) {
    self.dest = dest
    self.operands = args
    self.range = range
  }

}

/// Attempts to convert an existential container to a value of a different type.
///
/// If the conversion succeeds control is transferred to `thenDest` with an owned value of the
/// requested type as argument. Otherwise, control is transferred to `elseDest` with a owned value
/// of the original type as argument. Either way, the ownership of the specified value is consumed.
public final class CheckedCastBranchInst: Inst {

  public let range: SourceRange?

  /// The value to convert.
  public let value: Value

  /// The type to which the value is converted.
  public let type: VILType

  /// The block to which the execution should jump if the cast succeeds.
  public let thenDest: BasicBlock.ID

  /// The block to which the execution should jump if the condition does not hold.
  public let elseDest: BasicBlock.ID

  init(
    value: Value,
    type: VILType,
    thenDest: BasicBlock.ID,
    elseDest: BasicBlock.ID,
    range: SourceRange?
  ) {
    self.value = value
    self.type = type
    self.thenDest = thenDest
    self.elseDest = elseDest
    self.range = range
  }

  public var operands: [Value] { [value] }

}

/// Branches conditionally to the start of a basic block.
public final class CondBranchInst: Inst {

  public let range: SourceRange?

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
    elseDest: BasicBlock.ID, elseArgs: [Value],
    range: SourceRange?
  ) {
    self.cond = cond
    self.thenDest = thenDest
    self.thenArgs = thenArgs
    self.elseDest = elseDest
    self.elseArgs = elseArgs
    self.range = range
  }

  public var operands: [Value] { [cond] + thenArgs + elseArgs }

}

/// Halts the execution of the program.
public final class HaltInst: Inst {

  public let range: SourceRange?

  init(range: SourceRange?) {
    self.range = range
  }

  public var operands: [Value] { [] }

}

/// Returns from a function.
public final class RetInst: Inst {

  public let range: SourceRange?

  /// The value being returned.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    self.value = value
    self.range = range
  }

  public var operands: [Value] { [value] }

}
