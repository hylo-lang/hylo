import AST
import Basic

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The instruction's operands.
  var operands: [Value] { get }

  /// The range in Val source corresponding to the instruction, if any.
  var range: SourceRange? { get }

  /// Dumps the instruction to the given stream.
  func dump<S>(to stream: inout S, with printer: inout PrinterContext) where S: TextOutputStream

  /// The opstring of the instruction in textual VIL.
  static var opstring: String { get }

}

extension Inst {

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S: TextOutputStream {
    if let value = self as? Value {
      printer.write("%\(printer.uniqueID(of: value)) = ", to: &stream)
    }

    printer.write("\(Self.opstring)", to: &stream)

    if !operands.isEmpty {
      let os = operands
        .map({ printer.describe($0) })
        .joined(separator: ", ")
      printer.write(" \(os)", to: &stream)
    }

    printer.write("\n", to: &stream)
  }

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

// MARK: Memory instructions

/// Allocates uninitalized memory on the stack.
///
/// The instruction returns the address of a cell large enough to contain an instance of
/// `allocatedType`. That cell must be deallocated by `dealloc_stack`.
public final class AllocStackInst: Value, Inst {

  public let range: SourceRange?

  /// The type of the allocated value.
  public let allocType: VILType

  /// A flag indicating whether the allocated value represents a constructor's receiver.
  public let isReceiver: Bool

  /// The Val declaration related to the allocation, for debugging.
  public private(set) unowned var decl: ValueDecl?

  init(allocType: VILType, isReceiver: Bool, decl: ValueDecl?, range: SourceRange?) {
    precondition(allocType.isObject, "'allocatedType' must be an object type")

    self.allocType = allocType
    self.isReceiver = isReceiver
    self.decl = decl
    self.range = range ?? decl?.range
    super.init(type: allocType.address)
  }

  public var operands: [Value] { [] }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S: TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = alloc_stack ", to: &stream)
    if isReceiver { printer.write("[receiver] ", to: &stream) }
    printer.write("\(allocType)\n", to: &stream)
  }

  public static var opstring = "alloc_stack"

}

/// Borrows the value at the specified address.
///
/// This instruction is operationally equivalent to the identity function. It is used during VIL
/// analysis to identify the ownership state of the borrowed value.
///
/// If the borrow is immutable, the value at `source` must be owned, borrowed, or projected. If it
/// is owned, it becomes projected. Otherwise, it's typestate does not change.
///
/// If the borrow is mutable, the value at `source` must be owned and it becomes inouted.
public final class BorrowAddrInst: Value, Inst {

  public let range: SourceRange?

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  /// The source address.
  public let source: Value

  init(isMutable: Bool, source: Value, range: SourceRange?) {
    precondition(source.type.isAddress, "'source' must have an address type")

    self.isMutable = isMutable
    self.source = source
    self.range = range
    super.init(type: source.type)
  }

  public var operands: [Value] { [source] }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S : TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = borrow_addr ", to: &stream)
    if isMutable { printer.write("[mutable] ", to: &stream) }
    printer.write(printer.describe(source), to: &stream)
    printer.write("\n", to: &stream)
  }

  public static var opstring = "borrow_addr"

}

/// Deallocates memory previously allocated by `alloc_stack`.
///
/// The memory at `alloc` must be uninitialized. The allocation is formally freed. Accessing the
/// referenced memory after `dealloc_stack` causes undefined behavior.
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

  public static var opstring = "dealloc_stack"

}

/// Destroys the specified value, calling its destructor.
public final class DeleteInst: Inst {

  public let range: SourceRange?

  /// The value to delete.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    precondition(value.type.isObject, "'value' must have an object type")

    self.value = value
    self.range = range
  }

  public var operands: [Value] { [value] }

  public static var opstring = "delete"

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
    precondition(target.type.isAddress, "'target' must have an address type")

    self.target = target
    self.range = range
  }

  public var operands: [Value] { [target] }

  public static var opstring = "delete_addr"

}

/// Loads the value stored at the specified address.
///
/// The instruction operates on the source address directly. Hence, if it is assigned to an
/// existential container, the entire container is loaded, not only the packaged value.
public final class LoadInst: Value, Inst {

  public let range: SourceRange?

  /// The source address.
  public let source: Value

  init(source: Value, range: SourceRange?) {
    precondition(source.type.isAddress, "'source' must have an address type")

    self.source = source
    self.range = range
    super.init(type: source.type.object)
  }

  public var operands: [Value] { [source] }

  public static var opstring = "load"

}

/// Moves the contents from one location to another.
///
/// The value at `source` must be owned and the value at `target` must be uninitialized. Ownership
/// moves from `source` to `target`.
public final class MoveAddrInst: Inst {

  public let range: SourceRange?

  /// The source address.
  public let source: Value

  /// The target address.
  public let target: Value

  init(source: Value, target: Value, range: SourceRange?) {
    precondition(source.type.isAddress, "'source' must have an address type")
    precondition(target.type.isAddress, "'target' must have an address type")

    self.source = source
    self.target = target
    self.range = range
  }

  public var operands: [Value] { [source, target] }

  public static var opstring = "move_addr"

}

/// Stores a value at the specified address, consuming its ownership.
public final class StoreInst: Inst {

  public let range: SourceRange?

  /// The value to store.
  public let value: Value

  /// The target address.
  public let target: Value

  init(value: Value, target: Value, range: SourceRange?) {
    precondition(value.type.isObject, "'value' must have an object type")
    precondition(target.type.isAddress, "'target' must have an address type")

    self.value = value
    self.target = target
    self.range = range
  }

  public var operands: [Value] { [value, target] }

  public static var opstring = "store"

}

// MARK: Aggregate types

/// Creates an uninitialized record value (i.e., the instance of a product type).
///
/// FIXME: Currently, the only use of this intruction is to create `Nil` instances. If there are no
/// other use cases, then it should be removed for something more specific.
public final class RecordInst: Value, Inst {

  public let range: SourceRange?

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  init(typeDecl: NominalTypeDecl, type: VILType, range: SourceRange?) {
    precondition(type.isObject, "'type' must be an object type")

    self.typeDecl = typeDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [] }

  public static var opstring = "record"

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Value, Inst {

  public let range: SourceRange?

  /// The record value whose member is being extracted.
  public let record: Value

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType, range: SourceRange?) {
    precondition(record.type.isObject, "'record' must have an object type")
    precondition(type.isObject, "'type' must be an object type")

    self.record = record
    self.memberDecl = memberDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S: TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = record_member ", to: &stream)
    printer.write("\(memberDecl.debugID) ", to: &stream)
    printer.write("in \(printer.describe(record))\n", to: &stream)
  }

  public static var opstring = "record_member"

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address of the the record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  init(record: Value, memberDecl: VarDecl, type: VILType, range: SourceRange?) {
    precondition(record.type.isAddress, "'record' must have an address type")
    precondition(type.isAddress, "'type' must be an address type")

    self.record = record
    self.memberDecl = memberDecl
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [record] }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S: TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = record_member_addr ", to: &stream)
    printer.write("\(memberDecl.debugID) ", to: &stream)
    printer.write("in \(printer.describe(record))\n", to: &stream)
  }

  public static var opstring = "record_member_addr"

}

/// Creates a tuple value.
public final class TupleInst: Value, Inst {

  /// The values of the tuple's elements.
  public let operands: [Value]

  public let range: SourceRange?

  init(type: TupleType, operands: [Value], range: SourceRange?) {
    let type = type.dealiased
    precondition(type is TupleType, "'type' must be a tuple type")
    precondition(
      operands.allSatisfy({ $0.type.isObject }), "all operands must have an object type")

    self.operands = operands
    self.range = range
    super.init(type: .lower(type))
  }

  public static var opstring = "tuple"

}

// MARK: Casts

/// Converts the type of a value, causing a runtime failure if the conversion fails.
///
/// The operand is consumed.
public final class CheckedCastInst: Value, Inst {

  public let range: SourceRange?

  /// The value to convert.
  public let value: Value

  init(value: Value, type: VILType, range: SourceRange?) {
    precondition(value.type.isObject, "'value' must have an object type")

    self.value = value
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [value] }

  public static var opstring = "checked_cast"

}

/// Converts the type of a value.
///
/// The instruction first performs a checked of the operand. Control is transferred to `succ` if
/// the cast succeeds and the result of the cast is passed as an argument. Otherwise, control is
/// transferred to `fail` and the operand is passed as an argument.
///
/// The operand is consumed.
public final class CheckedCastBranchInst: Inst {

  public let range: SourceRange?

  /// The value to convert.
  public let value: Value

  /// The expected type of the packaged value.
  public let type: VILType

  /// The block to which the execution should jump if the cast succeeds.
  public let succ: BasicBlock.ID

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlock.ID

  init(
    value: Value,
    type: VILType,
    succ: BasicBlock.ID,
    fail: BasicBlock.ID,
    range: SourceRange?
  ) {
    precondition(value.type.isObject, "'value' must have an object type")
    precondition(type.isObject, "'type' must be an object type")

    self.value = value
    self.type = type
    self.succ = succ
    self.fail = fail
    self.range = range
  }

  public var operands: [Value] { [value] }

  public static var opstring = "checked_cast_br"

}

// MARK: Existential types

/// Unconditionally borrows the value packaged inside of an existential container.
///
/// The instruction first performs a checked cast on the packaged value and causess a runtime
/// failure if the cast fails.
///
/// If the borrow is immutable, the container at `source` must be owned, borrowed, or projected. If
/// it is owned, it becomes projected. Otherwise, it's typestate does not change.
///
/// If the borrow is mutable, the container at `source` must be owned and it becomes inouted.
public final class BorrowExistAddrInst: Value, Inst {

  public let range: SourceRange?

  /// The address of the existential container whose value is being borrowed.
  public let container: Value

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  init(isMutable: Bool, container: Value, type: VILType, range: SourceRange?) {
    precondition(container.type.isAddress, "'source' must have an address type")
    precondition(type.isAddress, "'type' must be an address type")

    self.isMutable = isMutable
    self.container = container
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [container] }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S : TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = borrow_exist_addr ", to: &stream)
    if isMutable { printer.write("[mutable] ", to: &stream) }
    printer.write(printer.describe(container), to: &stream)
    printer.write("\n", to: &stream)
  }

  public static var opstring = "borrow_exist_addr"

}

/// Borrows the value packaged inside of an existential container.
///
/// The instruction first performs a checked cast on the packaged value. Control is transferred to
/// `succ` if the cast succeeds and a borrowed address is passed as an argument. Otherwise, control
/// is transferred to `fail` without any argument.
///
/// If the borrow is immutable, the container at `source` must be owned, borrowed, or projected. If
/// it is owned, it becomes projected. Otherwise, it's typestate does not change.
///
/// If the borrow is mutable, the container at `source` must be owned and it becomes inouted.
public final class BorrowExistAddrBranchInst: Inst {

  public let range: SourceRange?

  /// The address of the existential container whose value is being borrowed.
  public let container: Value

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  /// The expected type of the packaged value.
  public let type: VILType

  /// The block to which the execution should jump if the cast succeeds.
  public let succ: BasicBlock.ID

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlock.ID

  init(
    isMutable: Bool,
    container: Value,
    type: VILType,
    succ: BasicBlock.ID,
    fail: BasicBlock.ID,
    range: SourceRange?
  ) {
    precondition(container.type.isAddress, "'container' must have an address type")
    precondition(type.isAddress, "'type' must be an address type")

    self.isMutable = isMutable
    self.container = container
    self.type = type
    self.succ = succ
    self.fail = fail
    self.range = range
  }

  public var operands: [Value] { [container] }

  public static var opstring = "borrow_exist_addr_br"

}

/// Initializes the value packaged inside of an existential container.
public final class InitExistAddrInst: Inst {

  public let range: SourceRange?

  /// The address of the existential container to initialize.
  public let container: Value

  /// The value that initializes the container.
  public let value: Value

  init(container: Value, value: Value, range: SourceRange?) {
    precondition(container.type.isAddress, "'container' must have an address type")
    precondition(value.type.isObject, "'value' must have an object type")

    self.container = container
    self.value = value
    self.range = range
  }

  public var operands: [Value] { [container, value] }

  public static var opstring = "init_exist_addr"

}

/// Creates an existential container initialized with a value borrowed from the specified address.
///
/// The value at `source` is borrowed immutably. It must be owned, borrowed, or projected. If it
/// is owned, it becomes projected. Otherwise, it's typestate does not change.
public final class PackBorrowInst: Value, Inst {

  public let range: SourceRange?

  /// The source address.
  public let source: Value

  init(source: Value, type: VILType, range: SourceRange?) {
    precondition(source.type.isAddress, "'source' must have an address type")
    precondition(type.isAddress, "'type' must be an address type")
    precondition(type.valType.isExistential, "'type' must be an existential type")

    self.source = source
    self.range = range
    super.init(type: type)
  }

  public var operands: [Value] { [source] }

  public static var opstring = "pack_borrow"

}

/// Creates a function reference to the implementation matching a view method in the witness of an
/// existential container.
public final class WitnessMethodInst: Value, Inst {

  public let range: SourceRange?

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Value

  /// The declaration of a view method.
  ///
  /// This property should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Value, decl: BaseFunDecl, range: SourceRange?) {
    precondition(container.type.isObject, "'container' must have an object type")

    self.container = container
    self.decl = decl
    self.range = range
    super.init(type: .lower(decl.unappliedType))
  }

  public var operands: [Value] { [container] }

  public static var opstring = "witness_method"

}

/// Same as `witness_method`, but the container is referred by address.
public final class WitnessMethodAddrInst: Value, Inst {

  public let range: SourceRange?

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Value

  /// The declaration of a view method.
  ///
  /// This property should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Value, decl: BaseFunDecl, range: SourceRange?) {
    precondition(container.type.isAddress, "'container' must have an object type")

    self.container = container
    self.decl = decl
    self.range = range
    super.init(type: .lower(decl.unappliedType))
  }

  public var operands: [Value] { [container] }

  public static var opstring = "witness_method_addr"

}

// MARK: Functions & methods

/// Applies a function.
public final class ApplyInst: Value, Inst {

  public let range: SourceRange?

  /// The function to applied.
  public let callee: Value

  /// The arguments of the function.
  public let args: [Value]

  init(callee: Value, args: [Value], range: SourceRange?) {
    precondition(callee.type.valType is FunType, "'callee' must have a function type")
    precondition(callee.type.isAddress, "'callee' must have an address type")

    self.callee = callee
    self.args = args
    self.range = range
    super.init(type: callee.type.retType!)
  }

  public var operands: [Value] { [callee] + args }

  public func dump<S>(
    to stream: inout S,
    with printer: inout PrinterContext
  ) where S : TextOutputStream {
    printer.write("%\(printer.uniqueID(of: self)) = apply ", to: &stream)
    printer.write(printer.describe(callee, withType: false), to: &stream)
    let args = self.args
      .map({ printer.describe($0) })
      .joined(separator: ", ")
    printer.write("(\(args))\n", to: &stream)
  }

  public static var opstring = "apply"

}

/// Creates the partial application of a function.
public final class PartialApplyInst: Value, Inst {

  public let range: SourceRange?

  /// The function being partially applied.
  public let delegator: Value

  /// The partial list of arguments of the function application (from left to right).
  public let partialArgs: [Value]

  init(delegator: Value, partialArgs: [Value], range: SourceRange?) {
    precondition(delegator.type.isObject, "'delegator' must have an object type")

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

  public static var opstring = "partial_apply"

}

/// Wraps a bare function reference into a thick function container with an empty environment.
///
/// Bare function references are not loadable. This instruction serves to wrap them into a "thick"
/// function container so that they have the same layout as partially applied functions.
public final class ThinToThickInst: Value, Inst {

  public let range: SourceRange?

  /// A bare reference to a VIL function.
  public let ref: FunRef

  init(ref: FunRef, range: SourceRange?) {
    self.ref = ref
    self.range = range
    super.init(type: ref.type.object)
  }

  public var operands: [Value] { [ref] }

  public static var opstring = "thin_to_thick"

}

// MARK: Async expressions

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

  public static var opstring = "async"

}

/// Awaits an asynchronous value.
public final class AwaitInst: Value, Inst {

  public let range: SourceRange?

  /// The value being awaited.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    precondition(value.type.isObject, "'value' must have an object type")
    precondition(value.type.valType is AsyncType, "'value' must have an asynchronous type")

    self.value = value
    self.range = range
    super.init(type: .lower((value.type.valType as! AsyncType).base))
  }

  public var operands: [Value] { [value] }

  public static var opstring = "await"

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

  public static var opstring = "br"

}

/// Branches conditionally to the start of a basic block.
public final class CondBranchInst: Inst {

  public let range: SourceRange?

  /// A Boolean condition.
  public let cond: Value

  /// The block to which the execution should jump if the condition holds.
  public let succ: BasicBlock.ID

  /// The arguments of the "succ" destination block.
  public let succArgs: [Value]

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlock.ID

  /// The arguments of the "fail" destination block.
  public let failArgs: [Value]

  init(
    cond: Value,
    succ: BasicBlock.ID, succArgs: [Value],
    fail: BasicBlock.ID, failArgs: [Value],
    range: SourceRange?
  ) {
    precondition(cond.type.isObject, "'cond' must have an object type")

    self.cond = cond
    self.succ = succ
    self.succArgs = succArgs
    self.fail = fail
    self.failArgs = failArgs
    self.range = range
  }

  public var operands: [Value] { [cond] + succArgs + failArgs }

  public static var opstring = "cond_br"

}

/// Halts the execution of the program.
public final class HaltInst: Inst {

  public let range: SourceRange?

  init(range: SourceRange?) {
    self.range = range
  }

  public var operands: [Value] { [] }

  public static var opstring = "halt"

}

/// Returns from a function.
public final class RetInst: Inst {

  public let range: SourceRange?

  /// The value being returned.
  public let value: Value

  init(value: Value, range: SourceRange?) {
    precondition(value.type.isObject, "'value' must have an object type")

    self.value = value
    self.range = range
  }

  public var operands: [Value] { [value] }

  public static var opstring = "ret"

}

// MARK: Runtime failures

/// Produces a runtime failure if the operand is `true`. Otherwise, does nothing.
public final class CondFail: Inst {

  public let range: SourceRange?

  /// A Boolean condition.
  public let cond: Value

  init(cond: Value, range: SourceRange?) {
    precondition(cond.type.isObject, "'cond' must have an object type")

    self.cond = cond
    self.range = range
  }

  public var operands: [Value] { [cond] }

  public static var opstring = "cond_fail"

}
