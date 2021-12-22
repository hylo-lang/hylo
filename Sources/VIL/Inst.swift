import AST
import Basic

/// A VIL instruction.
public protocol Inst: AnyObject {

  /// The basic block that contains the instruction.
  var parent: BasicBlockIndex { get }

  /// The instruction's operands.
  var operands: [Operand] { get }

  /// The range in Val source corresponding to the instruction, if any.
  var range: SourceRange? { get }

  /// Dumps the instruction to the given stream.
  func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>)

  /// The opstring of the instruction in textual VIL.
  static var opstring: String { get }

}

extension Inst {

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
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

// MARK: Memory instructions

/// Allocates uninitalized memory on the stack.
///
/// The instruction returns the address of a cell large enough to contain an instance of
/// `allocatedType`. That cell must be deallocated by `dealloc_stack`.
public final class AllocStackInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The type of the allocated value.
  public let allocType: VILType

  /// A flag indicating whether the allocated value represents a constructor's receiver.
  public let isReceiver: Bool

  /// The Val declaration related to the allocation, for debugging.
  public private(set) unowned var decl: ValueDecl?

  init(
    allocType: VILType,
    isReceiver: Bool,
    decl: ValueDecl?,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.allocType = allocType
    self.isReceiver = isReceiver
    self.decl = decl
    self.type = allocType.address
    self.parent = parent
    self.range = range ?? decl?.range
  }

  public var operands: [Operand] { [] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    if isReceiver { printer.write("[receiver] ", to: &stream) }
    printer.write("\(allocType)\n", to: &stream)
  }

  public static var opstring = "alloc_stack"

}

/// Given the address `%a` of an owned or lent object `%obj`, produces an address `%b` representing
/// a "borrowed" reference on `%obj`.
///
/// If the borrow is immutable, the object at `source` must be owned or lent. If it is owned, it
/// becomes lent. Otherwise, it's ownership does not change. If the borrow is mutable, the object
/// at `source` must be owned and it becomes projected. The reference is guaranteed live until a
/// lifetime-ending use of `%b` (e.g., `end_borrow`).
public final class BorrowAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  /// The source address.
  public let source: Operand

  init(
    isMutable: Bool,
    source: Operand,
    type: VILType,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.isMutable = isMutable
    self.source = source
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    if isMutable { printer.write("[mutable] ", to: &stream) }
    printer.write("\(printer.describe(source))\n", to: &stream)
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

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The corresponding stack allocation.
  public let alloc: Operand

  init(alloc: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.alloc = alloc
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [alloc] }

  public static var opstring = "dealloc_stack"

}

/// Destroys the specified value, calling its destructor.
public final class DeleteInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value to delete.
  public let value: Operand

  init(value: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.value = value
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [value] }

  public static var opstring = "delete"

}

/// Destroys the contents located at the specified address, calling its destructor and leaving the
/// memory uninitialized.
///
/// The specified address must be initialized. If the referenced memory is bound to an existential
/// type, it must hold an initialized container.
public final class DeleteAddrInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the object to delete.
  public let target: Operand

  init(target: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.target = target
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [target] }

  public static var opstring = "delete_addr"

}

/// Ends the lifetime of a borrow.
///
/// This instruction must be the last user of its operand.
public final class EndBorrowInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The borrowed address to release.
  public let source: Operand

  init(source: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.source = source
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  public static var opstring = "end_borrow"

}

/// Loads the value stored at the specified address.
///
/// The instruction operates on the source address directly. Hence, if it is assigned to an
/// existential container, the entire container is loaded, not only the packaged value.
public final class LoadInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The source address.
  public let source: Operand

  init(source: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.source = source
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  public static var opstring = "load"

}

/// Moves the contents from one location to another.
///
/// The value at `source` must be owned and the value at `target` must be uninitialized. Ownership
/// moves from `source` to `target`.
public final class MoveAddrInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The source address.
  public let source: Operand

  /// The target address.
  public let target: Operand

  init(source: Operand, target: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.source = source
    self.target = target
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source, target] }

  public static var opstring = "move_addr"

}

/// Stores a value at the specified address, consuming its ownership.
public final class StoreInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value to store.
  public let value: Operand

  /// The target address.
  public let target: Operand

  init(value: Operand, target: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.value = value
    self.target = target
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [value, target] }

  public static var opstring = "store"

}

// MARK: Aggregate types

/// Creates an uninitialized record value (i.e., the instance of a product type).
///
/// FIXME: Currently, the only use of this intruction is to create `Nil` instances. If there are no
/// other use cases, then it should be removed for something more specific.
public final class RecordInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  init(typeDecl: NominalTypeDecl, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.typeDecl = typeDecl
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [] }

  public static var opstring = "record"

}

/// Extracts the value of a stored member from a record.
public final class RecordMemberInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The record value whose member is being extracted.
  public let record: Operand

  /// The declaration of the member being extracted.
  public let memberDecl: VarDecl

  init(
    record: Operand,
    memberDecl: VarDecl,
    type: VILType,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.record = record
    self.memberDecl = memberDecl
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [record] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    printer.write("\(memberDecl.debugID) ", to: &stream)
    printer.write("in \(printer.describe(record))\n", to: &stream)
  }

  public static var opstring = "record_member"

}

/// Computes the address of a stored member from the address of a record.
public final class RecordMemberAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the the record value for which the member's address is computed.
  public let record: Operand

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

  init(
    record: Operand,
    memberDecl: VarDecl,
    type: VILType,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.record = record
    self.memberDecl = memberDecl
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [record] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    printer.write("\(memberDecl.debugID) ", to: &stream)
    printer.write("in \(printer.describe(record))\n", to: &stream)
  }

  public static var opstring = "record_member_addr"

}

/// Creates a tuple value.
public final class TupleInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  /// The values of the tuple's elements.
  public let operands: [Operand]

  public let range: SourceRange?

  init(type: TupleType, operands: [Operand], parent: BasicBlockIndex, range: SourceRange?) {
    self.type = .lower(type)
    self.operands = operands
    self.parent = parent
    self.range = range
  }

  public static var opstring = "tuple"

}

// MARK: Casts

/// Converts the type of a value, causing a runtime failure if the conversion fails.
///
/// The operand is consumed.
public final class CheckedCastInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value to convert.
  public let value: Operand

  init(value: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.value = value
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [value] }

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

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value to convert.
  public let value: Operand

  /// The expected type of the packaged value.
  public let type: VILType

  /// The block to which the execution should jump if the cast succeeds.
  public let succ: BasicBlockIndex

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlockIndex

  init(
    value: Operand,
    type: VILType,
    succ: BasicBlockIndex,
    fail: BasicBlockIndex,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.value = value
    self.type = type
    self.succ = succ
    self.fail = fail
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [value] }

  public static var opstring = "checked_cast_br"

}

// MARK: Existential types

/// Given the address `%a` of an owned or lent object existential container `%ec`, produces an
/// address `%b` representing a "borrowed" reference on the object packaged inside of `%ec`,
/// assuming it has the specified type.
///
/// The instruction first performs a checked cast on the packaged value and causess a runtime
/// failure if the cast fails.
///
/// If the borrow is immutable, the container at `source` must be owned or lent. If it is owned,
/// it becomes lent. Otherwise, it's ownership does not change. If the borrow is mutable, the
/// container at `source` must be owned and it becomes projected. The reference is guaranteed live
/// until a lifetime-ending use of `%b` (e.g., `end_borrow`).
public final class BorrowExistAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the existential container whose value is being borrowed.
  public let container: Operand

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  init(
    isMutable: Bool,
    container: Operand,
    type: VILType,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.isMutable = isMutable
    self.container = container
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    if isMutable { printer.write("[mutable] ", to: &stream) }
    printer.write("\(printer.describe(container)) as \(type)\n", to: &stream)
  }

  public static var opstring = "borrow_exist_addr"

}

/// Borrows the value packaged inside of an existential container.
///
/// The instruction first performs a checked cast on the packaged value. Control is transferred to
/// `succ` if the cast succeeds and a borrowed address is passed as an argument. Otherwise, control
/// is transferred to `fail` without any argument.
///
/// If the borrow is immutable, the container at `source` must be owned or lent. If it is owned,
/// it becomes lent. Otherwise, it's ownership does not change. If the borrow is mutable, the
/// container at `source` must be owned and it becomes projected.
public final class BorrowExistAddrBranchInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the existential container whose value is being borrowed.
  public let container: Operand

  /// A Boolean value that indicates whether the borrow is mutable.
  public let isMutable: Bool

  /// The expected type of the packaged value.
  public let type: VILType

  /// The block to which the execution should jump if the cast succeeds.
  public let succ: BasicBlockIndex

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlockIndex

  init(
    isMutable: Bool,
    container: Operand,
    type: VILType,
    succ: BasicBlockIndex,
    fail: BasicBlockIndex,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.isMutable = isMutable
    self.container = container
    self.type = type
    self.succ = succ
    self.fail = fail
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
    if isMutable { printer.write("[mutable] ", to: &stream) }
    printer.write("\(printer.describe(container)) as \(type),", to: &stream)
    printer.write("\(printer.numericID(of: succ)),", to: &stream)
    printer.write("\(printer.numericID(of: fail))\n", to: &stream)
  }

  public static var opstring = "borrow_exist_addr_br"

}

/// Initializes the value packaged inside of an existential container.
public final class InitExistAddrInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the existential container to initialize.
  public let container: Operand

  /// The value that initializes the container.
  public let value: Operand

  init(container: Operand, value: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.value = value
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container, value] }

  public static var opstring = "init_exist_addr"

}

/// Creates an existential container initialized with a value borrowed from the specified address.
///
/// The value at `source` is borrowed immutably. It must be owned or lent. If it is owned, it
/// becomes lent. Otherwise, it's ownership does not change.
public final class PackBorrowInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The source address.
  public let source: Operand

  init(source: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.source = source
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  public static var opstring = "pack_borrow"

}

/// Creates a function reference to the implementation matching a view method in the witness of an
/// existential container.
public final class WitnessMethodInst: Value, Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Operand

  /// The declaration of a view method.
  ///
  /// This property should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Operand, decl: BaseFunDecl, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.decl = decl
    self.parent = parent
    self.range = range
  }

  public var type: VILType { .lower(decl.unappliedType) }

  public var operands: [Operand] { [container] }

  public static var opstring = "witness_method"

}

/// Same as `witness_method`, but the container is referred by address.
public final class WitnessMethodAddrInst: Value, Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// An existential container that conforms to the view for which the method is being looked up.
  public let container: Operand

  /// The declaration of a view method.
  ///
  /// This property should be either a regular method or a constructor declaration.
  public let decl: BaseFunDecl

  init(container: Operand, decl: BaseFunDecl, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.decl = decl
    self.parent = parent
    self.range = range
  }

  public var type: VILType { .lower(decl.unappliedType) }

  public var operands: [Operand] { [container] }

  public static var opstring = "witness_method_addr"

}

// MARK: Functions & methods

/// Applies a function.
///
/// The callee must be a function and have an address type. The list of arguments must correspond
/// to the callee's parameters. Arguments to consuming parameters must be owned  have an object
/// type. Arguments to local and mutating parameters must result from a borrowing instruction.
///
/// Uses of consuming arguments are lifetime-ending; uses of local and mutating arguments are not.
///
/// The result of `apply` is an owned value.
public final class ApplyInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  /// The function to applied.
  public let callee: Operand

  /// The arguments of the function.
  public let args: [Operand]

  /// The ranges corresponding to this instruction.
  private let ranges: [SourceRange?]

  init(
    callee: Operand,
    args: [Operand],
    type: VILType,
    parent: BasicBlockIndex,
    ranges: [SourceRange?]
  ) {
    self.callee = callee
    self.args = args
    self.type = type
    self.parent = parent
    self.ranges = ranges
  }

  public var operands: [Operand] { [callee] + args }

  public var range: SourceRange? { ranges.last! }

  /// The ranges in Val source corresponding to the locations where arguments are passed.
  public var argsRanges: ArraySlice<SourceRange?> { ranges.dropLast(1) }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write(Self.opstring + " ", to: &stream)
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

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The function being partially applied.
  public let delegator: Operand

  /// The partial list of arguments of the function application (from left to right).
  public let partialArgs: [Operand]

  init(
    delegator: Operand,
    delegatorType: FunType,
    partialArgs: [Operand],
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.delegator = delegator
    self.partialArgs = partialArgs
    self.parent = parent
    self.range = range

    let partialType = delegatorType.context.funType(
      params: delegatorType.params.dropLast(partialArgs.count),
      retType: delegatorType.retType)
    self.type = .lower(partialType)
  }

  public var operands: [Operand] { [delegator] + partialArgs }

  public static var opstring = "partial_apply"

}

/// Wraps a bare function reference into a thick function container with an empty environment.
///
/// Bare function references are not loadable. This instruction serves to wrap them into a "thick"
/// function container so that they have the same layout as partially applied functions.
public final class ThinToThickInst: Value, Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// A bare reference to a VIL function.
  public let ref: FunRef

  init(ref: FunRef, parent: BasicBlockIndex, range: SourceRange?) {
    self.ref = ref
    self.parent = parent
    self.range = range
  }

  public var type: VILType { ref.type.object }

  public var operands: [Operand] { [Operand(ref)] }

  public static var opstring = "thin_to_thick"

}

// MARK: Async expressions

/// Creates an asynchronous value.
///
/// The list of captures must correspond to `ref`'s parameters. Arguments to consuming captures
/// must be owned and have an object type. Arguments to local and mutating captures must result
/// from a borrowing instruction.
///
/// The instruction is a lifetime-ending use of all its operands.
public final class AsyncInst: Value, Inst {

  public let parent: BasicBlockIndex

  /// A bare reference to the function that represents the asynchronous execution.
  public let ref: FunRef

  /// The values captured by the asynchronous expression, representing the arguments passed to the
  /// underlying function.
  public let captures: [Operand]

  /// The ranges corresponding to this instruction.
  private let ranges: [SourceRange?]

  init(ref: FunRef, captures: [Operand], parent: BasicBlockIndex, ranges: [SourceRange?]) {
    self.ref = ref
    self.captures = captures
    self.parent = parent
    self.ranges = ranges
  }

  public var type: VILType {
    let valType = ref.type.valType as! FunType
    let context = valType.context
    return .lower(context.asyncType(of: valType.retType))
  }

  public var operands: [Operand] { [Operand(ref)] + captures }

  public var range: SourceRange? { ranges.last! }

  /// The ranges in Val source corresponding to the locations where captures are formed.
  public var captureRanges: ArraySlice<SourceRange?> { ranges.dropLast(1) }

  public static var opstring = "async"

}

/// Awaits an asynchronous value.
public final class AwaitInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value being awaited.
  public let value: Operand

  init(value: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.value = value
    self.type = type
    self.parent = parent
    self.range = range
  }

  // public var type: VILType { .lower((value.type.valType as! AsyncType).base) }

  public var operands: [Operand] { [value] }

  public static var opstring = "await"

}

// MARK: Terminators

/// Branches unconditionally to the start of a basic block.
public final class BranchInst: Inst {

  public let operands: [Operand]

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The block to which the execution should jump.
  public let dest: BasicBlockIndex

  init(dest: BasicBlockIndex, args: [Operand], parent: BasicBlockIndex, range: SourceRange?) {
    self.dest = dest
    self.operands = args
    self.parent = parent
    self.range = range
  }

  public static var opstring = "br"

}

/// Branches conditionally to the start of a basic block.
public final class CondBranchInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// A Boolean condition.
  public let cond: Operand

  /// The block to which the execution should jump if the condition holds.
  public let succ: BasicBlockIndex

  /// The arguments of the "succ" destination block.
  public let succArgs: [Operand]

  /// The block to which the execution should jump if the condition does not hold.
  public let fail: BasicBlockIndex

  /// The arguments of the "fail" destination block.
  public let failArgs: [Operand]

  init(
    cond: Operand,
    succ: BasicBlockIndex, succArgs: [Operand],
    fail: BasicBlockIndex, failArgs: [Operand],
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.cond = cond
    self.succ = succ
    self.succArgs = succArgs
    self.fail = fail
    self.failArgs = failArgs
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [cond] + succArgs + failArgs }

  public static var opstring = "cond_br"

}

/// Halts the execution of the program.
public final class HaltInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  init(parent: BasicBlockIndex, range: SourceRange?) {
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [] }

  public static var opstring = "halt"

}

/// Returns from a function.
public final class RetInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The value being returned.
  public let value: Operand

  init(value: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.value = value
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [value] }

  public static var opstring = "ret"

}

// MARK: Runtime failures

/// Produces a runtime failure if the operand is `true`. Otherwise, does nothing.
public final class CondFail: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// A Boolean condition.
  public let cond: Operand

  init(cond: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.cond = cond
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [cond] }

  public static var opstring = "cond_fail"

}
