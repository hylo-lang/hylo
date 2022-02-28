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

  /// The Val declaration related to the allocation, for debugging.
  public private(set) unowned var decl: ValueDecl?

  init(
    allocType: VILType,
    decl: ValueDecl?,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.allocType = allocType
    self.decl = decl
    self.type = allocType.address
    self.parent = parent
    self.range = range ?? decl?.range
  }

  public var operands: [Operand] { [] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("alloc_stack \(allocType)\n", to: &stream)
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
    printer.write("borrow_addr ", to: &stream)
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

/// Creates a record value (i.e., the instance of a product type) from its concrete parts.
public final class RecordInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let operands: [Operand]

  public let range: SourceRange?

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

  init(
    typeDecl: NominalTypeDecl,
    type: VILType,
    parts: [Operand],
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.typeDecl = typeDecl
    self.type = type
    self.parent = parent
    self.operands = parts
    self.range = range
  }

  /// The concrete (a.k.a. stored) parts of the record's elements.
  public var parts: [Operand] { operands }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    let parts = self.parts
      .map({ printer.describe($0) })
      .joined(separator: ", ")
    printer.write("record \(type) (\(parts))\n", to: &stream)
  }

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
    printer.write(
      "record_member \(memberDecl.debugID) in \(printer.describe(record))\n", to: &stream)
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
    printer.write(
      "record_member_addr \(memberDecl.debugID) in \(printer.describe(record))\n", to: &stream)
  }

  public static var opstring = "record_member_addr"

}

/// Creates a tuple value.
public final class TupleInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let operands: [Operand]

  public let range: SourceRange?

  init(type: TupleType, parts: [Operand], parent: BasicBlockIndex, range: SourceRange?) {
    self.type = .lower(type)
    self.operands = parts
    self.parent = parent
    self.range = range
  }

  /// The parts of the tuple.
  public var parts: [Operand] { operands }

  public static var opstring = "tuple"

}

// MARK: Casts

/// Returns whether the given address can be converted to the specified type.
public final class IsCastableAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The source address.
  public let source: Operand

  /// The type to which the address should be converted.
  public let targetType: VILType

  init(source: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.source = source
    self.targetType = type
    self.type = .lower(type.valType.context.getBuiltinType(named: "i1")!)
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("is_castable_addr \(printer.describe(source)) to \(targetType)\n", to: &stream)
  }

  public static var opstring = "is_castable_addr"

}

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

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("checked_cast \(printer.describe(value)) to \(type)\n", to: &stream)
  }

  public static var opstring = "checked_cast"

}

/// Converts an address to a different type, causing a runtime failure if the conversion fails.
public final class CheckedCastAddrInst: Value, Inst {

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

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("checked_cast_addr \(printer.describe(source)) to \(type)\n", to: &stream)
  }

  public static var opstring = "checked_cast_addr"

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

  /// The type to which `value` should be converted.
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

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("checked_cast_br \(printer.describe(value)) to \(type), ", to: &stream)
    printer.write("bb\(printer.numericID(of: succ)), ", to: &stream)
    printer.write("bb\(printer.numericID(of: fail))\n", to: &stream)
  }

  public static var opstring = "checked_cast_br"

}

/// Converts a pointer to an address.
public final class PointerCastInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The pointer to convert.
  public let pointer: Operand

  init(pointer: Operand, type: VILType, parent: BasicBlockIndex, range: SourceRange?) {
    self.pointer = pointer
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [pointer] }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("pointer_cast \(printer.describe(pointer)) to \(type)\n", to: &stream)
  }

  public static var opstring = "pointer_cast"

}

// MARK: Existential types

/// Given the address `%a` of an owned or lent object `%obj`, produces an address `%b` representing
/// an existential container whose package is a wrapped reference on `%obj`.
///
/// The borrow is immutable; the object at `source` must be owned or lent. If it is owned, it
/// becomes lent. The container's package is guaranteed live until a lifetime-ending use of `%b`
/// (e.g., `end_borrow`).
public final class BorrowExistAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The source address.
  public let source: Operand

  init(
    source: Operand,
    type: VILType,
    parent: BasicBlockIndex,
    range: SourceRange?
  ) {
    self.source = source
    self.type = type
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [source] }

  /// The type of the wrapping container.
  public var interfaceType: VILType { type }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("borrow_exist_addr \(printer.describe(source)) as \(type)\n", to: &stream)
  }

  public static var opstring = "borrow_exist_addr"

}

/// Initializes the contents of an existential container with by consuming object.
public final class InitExistInst: Inst {

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of the existential container to initialize.
  public let container: Operand

  /// The object that initializes the container.
  public let object: Operand

  init(container: Operand, object: Operand, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.object = object
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container, object] }

  public static var opstring = "init_exist"

}

/// Extracts the contents of an existential container.
public final class OpenExistInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// An existential container.
  public let container: Operand

  init(container: Operand, type: WitnessType, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.type = .lower(type)
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container] }

  public static var opstring = "open_exist"

}

/// Obtains the address of the concrete value inside an existential container.
public final class OpenExistAddrInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  public let range: SourceRange?

  /// The address of an existential container.
  public let container: Operand

  init(container: Operand, type: WitnessType, parent: BasicBlockIndex, range: SourceRange?) {
    self.container = container
    self.type = .lower(type).address
    self.parent = parent
    self.range = range
  }

  public var operands: [Operand] { [container] }

  public static var opstring = "open_exist_addr"

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
/// The callee is treated as a borrowed reference on a function. The arguments must correspond to
/// the callee's parameters. Arguments to consuming parameters must be owned and have an object
/// type. Arguments to local and mutating parameters must result from a borrowing instruction. All
/// uses are lifetime-ending.
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
    let args = self.args
      .map({ printer.describe($0) })
      .joined(separator: ", ")
    printer.write("apply ", to: &stream)
    printer.write(printer.describe(callee, withType: false), to: &stream)
    printer.write("(\(args))\n", to: &stream)
  }

  public static var opstring = "apply"

}

/// Creates the partial application of a function.
public final class PartialApplyInst: Value, Inst {

  public let type: VILType

  public let parent: BasicBlockIndex

  /// The function being partially applied.
  public let delegator: FunRef

  /// The partial list of arguments of the function application (from left to right).
  public let args: [Operand]

  /// The ranges corresponding to this instruction.
  private let ranges: [SourceRange?]

  init(
    delegator: FunRef,
    args: [Operand],
    parent: BasicBlockIndex,
    ranges: [SourceRange?]
  ) {
    self.delegator = delegator
    self.args = args
    self.parent = parent
    self.ranges = ranges

    let totalType = delegator.type.valType as! FunType
    let partialType = totalType.context.funType(
      params: totalType.params.dropLast(args.count),
      retType: totalType.retType)
    self.type = .lower(partialType)
  }

  public var operands: [Operand] { [Operand(delegator)] + args }

  public var range: SourceRange? { ranges.last! }

  /// The ranges in Val source corresponding to the locations where arguments are passed.
  public var argsRanges: ArraySlice<SourceRange?> { ranges.dropLast(1) }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    let args = args
      .map({ printer.describe($0) })
      .joined(separator: ", ")
    printer.write("partial_apply \(delegator)(\(args))\n", to: &stream)
  }

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
/// Captures must correspond to `ref`'s parameters. Arguments to consuming captures must be owned
/// and have an object type. Arguments to local and mutating captures must result from a borrowing
/// instruction. All uses are lifetime-ending.
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

  public let parent: BasicBlockIndex

  public let operands: [Operand]

  public let range: SourceRange?

  /// The block to which the execution should jump.
  public let dest: BasicBlockIndex

  init(dest: BasicBlockIndex, args: [Operand], parent: BasicBlockIndex, range: SourceRange?) {
    self.dest = dest
    self.operands = args
    self.parent = parent
    self.range = range
  }

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    printer.write("br bb\(printer.numericID(of: dest))\n", to: &stream)
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

  public func dump<S>(to stream: inout S, with printer: inout PrinterContext<S>) {
    let succArgs = self.succArgs
      .map({ printer.describe($0) })
      .joined(separator: ", ")
    let failArgs = self.succArgs
      .map({ printer.describe($0) })
      .joined(separator: ", ")

    printer.write("cond_br \(printer.describe(cond)), ", to: &stream)
    printer.write("bb\(printer.numericID(of: succ))(\(succArgs)), ", to: &stream)
    printer.write("bb\(printer.numericID(of: fail))(\(failArgs))\n", to: &stream)
  }

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

  public static var opstring = "return"

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
