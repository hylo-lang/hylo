import AST

/// A VIL instruction.
public protocol Inst {}

/// A stack allocation.
public struct AllocStackInst: Inst, Value {

  /// The type of the value produced by the allocation.
  public let type: VILType

  /// The type of the allocated object.
  public let allocatedType: ValType

}

/// The creation of an uninitialized record value (i.e., an instance of a product type).
public struct RecordInst: Inst, Value {

  /// The declaration of the type of which the record value is an instance.
  public let typeDecl: NominalTypeDecl

}

/// The address of a stored member in a record value.
public struct RecordMemberAddrInst: Inst, Value {

  /// The record value for which the member's address is computed.
  public let record: Value

  /// The declaration of the member whose address is computed.
  public let memberDecl: VarDecl

}

/// A store instruction.
public struct StoreInst: Inst {

  /// The location (or target) of the store.
  public let lvalue: Value

  /// The value being stored.
  public let rvalue: Value

}
