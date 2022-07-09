/// Allocates enough storage to store an object of type `objectType`.
public struct AllocInst: Inst {

  /// The type of the object for which storage is allocated.
  public let objectType: Type

  /// The space where memory should be allocated.
  public let space: MemorySpace

  public var type: LoweredType { .address(objectType) }

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    output.write("alloc [\(space)] \(objectType)")
  }

}
