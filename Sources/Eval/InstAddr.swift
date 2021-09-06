import VIL

/// The address of an instruction.
enum InstAddr: Equatable {

  /// The null address.
  case null

  /// An non-null address.
  case some(fun: Int, block: BasicBlock.ID, inst: BasicBlock.Index)

}
