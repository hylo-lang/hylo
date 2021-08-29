import VIL

/// The address of an instruction.
public struct InstAddr: Equatable {

  /// The address of a function, stored as a pointer to an unmanaged reference to sidestep Swift's
  /// reference counting.
  let functionPtr: UnsafeMutableRawPointer?

  /// The ID of the block in the function pointed by `functionPtr`.
  var blockID: BasicBlock.ID

  /// An instruction offset in the block identified by `blockID`.
  var offset: Int

  private init() {
    self.functionPtr = nil
    self.blockID = 0
    self.offset = 0
  }

  public init(function: Function, blockID: BasicBlock.ID, offset: Int) {
    self.functionPtr = Unmanaged.passUnretained(function).toOpaque()
    self.blockID = blockID
    self.offset = offset
  }

  /// The null address.
  static var null = InstAddr()

}
