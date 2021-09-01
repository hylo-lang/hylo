import VIL

/// The address of an instruction.
public struct InstAddr: Equatable {

  /// The VIL name of the function.
  let funName: VILName

  /// The ID of the block in the function pointed by `functionPtr`.
  var blockID: BasicBlock.ID

  /// An instruction offset in the block identified by `blockID`.
  var offset: Int

  private init() {
    self.funName = VILName("")
    self.blockID = 0
    self.offset = 0
  }

  public init(fun: VILFun, blockID: BasicBlock.ID, offset: Int) {
    self.funName = fun.name
    self.blockID = blockID
    self.offset = offset
  }

  /// The null address.
  static var null = InstAddr()

}
