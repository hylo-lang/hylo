import Core

extension TraitType: Constant {

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(self) }

}
