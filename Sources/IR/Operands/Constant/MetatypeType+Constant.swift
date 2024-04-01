import FrontEnd

extension MetatypeType: Constant {

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(self) }

}
