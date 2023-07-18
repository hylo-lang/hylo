import Core

extension TraitType: Constant {

  /// The Val IR type of this instance.
  public var type: IR.LoweredType { .object(self) }

}
