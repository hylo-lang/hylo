import Core

extension MetatypeType: Constant {

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(self) }

}
