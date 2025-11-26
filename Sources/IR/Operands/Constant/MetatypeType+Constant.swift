import FrontEnd
import Utils

extension MetatypeType: Constant {

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(self) }

}

extension MetatypeType: ColoredDescribable {

  public var coloredDescription: String {
    styledType(String(describing: self))
  }

}
