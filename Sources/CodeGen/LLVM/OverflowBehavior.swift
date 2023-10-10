import LLVM
import Core

extension LLVM.OverflowBehavior {
  public init(_ ob: Core.OverflowBehavior) {
    switch ob {
      case .ignore: self = OverflowBehavior.ignore
      case .nuw: self = OverflowBehavior.nuw
      case .nsw: self = OverflowBehavior.nsw
    }
  }
}
