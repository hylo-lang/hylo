import Core
import LLVM

extension LLVM.OverflowBehavior {
  public init(_ ob: Core.OverflowBehavior) {
    self = switch ob {
      case .ignore: .ignore
      case .nuw: .nuw
      case .nsw: .nsw
    }
  }
}
