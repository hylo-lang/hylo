import Core
import LLVM

extension LLVM.FloatingPointPredicate {
  public init(_ p: Core.FloatingPointPredicate) {
    self = LLVM.FloatingPointPredicate(rawValue: p.rawValue)!
  }
}
