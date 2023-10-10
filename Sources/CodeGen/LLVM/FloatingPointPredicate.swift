import LLVM
import Core

extension LLVM.FloatingPointPredicate {
  public init(_ p: Core.FloatingPointPredicate) {
    self = LLVM.FloatingPointPredicate(rawValue: p.rawValue)!
  }
}
