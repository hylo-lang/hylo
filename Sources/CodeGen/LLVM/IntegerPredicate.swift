import Core
import LLVM

extension LLVM.IntegerPredicate {
  public init(_ p: Core.IntegerPredicate) {
    self = LLVM.IntegerPredicate(rawValue: p.rawValue)!
  }
}
