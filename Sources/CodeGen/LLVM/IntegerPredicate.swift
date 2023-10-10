import LLVM
import Core

extension LLVM.IntegerPredicate {
  public init(_ p: Core.IntegerPredicate) {
    self = LLVM.IntegerPredicate(rawValue: p.rawValue)!
  }
}
