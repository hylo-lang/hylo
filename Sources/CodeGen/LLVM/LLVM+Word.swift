import SwiftyLLVM

extension SwiftyLLVM.Module {

  /// Returns the LLVM type of a machine word.
  mutating func word() -> SwiftyLLVM.IntegerType {
    IntegerType(layout.bitWidth(of: ptr), in: &self)
  }

}
