import LLVM

extension LLVM.Module {

  /// Returns the LLVM type of a machine word.
  mutating func word() -> LLVM.IntegerType {
    IntegerType(layout.bitWidth(of: ptr), in: &self)
  }

}
