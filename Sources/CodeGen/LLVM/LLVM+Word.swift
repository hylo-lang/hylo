import SwiftyLLVM

extension SwiftyLLVM.Module {

  /// Returns the LLVM type of a machine word.
  var word: SwiftyLLVM.IntegerType.UnsafeReference {
    mutating get {
      integerType(layout.bitWidth(of: ptr))
    }
  }

}
