/// Whether Swifty-LLVM crashes when running mandatory passes: (https://github.com/hylo-lang/Swifty-LLVM/issues/24).
#if os(Windows)
  public let swiftyLLVMMandatoryPassesCrash = false
#else
  public let swiftyLLVMMandatoryPassesCrash = true
#endif
