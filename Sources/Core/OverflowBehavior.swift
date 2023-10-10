/// Copied from SwiftLLVM, used to remove LLVM dependency from Core

/// The behavior that should occur on overflow during mathematical operations.
public enum OverflowBehavior {

  /// Overflow is ignored.
  case ignore

  /// The result is a poison value should unsigned overflow occur.
  case nuw

  /// The result is a poison value should signed overflow occur.
  case nsw

}
