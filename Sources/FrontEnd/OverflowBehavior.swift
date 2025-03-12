/// The result of overflow during mathematical operations.
public enum OverflowBehavior {

  /// Overflow is ignored.
  ///
  /// This value is the default, and is thus omitted from `Builtin` function names
  /// (e.g. `Builtin.add_i32`).
  case ignore

  /// The result is a poison value should unsigned overflow occur.
  case nuw

  /// The result is a poison value should signed overflow occur.
  case nsw

}
