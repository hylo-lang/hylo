/// The predicate of an integer comparison.
///
/// - Note: Ordered means that neither operand is a QNAN while unordered means that either operand
///   may be a QNAN.
public enum FloatingPointPredicate: String, Hashable {

  /// No comparison; always false.
  case alwaysFalse = "false"

  /// No comparison; always true.
  case alwaysTrue = "true"

  /// Values are ordered and equal.
  case oeq

  /// Values are ordered and not equal.
  case one

  /// Values are ordered and LHS is greater than RHS.
  case ogt

  /// Values are ordered and LHS greater than or equal to RHS.
  case oge

  /// Values are ordered and LHS is less than RHS.
  case olt

  /// Values are ordered and LHS is less than or equal to RHS.
  case ole

  /// Values are ordered (no nans).
  case ord

  /// Values are unordered or equal.
  case ueq

  /// Values are unordered or not equal.
  case une

  /// Values are unordered or LHS is greater than RHS.
  case ugt

  /// Values are unordered or LHS is greater than or equal to RHS.
  case uge

  /// Values are unordered or LHS is less than RHS.
  case ult

  /// Values are unordered or LHS is less than or equal to RHS.
  case ule

  /// Values are unordered (either nans).
  case uno

}

extension FloatingPointPredicate: LosslessStringConvertible {

  public init?(_ description: String) {
    self.init(rawValue: description)
  }

  public var description: String { self.rawValue }

}
