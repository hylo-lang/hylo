/// The kind of borrow applied to a `Place`.
public enum BorrowKind {

  /// A shared, read-only borrow. Multiple `let` borrow may coexist, provided
  /// no conflicting `inout` borrow exists.
  case `let`

  /// An exclusive, read-write borrow. No other borrow (`let` or `inout`)
  /// may overlap unless it is same borrow (reentrant).
  case `inout`

}
