import Foundation
import Utils

/// The kind of borrow applied to a `Place`.
public enum BorrowKind: Regular {

  /// A shared, read-only borrow. Multiple `let` borrow may coexist, provided
  /// no conflicting `inout` borrow exists.
  case `let`

  /// An exclusive, read-write borrow. No other borrow (`let` or `inout`)
  /// may overlap unless it refers to same borrow (reentrant).
  case `inout`

}

/// A unique borrow participating in access control.
public struct Borrow: Regular {

  /// A unique `Borrow` identifier.
  public typealias ID = UUID

  /// A unique `Borrow` identifier.
  public let id: ID

  /// Kind of borrow.
  public let kind: BorrowKind

  /// A `k` type borrow.
  public init(_ k: BorrowKind) {
    id = UUID()
    kind = k
  }

}
