import Foundation
import Utils

/// The kind of access to a `Place`.
public enum AccessKind: UInt8, Regular {

  /// Value is accessed immutably.
  case `let` = 1

  /// Value is assigned but never read.
  case `set` = 2

  /// Value is accessed mutably.
  case `inout` = 4

  /// Value is consumed.
  case sink = 8

}

/// A unique access participating in access control.
public struct Access: Regular {

  /// A unique `Borrow` identifier.
  public typealias ID = UUID

  /// A unique `Borrow` identifier.
  public let id: ID

  /// Kind of borrow.
  public let kind: AccessKind

  /// A `k` type borrow.
  public init(_ k: AccessKind) {
    id = UUID()
    kind = k
  }

}
