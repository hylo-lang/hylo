import FrontEnd
import Foundation
import Utils

/// A unique access participating in access control.
public struct Access: Regular {

  /// A unique `Access` identifier.
  public typealias ID = UUID

  /// A unique `Access` identifier.
  public let id: ID

  /// Effect of access.
  public let effect: AccessEffect

  /// Creates an instance of access of effect `e`.
  public init(effect e: AccessEffect) {
    id = UUID()
    effect = e
  }

}
