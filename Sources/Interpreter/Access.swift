import FrontEnd
import Foundation
import Utils

extension AccessEffect: Regular {}

/// A unique access participating in access control.
public struct Access: Regular {

  /// A unique `Access` identifier.
  public typealias ID = UUID

  /// A unique `Access` identifier.
  public let id: ID

  /// Kind of access.
  public let kind: AccessEffect

  /// An access of kind `k`.
  public init(kind k: AccessEffect) {
    id = UUID()
    kind = k
  }

}
