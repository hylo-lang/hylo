import FrontEnd
import Foundation
import Utils

/// An access to a `Region` occurring during execution.
public struct Access<Region: Regular>: Regular {

  /// A unique `Access` identifier.
  public typealias ID = UUID

  /// A unique `Access` identifier.
  public let id: ID

  /// The associated permissions and obligations.
  public let effect: AccessEffect

  /// The location to which access applies.
  public let location: Region

  /// Creates an instance of access to `r` having effect `e`.
  public init(to r: Region, effect e: AccessEffect) {
    id = UUID()
    location = r
    effect = e
  }

  /// Creates an instance of access to `r` having effect `e` and the given `id`.
  public init(to r: Region, effect e: AccessEffect, id: ID) {
    self.id = id
    location = r
    effect = e
  }

}
