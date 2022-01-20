import OrderedCollections
import Basic

public typealias CaptureTable = OrderedDictionary<CaptureKey, CaptureDecl>

/// The key of an entry in a capture table.
public struct CaptureKey: Hashable {

  /// The declaration being captured.
  public let decl: ValueDecl

  public init(_ decl: ValueDecl) {
    self.decl = decl
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public static func == (lhs: CaptureKey, rhs: CaptureKey) -> Bool {
    return lhs.decl === rhs.decl
  }

}
