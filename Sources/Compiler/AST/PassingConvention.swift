/// A parameter passing convention.
public enum PassingConvention: Codable {

  /// Pass by immutable projection.
  case `let`

  /// Pass by assignable projection.
  case `set`

  /// Pass by mutable projection.
  case `inout`

  /// Pass by consumption.
  case sink

  /// Yielded.
  case yielded

  init(matching capability: RemoteType.Capability) {
    switch capability {
    case .let     : self = .let
    case .set     : self = .set
    case .inout   : self = .inout
    case .yielded : self = .yielded
    }
  }

}
