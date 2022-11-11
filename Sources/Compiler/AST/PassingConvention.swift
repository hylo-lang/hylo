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

  /// Creates a convention matching the given remote type capability.
  init(matching capability: RemoteType.Capability) {
    switch capability {
    case .let     : self = .let
    case .set     : self = .set
    case .inout   : self = .inout
    case .yielded : self = .yielded
    }
  }

  /// Creates a convnetion matching the given receiver effect.
  init(matching effect: ReceiverEffect) {
    switch effect {
    case .inout   : self = .inout
    case .sink    : self = .sink
    }
  }

}
