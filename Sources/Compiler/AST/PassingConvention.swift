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

  /// Creates the passing convention that corresponds to `capability`.
  init(matching capability: RemoteType.Capability) {
    switch capability {
    case .let: self = .let
    case .set: self = .set
    case .inout: self = .inout
    case .yielded: self = .yielded
    }
  }

  /// Creates the passing convention that corresponds to `effect`.
  init(matching effect: ReceiverEffect) {
    switch effect {
    case .inout: self = .inout
    case .sink: self = .sink
    }
  }

}
