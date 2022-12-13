/// An access effect, specifying how a parameter, receiver, or remote part is accessed
public enum AccessEffect: Codable {

  /// Value is accessed immutably.
  case `let`

  /// Value is assigned but never read.
  case `set`

  /// Value is accessed mutably.
  case `inout`

  /// Value is consumed.
  case sink

  /// Value may be accessed with any of the other effects, depending on the context.
  case yielded

  /// Creates the passing convention that corresponds to `capability`.
  init(matching capability: RemoteType.Capability) {
    switch capability {
    case .let     : self = .let
    case .set     : self = .set
    case .inout   : self = .inout
    case .yielded : self = .yielded
    }
  }

  /// Creates the passing convention that corresponds to `effect`.
  init(matching effect: ReceiverEffect) {
    switch effect {
    case .inout   : self = .inout
    case .sink    : self = .sink
    }
  }

}
