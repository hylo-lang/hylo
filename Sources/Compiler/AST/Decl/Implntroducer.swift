/// The introducer of a method or subscript implementation.
public enum ImplIntroducer: Codable {

  case `let`

  case `inout`

  case set

  case sink

  /// The parameter passing convention corresponding to this introducer.
  public var convention: PassingConvention {
    switch self {
    case .let: return .let
    case .inout: return .inout
    case .set: return .set
    case .sink: return .sink
    }
  }

}
