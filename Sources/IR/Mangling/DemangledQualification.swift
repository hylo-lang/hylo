/// The demangled qualification of a symbol.
public indirect enum DemangledQualification: Hashable {

  /// An entity.
  case entity(DemangledEntity)

  /// A reference to the innermost enclosing entity.
  case relative

  /// Creates an instance wrapping `e` iff it is not `nil`.
  init?(_ e: DemangledEntity?) {
    if let s = e {
      self = .entity(s)
    } else {
      return nil
    }
  }

  /// The entity wrapped in `self` if its payload is `.entity`, or `nil` otherwise.
  public var entity: DemangledEntity? {
    if case .entity(let e) = self {
      return e
    } else {
      return nil
    }
  }

}

extension DemangledQualification: CustomStringConvertible {

  public var description: String {
    switch self {
    case .entity(let d):
      return d.description
    case .relative:
      return ".."
    }
  }

}
