import Core
import FrontEnd
import Utils

/// The value of a register in an abstract interpreter.
enum AbstractValue<Domain: AbstractDomain>: Equatable {

  /// A non-empty set of locations.
  case locations(Set<AbstractLocation>)

  /// An object.
  case object(AbstractObject<Domain>)

  /// Creates a `.object` value with an object of given `value` and `type`, using `program` to
  /// compute its layout.
  init(
    object value: AbstractObject<Domain>.Value,
    ofType type: AnyType,
    definedIn program: TypedProgram
  ) {
    self = .object(.init(layout: AbstractTypeLayout(of: type, definedIn: program), value: value))
  }

  /// If `self` is `.locations(l)`, returns `l`; otherwise, returns `nil`.
  func unwrapLocations() -> Set<AbstractLocation>? {
    if case .locations(let ls) = self {
      return ls
    } else {
      return nil
    }
  }

  /// If `self`is `.object(o)`, returns `o`; otherwise, returns `nil`.
  func unwrapObject() -> AbstractObject<Domain>? {
    if case .object(let o) = self {
      return o
    } else {
      return nil
    }
  }

  /// Returns `l` merged with `r`.
  static func && (l: AbstractValue, r: AbstractValue) -> AbstractValue {
    switch (l, r) {
    case (.locations(let a), .locations(let b)):
      return .locations(a.union(b))
    case (.object(let a), .object(let b)):
      return .object(a && b)
    default:
      unreachable()
    }
  }

}

extension AbstractValue: CustomStringConvertible {

  var description: String {
    switch self {
    case .locations(let v):
      return String(describing: v)
    case .object(let v):
      return String(describing: v)
    }
  }

}
