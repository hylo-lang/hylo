/// A protocol describing the API of a Hylo term.
public protocol TermProtocol: Hashable {

  /// Properties about the representation of `self`.
  var flags: ValueFlags { get }

}

extension TermProtocol {

  /// Creates an instance with the value of `container.base` or returns `nil` if that value has
  /// a different type.
  public init?(_ container: AnyTerm) {
    if let t = container.base as? Self {
      self = t
    } else {
      return nil
    }
  }

  /// Creates an instance with the value of `container.base` or returns `nil` if either that value
  /// has a different type or `container` is `nil`.
  public init?(_ container: AnyTerm?) {
    if let t = container.flatMap(Self.init(_:)) {
      self = t
    } else {
      return nil
    }
  }

  /// Returns whether the specified flags are raised on this term.
  public subscript(fs: ValueFlags) -> Bool { flags.contains(fs) }

}
