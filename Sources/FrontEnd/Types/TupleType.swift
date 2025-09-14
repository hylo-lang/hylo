import Utils

/// A tuple type.
public struct TupleType: TypeProtocol {

  /// A part of a tuple type.
  public struct Component: Hashable {

    /// Creates an instance having the given properties.
    public init(label: String?, type: AnyType) {
      self.label = label
      self.type = type
    }

    /// The label of the element.
    public let label: String?

    /// The type of the element.
    public let type: AnyType

  }

  /// The components of the tuple.
  public let components: [Component]

  public let flags: ValueFlags

  /// Creates a tuple type having the given components.
  public init<S: Sequence>(_ components: S) where S.Element == Component {
    self.components = Array(components)
    self.flags = ValueFlags(self.components.map(\.type.flags))
  }

  /// Creates a tuple type having the given components.
  public init<S: Sequence>(_ components: S) where S.Element == (String?, AnyType) {
    self.init(components.map({ Component(label: $0.0, type: $0.1) }))
  }

  /// Creates a tuple having the given unlabeled components.
  public init<S: Sequence>(_ components: S) where S.Element == AnyType {
    self.init(components.map({ Component(label: nil, type: $0) }))
  }

  /// The labels of the tuple.
  public var labels: LazyMapSequence<[Component], String?> {
    components.lazy.map(\.label)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    let newElements = components.map { (e) -> Component in
      .init(label: e.label, type: e.type.transform(mutating: &m, transformer))
    }
    return TupleType(newElements)
  }

}

extension TupleType: CustomStringConvertible {

  public var description: String {
    if (components.count == 1) && (components[0].label == nil) {
      return "{\(components[0]),}"
    } else {
      return "{\(list: components)}"
    }
  }

}

extension TupleType.Component: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
