import Utils

/// A tuple type.
public struct TupleType: TypeProtocol {

  /// An element in a tuple type.
  public struct Element: Hashable {

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

  /// The elements of the tuple.
  public let elements: [Element]

  public let flags: ValueFlags

  /// Creates a tuple type with a sequence of elements.
  public init<S: Sequence>(_ elements: S) where S.Element == Element {
    self.elements = Array(elements)
    self.flags = ValueFlags(self.elements.map(\.type.flags))
  }

  /// Creates a tuple type with a sequence of label-type pairs.
  public init<S: Sequence>(labelsAndTypes: S) where S.Element == (String?, AnyType) {
    self.init(labelsAndTypes.map({ Element(label: $0.0, type: $0.1) }))
  }

  /// Creates a tuple of unlabeled elements with the types in the given sequence.
  public init<S: Sequence>(types: S) where S.Element == AnyType {
    self.init(types.map({ Element(label: nil, type: $0) }))
  }

  /// The labels of the tuple.
  public var labels: LazyMapSequence<[Element], String?> {
    elements.lazy.map(\.label)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    let newElements = elements.map { (e) -> Element in
      .init(label: e.label, type: e.type.transform(mutating: &m, transformer))
    }
    return TupleType(newElements)
  }

}

extension TupleType: CustomStringConvertible {

  public var description: String {
    if (elements.count == 1) && (elements[0].label == nil) {
      return "{\(elements[0]),}"
    } else {
      return "{\(list: elements)}"
    }
  }

}

extension TupleType.Element: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
