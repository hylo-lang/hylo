import Utils
import FrontEnd


/// The layout of a type in memory, including the positions of its components.
public struct TypeLayout: Regular {

  /// Memory layout of a type, without any detail about components.
  public struct Bytes: Regular {
    /// The minimum alignment of an instance.  Always a power of 2.
    let alignment: Int

    /// The number of bytes occupied by an instance.
    let size: Int

    /// The number of bytes between the beginnings of consecutive array elements.
    var stride: Int {
      size.rounded(upToNearestMultipleOf: alignment)
    }
  }

  /// A (potential, in the case of union types) part of `type` and
  /// where it is stored in a `type` instance.
  public struct Component: Regular {
    /// The name if any (i.e. tuple label or stored property name).
    public let name: String?

    /// The type of the part.
    public let type: AnyType

    /// The byte offset of the part with respect to the layout.
    public let offset: Int
  }

  /// Aggregate layout values of this layout.
  public let bytes: Bytes

  /// The minimum alignment of an instance.  Always a power of 2.
  public var alignment: Int { bytes.alignment }

  /// The number of bytes occupied by an instance.
  public var size: Int { bytes.size }

  /// The number of bytes between the beginnings of consecutive array elements.
  public var stride: Int { bytes.stride }

  /// The type whose layout is described by `self`.
  public let type: AnyType

  /// The sub-structure of `self`.
  ///
  /// For product types, info for each stored property in storage order.
  /// For union types, info for each case when it is active, followed by info for the discriminator.
  /// Empty otherwise (built-in types).
  public let components: [Component]

  public var componentIDs: some Collection<Component.ID> {
    components.indices.lazy.map { .init(self, $0) }
  }

  /// True iff `self` is the layout of a union type, which changes how
  /// its `components` are interpreted.
  public let isUnionLayout: Bool
}

extension TypeLayout {

  public var discriminator: Component {
    precondition(isUnionLayout)
    return components.last!
  }

  public var discriminatorID: Component.ID {
    precondition(isUnionLayout)
    return .init(self, components.count - 1)
  }

  public var storedComponents: Int {
    isUnionLayout ? 2 : components.count
  }
}

extension TypeLayout.Bytes {

  /// Returns the layout of the tuple `(S, T)`, where `S` and `T` are types whose layout is
  /// represented by `self` and `t` respectively.
  ///
  /// - Note: the `T` instance is stored `t.size` bytes before the end of the tuple.
  public func appending(_ t: Self) -> Self {
    let r = self.size.rounded(upToNearestMultipleOf: t.alignment)
    return .init(alignment: max(self.alignment, t.alignment), size: r + t.size)
  }

}

extension TypeLayout.Component {

  public struct ID: Regular, CustomStringConvertible {

    public let layout: TypeLayout
    public let component: Int

    init(_ layout: TypeLayout, _ component: Int) {
      self.layout = layout
      self.component = component
    }

    public var description: String {
      "{part \(component) of \(layout.type)}"
    }
  }

}
