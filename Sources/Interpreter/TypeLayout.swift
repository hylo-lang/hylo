import Utils
import FrontEnd


/// The layout of a type in memory, including the positions of its parts.
public struct TypeLayout: Regular {

  /// Memory layout of a type, without any detail about parts.
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
  public struct Part: Regular {
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

  /// The structure.
  ///
  /// For product types, info for each stored property in storage order.
  /// For union types, info for each case when it is active, followed by info for the discriminator.
  /// Empty otherwise (built-in types).
  public let parts: [Part]

  /// The parentage of the parts.
  ///
  /// For product types, the parentage of each stored property in storage order.
  /// For union types, the parentage of each case when it is active, followed by the parentage of the discriminator.
  /// Empty otherwise (built-in types).
  public var partParentages: some Collection<Part.Parentage> {
    parts.indices.lazy.map { .init(self, $0) }
  }

  /// True iff `self` is the layout of a union type, which changes how
  /// its `parts` are interpreted.
  public let isUnionLayout: Bool
}

extension TypeLayout {

  /// The discriminator of a union layout.,
  public var discriminator: Part {
    precondition(isUnionLayout)
    return parts.last!
  }

  /// The id of the discriminator of a union layout.,
  public var discriminatorParentage: Part.Parentage {
    precondition(isUnionLayout)
    return .init(self, parts.count - 1)
  }

  /// The number of parts that will be stored at one time for a given instance.
  public var storedPartCount: Int {
    isUnionLayout ? 2 : parts.count
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

extension TypeLayout.Part {

  public struct Parentage: Regular, CustomStringConvertible {

    /// The type in which the part exists.
    public let parent: TypeLayout

    /// The part index in the parent.
    public let partIndex: Int

    init(_ layout: TypeLayout, _ part: Int) {
      self.parent = layout
      self.partIndex = part
    }

    public var description: String {
      "{part \(partIndex) of \(parent.type)}"
    }
  }

}

extension TypeLayout {

  public func contains(_ t: AnyType, at o: Int, layouts: inout TypeLayoutCache) -> Bool {
    if o == 0 && t == self.type { return true }
    if isUnionLayout {
      return false
    }
    let i = parts.partitioningIndex { $0.offset > o } - 1
    if parts[i].offset == o && parts[i].type == t {
      return true
    }
    return layouts[parts[i].type].contains(t, at: o - parts[i].offset, layouts: &layouts)
  }

}
