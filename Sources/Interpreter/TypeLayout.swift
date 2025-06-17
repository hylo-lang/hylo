import Utils
import FrontEnd


/// The layout of a type in memory, including the positions of its components.
struct TypeLayout {

  /// Memory layout of a type, without any detail about components.
  struct Bytes {
    /// The minimum alignment of an instance.  Always a power of 2.
    let alignment: Int

    /// The number of bytes occupied by an instance.
    let size: Int

    /// The number of bytes between the beginnings of consecutive array elements.
    var stride: Int {
      size.rounded(upToNearestMultipleOf: alignment)
    }
  }

  /// Types stored in `self` with their names (if any), and byte offsets from the base of this
  /// type's layout.
  typealias Component = (name: String?, type: AnyType, offset: Int)

  /// Aggregate layout values of this layout.
  let bytes: Bytes

  /// The minimum alignment of an instance.  Always a power of 2.
  var alignment: Int { bytes.alignment }

  /// The number of bytes occupied by an instance.
  var size: Int { bytes.size }

  /// The number of bytes between the beginnings of consecutive array elements.
  var stride: Int { bytes.stride }

  /// The type whose layout is described by `self`.
  let type: AnyType

  /// The sub-structure of `self`.
  ///
  /// For product types, info for each stored property.
  /// For union types, info for each case when it is active, followed by info for the discriminator.
  /// Empty otherwise (built-in types).
  let components: [Component]

  /// True iff `self` is the layout of a union type, which changes how
  /// its `components` are interpreted.
  let isUnionLayout: Bool
}

extension TypeLayout.Bytes {

  /// Returns the layout of the tuple `(S, T)`, where `S` and `T` are types whose layout is
  /// represented by `self` and `t` respectively.
  ///
  /// - Note: the `T` instance is stored `t.size` bytes before the end of the tuple.
  func appending(_ t: Self) -> Self {
    let r = self.size.rounded(upToNearestMultipleOf: t.alignment)
    return .init(alignment: max(self.alignment, t.alignment), size: r + t.size)
  }

}
