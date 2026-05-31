import FrontEnd
import Utils

/// The non-overlapping regions of an `Allocation` reserved to be interpreted as specific type.
struct ReservedTypeRegions {

  public enum Error: Swift.Error, Regular {
    case regionAlreadyBound(to: AnyType)
  }

  /// A position in some allocation.
  public typealias Offset = Memory.Storage.Index

  /// The non-overlapping type bindings that determine how reserved regions
  /// are to be interpreted, sorted by region offset.
  private var typeBindings: [Memory.Allocation.TypedRegion] = []

  /// Returns the index of region enclosing `a`.
  ///
  /// - Precondition: The allocation to which these regions belong is
  ///   consistent with the type layouts in `l`.
  private func regionIndex(
    enclosing a: Offset,
    typeLayouts l: inout TypeLayoutCache
  ) -> Int? {
    let i = typeBindings.partitioningIndex { $0.offset > a }
    if i == 0 {
      return nil
    }
    if typeBindings[i - 1].offset + l[typeBindings[i - 1].type].size <= a {
      return nil
    }
    return i - 1
  }

  /// Returns the region enclosing `a`.
  ///
  /// - Precondition: The allocation to which these regions belong is
  ///   consistent with the type layouts in `l`.
  public func region(enclosing a: Offset, typeLayouts l: inout TypeLayoutCache)
    -> Memory.Allocation.TypedRegion?
  {
    guard let i = regionIndex(enclosing: a, typeLayouts: &l) else {
      return nil
    }
    return typeBindings[i]
  }

  /// Binds the region starting at `o` in `a` to `t`, constraining accesses to that
  /// region to the layout of `t`.
  ///
  /// - Precondition: The allocation to which these regions belong is
  ///   consistent with the type layouts in `l`.
  /// - Precondition: All previous `bind` calls to `self` were made with same
  ///   allocation as of `a.
  public mutating func bind(
    _ t: AnyType, at o: Offset, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    try a.checkAlignmentAndAllocationBounds(at: o, for: l[t])

    let i = typeBindings.partitioningIndex { $0.offset > o }
    if i != 0
      && typeBindings[i - 1].offset + l[typeBindings[i - 1].type].size > o
    {
      throw Error.regionAlreadyBound(to: typeBindings[i - 1].type)
    }
    if i != typeBindings.endIndex && o + l[t].size > typeBindings[i].offset
    {
      throw Error.regionAlreadyBound(to: typeBindings[i].type)
    }
    typeBindings.insert(.init(offset: o, type: t), at: i)
  }

  /// Removes the binding for region starting at `o`.
  public mutating func unbind(at o: Offset) {
    let i = typeBindings.partitioningIndex { $0.offset >= o }
    let j = typeBindings.partitioningIndex { $0.offset > o }
    typeBindings.removeSubrange(i..<j)
  }
}
