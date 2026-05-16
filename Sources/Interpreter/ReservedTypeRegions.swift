import FrontEnd
import Utils

/// The non-overlapping regions of an `Allocation` reserved to be interpreted as specific type.
struct ReservedTypeRegions {

  public enum Error: Swift.Error, Regular {
    case regionAlreadyBound(to: AnyType)
  }

  /// A position in some allocation.
  public typealias Offset = Memory.Storage.Index

  /// The reserved typed regions, in ascending order.
  private var reservedTypeRegions: [Memory.Allocation.TypedRegion] = []

  /// Returns the index of typed region enclosing `a`, assuming every allocation
  /// follows type layouts from `l`.
  private func regionIndex(
    enclosing a: Offset,
    typeLayouts l: inout TypeLayoutCache
  ) -> Int? {
    let i = reservedTypeRegions.partitioningIndex { $0.offset > a }
    if i == 0 {
      return nil
    }
    if reservedTypeRegions[i - 1].offset + l[reservedTypeRegions[i - 1].type].size <= a {
      return nil
    }
    return i - 1
  }

  /// Returns the typed region enclosing `a`, assuming every allocations follow
  /// type layouts from `l`.
  public func region(enclosing a: Offset, typeLayouts l: inout TypeLayoutCache)
    -> Memory.Allocation.TypedRegion?
  {
    guard let i = regionIndex(enclosing: a, typeLayouts: &l) else {
      return nil
    }
    return reservedTypeRegions[i]
  }

  /// Binds the region starting at `o` in `a` to `t`, constraining accesses to that
  /// region to the layout of `t`, assuming every allocations follow type
  /// layouts from `l`.
  ///
  /// - Precondition: All the previous bind calls were made with allocation same as `a`.
  public mutating func bind(
    _ t: AnyType, at o: Offset, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    try a.checkAlignmentAndAllocationBounds(at: o, for: l[t])

    let i = reservedTypeRegions.partitioningIndex { $0.offset > o }
    if i != 0
      && reservedTypeRegions[i - 1].offset + l[reservedTypeRegions[i - 1].type].size > o
    {
      throw Error.regionAlreadyBound(to: reservedTypeRegions[i - 1].type)
    }
    if i != reservedTypeRegions.endIndex && o + l[t].size > reservedTypeRegions[i].offset
    {
      throw Error.regionAlreadyBound(to: reservedTypeRegions[i].type)
    }
    reservedTypeRegions.insert(.init(offset: o, type: t), at: i)
  }

  /// Removes the type binding for the region starting at `o`, removing the
  /// layout constraints previously applied to that region.
  public mutating func unbind(at o: Offset) {
    let i = reservedTypeRegions.partitioningIndex { $0.offset >= o }
    let j = reservedTypeRegions.partitioningIndex { $0.offset > o }
    reservedTypeRegions.removeSubrange(i..<j)
  }
}
