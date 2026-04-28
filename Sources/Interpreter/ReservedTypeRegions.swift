import FrontEnd
import Utils

/// The non-overlapping regions of an `Allocation` reserved to be interpreted as specific type.
public struct ReservedTypeRegions {

  public enum Error: Swift.Error, Regular {
    case regionAlreadyBound(to: AnyType)
  }

  /// A position in some allocation.
  public typealias Offset = Memory.Storage.Index

  /// The type layout, every type follows.
  private let typeLayouts: UnsafeMutablePointer<TypeLayoutCache>

  /// The allocation for which type bindings belong to.
  private let allocation: UnsafePointer<Memory.Allocation>

  /// The reserved typed regions, in ascending order.
  private var reservedTypeRegions: [Memory.Allocation.TypedRegion]

  /// Empty regions ensuring type layouts from `l`.
  public init(
    allocation a: UnsafePointer<Memory.Allocation>,
    typeLayouts l: UnsafeMutablePointer<TypeLayoutCache>
  ) {
    typeLayouts = l
    allocation = a
    reservedTypeRegions = []
  }

  /// Returns the index of typed region enclosing `a`.
  private func regionIndex(enclosing a: Offset) -> Int? {
    let i = reservedTypeRegions.partitioningIndex { $0.startOffset > a }
    if i == 0 {
      return nil
    }
    if reservedTypeRegions[i - 1].startOffset
      + typeLayouts.pointee[reservedTypeRegions[i - 1].type].size <= a
    {
      return nil
    }
    return i - 1
  }

  /// Returns the typed region enclosing `a`.
  public func region(enclosing a: Offset) -> Memory.Allocation.TypedRegion? {
    guard let i = regionIndex(enclosing: a) else {
      return nil
    }
    return reservedTypeRegions[i]
  }

  /// Binds the region starting at `o` to `t`, constraining accesses to that
  /// region to the layout of `t`.
  public mutating func bind(_ t: AnyType, at o: Offset) throws {
    try allocation.pointee.checkAlignmentAndAllocationBounds(
      at: o, for: typeLayouts.pointee[t])

    let i = reservedTypeRegions.partitioningIndex { $0.startOffset > o }
    if i != 0
      && reservedTypeRegions[i - 1].startOffset
        + typeLayouts.pointee[reservedTypeRegions[i - 1].type].size > o
    {
      throw Error.regionAlreadyBound(to: reservedTypeRegions[i - 1].type)
    }
    if i != reservedTypeRegions.endIndex
      && o + typeLayouts.pointee[t].size > reservedTypeRegions[i].startOffset
    {
      throw Error.regionAlreadyBound(to: reservedTypeRegions[i].type)
    }
    reservedTypeRegions.insert(.init(startOffset: o, type: t), at: i)
  }

  /// Removes the type binding for the region starting at `o`, removing the
  /// layout constraints previously applied to that region.
  public mutating func unbind(at o: Offset) {
    let i = reservedTypeRegions.partitioningIndex { $0.startOffset >= o }
    let j = reservedTypeRegions.partitioningIndex { $0.startOffset > o }
    reservedTypeRegions.removeSubrange(i..<j)
  }
}
