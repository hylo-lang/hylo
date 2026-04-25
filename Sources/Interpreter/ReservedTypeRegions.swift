import FrontEnd
import Utils

/// The non-overlapping regions of an `Allocation` reserved to be interpreted as specific type.
public struct ReservedTypeRegions {

  /// A position in some allocation.
  public typealias Offset = Memory.Storage.Index

  /// The type layout, every type follows.
  private let typeLayouts: UnsafeMutablePointer<TypeLayoutCache>

  /// The reserved typed regions, in ascending order.
  private var reservedTypeRegions: [Memory.Allocation.TypedRegion]

  /// Empty regions ensuring type layouts from `l`.
  public init(
    havingLayoutsFrom l: UnsafeMutablePointer<TypeLayoutCache>
  ) {
    typeLayouts = l
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

  /// Constrains accesses to the region starting at `o` only according to
  /// structure of `t`, disallowing incompatible representations.
  public mutating func reserve(_ t: AnyType, at o: Offset) throws {
    let i = reservedTypeRegions.partitioningIndex { $0.startOffset > o }
    if i != 0
      && reservedTypeRegions[i - 1].startOffset
        + typeLayouts.pointee[reservedTypeRegions[i - 1].type].size > o
    {
      throw Memory.Error.regionAlreadyReserved(for: reservedTypeRegions[i - 1].type)
    }
    if i != reservedTypeRegions.endIndex
      && o + typeLayouts.pointee[t].size > reservedTypeRegions[i].startOffset
    {
      throw Memory.Error.regionAlreadyReserved(for: reservedTypeRegions[i].type)
    }
    reservedTypeRegions.insert(.init(startOffset: o, type: t), at: i)
  }

  /// Removes the type-based access constraints for the region starting at `o`.
  public mutating func removeTypeReservation(from o: Offset) {
    let i = reservedTypeRegions.partitioningIndex { $0.startOffset >= o }
    let j = reservedTypeRegions.partitioningIndex { $0.startOffset > o }
    reservedTypeRegions.removeSubrange(i..<j)
  }
}
