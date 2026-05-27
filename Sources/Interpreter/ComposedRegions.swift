import FrontEnd
import Utils

/// A region of some allocation that contains a complete instance of
/// some type, ready to be a part of a new composition.
public struct ComposedRegion: Regular {

  /// A position in some allocation.
  public typealias Offset = Memory.Storage.Index

  /// Where the region begins relative to an `Allocation`'s `baseOffset`.
  let offset: Offset

  /// The type in the region.
  let type: AnyType

}

/// The composed regions of an `Allocation`.
struct ComposedRegions {

  /// A position in some allocation.
  public typealias Offset = ComposedRegion.Offset

  /// Non-overlapping set of composed regions, in ascending order based on `offset`.
  private var composedRegions: [ComposedRegion] = []

  /// Returns the region enclosing `a`.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  public func region(
    enclosing a: Offset, typeLayouts l: inout TypeLayoutCache
  ) -> ComposedRegion? {
    indexOfRegion(enclosing: a, typeLayouts: &l).map { composedRegions[$0] }
  }

  /// Returns true iff initialization records starting at `o` for the parts
  /// of a `t` can be replaced with initialization record of `t` instance.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  /// - Precondition: `self` stores composed regions of `a`.
  public func canCompose(
    _ t: AnyType, at o: Offset, in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    let t = l[t]
    let i = composedRegions.partitioningIndex { $0.offset >= o }

    if t.isUnionLayout {
      let dc = t.discriminator
      if isComposed(
        part: t.discriminatorParentage, baseOffset: o,
        region: dc.offset == 0 ? i : i + 1) == false
      {
        return false
      }

      let dv = a.unsignedIntValue(
        at: dc.offset + o, ofType: t.discriminator.type.base as! BuiltinType)
      if isComposed(
        part: .init(t, Int(dv)), baseOffset: o,
        region: dc.offset == 0 ? i + 1 : i) == false
      {
        return false
      }

      return true
    } else {
      return t.parts.indices.allSatisfy {
        isComposed(part: .init(t, $0), baseOffset: o, region: i + $0)
      }
    }
  }

  /// Replaces the initialization records starting at `a` for the
  /// parts of a `t` instance with the initialization record for a
  /// `t` instance.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  /// - Precondition: `canCompose(t, at: a, &l)`.
  public mutating func compose(
    _ t: AnyType, at a: Offset, typeLayouts l: inout TypeLayoutCache
  ) {
    let t = l[t]
    let i = composedRegions.partitioningIndex { $0.offset >= a }
    composedRegions.replaceSubrange(
      i..<(i + t.storedPartCount),
      with: CollectionOfOne(.init(offset: a, type: t.type)))
  }

  /// If there exists an initialization record for a `t` instance at `o` in `a`,
  /// replaces it with parts of that instance and returns true;
  /// Otherwise, returns false.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  /// - Precondition: `self` stores composed regions of `a`.
  public mutating func tryDecompose(
    _ t: AnyType, at o: Offset, in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    let t = l[t]
    guard let i = decomposable(t, at: o) else {
      return false
    }
    decompose(t, inRegion: i, in: a)
    return true
  }

  /// Returns true iff the given `part` of some type at `baseOffset` is
  /// represented as the `n`th composed region.
  private func isComposed(
    part: TypeLayout.Part.Parentage, baseOffset: Offset, region n: Int
  ) -> Bool {
    let p = part.parent.parts[part.partIndex]
    let partOffset = baseOffset + p.offset
    guard let r = composedRegions.dropFirst(n).first,
      r.offset == partOffset
    else {
      return false
    }
    if r.type != p.type {
      return false
    }
    return true
  }

  /// Returns the region index of the top-level composed instance of `t` at `a`, if any, or `nil`
  /// otherwise.
  private func decomposable(_ t: TypeLayout, at a: Offset) -> Int? {
    let i = composedRegions.partitioningIndex { $0.offset >= a }
    guard let r0 = composedRegions[i...].first,
      r0.offset == a,
      r0.type == t.type
    else {
      return nil
    }
    assert(
      composedRegions[i...].dropFirst().first.map {
        $0.offset >= r0.offset + t.size
      } ?? true
    )
    return i
  }

  /// Replaces the initialization record for a `t` in `i`th region with
  /// the initialization records for any parts of that instance.
  ///
  /// - Precondition: `self` stores composed regions of `a`.
  private mutating func decompose(
    _ t: TypeLayout, inRegion i: Int, in a: Memory.Allocation
  ) {
    let o = composedRegions[i].offset

    if t.isUnionLayout {
      let expectedDiscriminator = t.parts.last!
      let discriminator = ComposedRegion.init(
        offset: expectedDiscriminator.offset + o, type: expectedDiscriminator.type)
      let d = a.unsignedIntValue(
        at: discriminator.offset, ofType: discriminator.type.base as! BuiltinType)
      let expectedPayload = t.parts[Int(d)]
      let payload = ComposedRegion(offset: expectedPayload.offset + o, type: expectedPayload.type)
      let newRecords =
        expectedDiscriminator.offset == 0 ? [discriminator, payload] : [payload, discriminator]
      composedRegions.replaceSubrange(i..<i + 1, with: newRecords)
    } else {
      composedRegions.replaceSubrange(
        i..<i + 1, with: t.parts.lazy.map { .init(offset: o + $0.offset, type: $0.type) })
    }
  }

  /// Returns end offset of `i`th composed region.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  private func endOffset(
    _ i: Int, typeLayouts l: inout TypeLayoutCache
  ) -> Offset {
    composedRegions[i].offset + l[composedRegions[i].type].size
  }

  /// Returns index of region enclosing `a`.
  ///
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  private func indexOfRegion(
    enclosing a: Offset, typeLayouts l: inout TypeLayoutCache
  ) -> Int? {
    let i = composedRegions.partitioningIndex { $0.offset > a }
    if i == 0 { return nil }
    let o = composedRegions[i - 1].offset
    let n = l[composedRegions[i - 1].type].size
    if o + n > a {
      return i - 1
    }
    return nil
  }

}

extension ComposedRegions {

  /// Returns true iff object at `p` is complete.
  ///
  /// - Precondition: `p` doesn't partially overlap with any composed region in `self`.
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  public func isComplete(
    _ p: Memory.Allocation.TypedRegion, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    guard let r = region(enclosing: p.offset, typeLayouts: &l) else {
      return false
    }
    return r.offset <= p.offset
      && (r.offset + l[r.type].size) >= (p.offset + l[p.type].size)
  }

  /// Returns true iff object at `p` is fully uninitialized.
  ///
  /// - Precondition: `p` doesn't partially overlap with any composed region in `self`.
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  public func isFullyUninitialized(
    _ p: Memory.Allocation.TypedRegion, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    let i = composedRegions.partitioningIndex { $0.offset >= p.offset }
    if i != 0 && endOffset(i - 1, typeLayouts: &l) > p.offset {
      return false
    }
    if i != composedRegions.endIndex
      && (p.offset + l[p.type].size) > composedRegions[i].offset
    {
      return false
    }
    return true
  }

  /// Removes all composed regions contained within `p`.
  ///
  /// - Precondition: Every composed region in `self` is either
  ///   disjoint from `p` or fully contained within `p`.
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  public mutating func removeRegions(
    in p: Memory.Allocation.TypedRegion, typeLayouts l: inout TypeLayoutCache
  ) {
    let start = p.offset
    let end = start + l[p.type].size

    let i = composedRegions.partitioningIndex { $0.offset >= start }
    let j = composedRegions.partitioningIndex { $0.offset >= end }

    composedRegions.removeSubrange(i..<j)
  }

  /// Composes from the end of `regions` while composition remains possible.
  ///
  /// - Precondition: Any element in `regions` doesn't partially overlap with any
  ///   composed region in `self`.
  /// - Precondition: `self` stores composed regions of `a`.
  /// - Precondition: The composed regions in `self` are consistent
  ///   with the type layouts provided by `l`.
  public mutating func composeUpwards(
    along regions: some BidirectionalCollection<Memory.Allocation.TypedRegion>,
    in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) {
    for p in regions.reversed() {
      let r = region(enclosing: p.offset, typeLayouts: &l)!
      if r.offset < p.offset || (r.offset == p.offset && r.type == p.type) {
        continue
      }
      if canCompose(p.type, at: p.offset, in: a, typeLayouts: &l) {
        compose(p.type, at: p.offset, typeLayouts: &l)
      } else {
        break
      }
    }
  }

}
