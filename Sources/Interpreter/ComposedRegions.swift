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
public struct ComposedRegions {

  /// A position in some allocation.
  public typealias Offset = ComposedRegion.Offset

  /// The type layout, every type follows.
  private let typeLayouts: UnsafeMutablePointer<TypeLayoutCache>

  /// The allocation for which composed region belongs to.
  private let allocation: UnsafePointer<Memory.Allocation>

  /// The composed regions, in ascending order.
  private var composedRegions: [ComposedRegion]

  /// Empty regions for `a` ensuring type layouts from `l`.
  public init(
    allocation a: UnsafePointer<Memory.Allocation>,
    typeLayouts l: UnsafeMutablePointer<TypeLayoutCache>
  ) {
    typeLayouts = l
    allocation = a
    composedRegions = []
  }

  /// Returns the region enclosing `a`.
  public func region(enclosing a: Offset) -> ComposedRegion? {
    indexOfRegion(enclosing: a).map { composedRegions[$0] }
  }

  /// Returns true iff initialization records starting at `a` for the parts
  /// of a `t` can be replaced with initialization record of `t` instance.
  public func canCompose(_ t: AnyType, at a: Offset) -> Bool {
    let t = typeLayouts.pointee[t]
    let i = composedRegions.partitioningIndex { $0.offset >= a }

    if t.isUnionLayout {
      let dc = t.discriminator
      if isComposed(
        part: t.discriminatorParentage, baseOffset: a,
        region: dc.offset == 0 ? i : i + 1) == false
      {
        return false
      }

      let dv = allocation.pointee.unsignedIntValue(
        at: dc.offset + a, ofType: t.discriminator.type.base as! BuiltinType)

      if isComposed(
        part: .init(t, Int(dv)), baseOffset: a,
        region: dc.offset == 0 ? i + 1 : i) == false
      {
        return false
      }

      return true
    } else {
      return t.parts.indices.allSatisfy {
        isComposed(part: .init(t, $0), baseOffset: a, region: i + $0)
      }
    }
  }

  /// Replaces the initialization records starting at `a` for the
  /// parts of a `t` instance with the initialization record for a
  /// `t` instance.
  ///
  /// - Precondition: `canCompose(t, at: a)`.
  public mutating func compose(_ t: AnyType, at a: Offset) {
    let t = typeLayouts.pointee[t]
    let i = composedRegions.partitioningIndex { $0.offset >= a }
    composedRegions.replaceSubrange(
      i..<(i + t.storedPartCount),
      with: CollectionOfOne(.init(offset: a, type: t.type)))
  }

  /// If there exists an initialization record for a `t` instance at `a`, replaces
  /// it with parts of that instance and returns true. Otherwise, returns false.
  public mutating func tryDecompose(_ t: AnyType, at a: Offset) -> Bool {
    let t = typeLayouts.pointee[t]
    guard let i = decomposable(t, at: a) else {
      return false
    }
    decompose(t, inRegion: i)
    return true
  }

  /// Returns true iff the given `part` of some type at `baseOffset` is
  /// represented as the `n`th composed region.
  private func isComposed(
    part: TypeLayout.Part.Parentage,
    baseOffset: Offset,
    region n: Int
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

  /// Replaces the initialization record for a `t` instance at `a` with
  /// the initialization records for any parts of that instance.
  private mutating func decompose(_ t: TypeLayout, inRegion i: Int) {
    let a = composedRegions[i].offset

    if t.isUnionLayout {
      let expectedDiscriminator = t.parts.last!
      let discriminator = ComposedRegion.init(
        offset: expectedDiscriminator.offset + a, type: expectedDiscriminator.type)
      let d = allocation.pointee.unsignedIntValue(
        at: discriminator.offset, ofType: discriminator.type.base as! BuiltinType)
      let expectedPayload = t.parts[Int(d)]
      let payload = ComposedRegion(offset: expectedPayload.offset + a, type: expectedPayload.type)
      let newRecords =
        expectedDiscriminator.offset == 0 ? [discriminator, payload] : [payload, discriminator]
      composedRegions.replaceSubrange(i..<i + 1, with: newRecords)
    } else {
      composedRegions.replaceSubrange(
        i..<i + 1, with: t.parts.lazy.map { .init(offset: a + $0.offset, type: $0.type) })
    }
  }

  /// Returns end offset of `i`th composed region.
  private func endOffset(_ i: Int) -> Offset {
    composedRegions[i].offset + typeLayouts.pointee[composedRegions[i].type].size
  }

  /// Returns index of region enclosing `a`.
  private func indexOfRegion(enclosing a: Offset) -> Int? {
    let i = composedRegions.partitioningIndex { $0.offset > a }
    if i == 0 { return nil }
    let o = composedRegions[i - 1].offset
    let n = typeLayouts.pointee[composedRegions[i - 1].type].size
    if o + n > a {
      return i - 1
    }
    return nil
  }

}

extension ComposedRegions {

  /// Returns true iff object at `p` is complete.
  ///
  /// - Precondition: For every composed region `r`, one of the following should hold:
  ///     - `r` and `p` are disjoint, or
  ///     - `r` fully contains `p`, or
  ///     - `r` is fully contained within `p`.
  public func isComplete(_ p: Memory.Place) -> Bool {
    guard let r = region(enclosing: p.offset) else {
      return false
    }
    return r.offset <= p.offset
      && (r.offset + typeLayouts.pointee[r.type].size)
        >= (p.offset + typeLayouts.pointee[p.type].size)
  }

  /// Returns true iff object at `p` is fully uninitialized.
  ///
  /// - Precondition: For every composed region `r`, one of the following should hold:
  ///     - `r` and `p` are disjoint, or
  ///     - `r` fully contains `p`, or
  ///     - `r` is fully contained within `p`.
  public func isFullyUninitialized(_ p: Memory.Place) -> Bool {
    let i = composedRegions.partitioningIndex { $0.offset >= p.offset }
    if i != 0 && endOffset(i - 1) > p.offset {
      return false
    }
    if i != composedRegions.endIndex
      && (p.offset + typeLayouts.pointee[p.type].size) > composedRegions[i].offset
    {
      return false
    }
    return true
  }

  /// Removes all composed regions contained in `p`.
  ///
  /// - Precondition: For every composed region `r`, one of the following should hold:
  ///     - `r` and `p` are disjoint, or
  ///     - `r` is fully contained within `p`.
  public mutating func decomposeSubtree(of p: Memory.Place) {
    let start = p.offset
    let end = start + typeLayouts.pointee[p.type].size

    let i = composedRegions.partitioningIndex { $0.offset < start }
    let j = composedRegions.partitioningIndex { $0.offset < end }

    composedRegions.removeSubrange(i..<j)
  }

  /// Attempts to compose regions along `path` by moving upward toward the root.
  ///
  /// Starting from the end of `path`, this operation repeatedly merges each
  /// region into its enclosing context while composition remains possible.
  ///
  /// - Precondition: For every element `q` in `path` and every composed region `r`,
  ///     one of the following holds:
  ///     - `r` and `q` are disjoint, or
  ///     - `r` fully contains `q`, or
  ///     - `r` is fully contained within `q`.
  public mutating func composeUpwards(
    along path: some BidirectionalCollection<Memory.Allocation.TypedRegion>
  ) {
    for p in path.reversed() {
      let r = region(enclosing: p.startOffset)!
      if r.offset < p.startOffset || (r.offset == p.startOffset && r.type == p.type) {
        continue
      }
      if canCompose(p.type, at: p.startOffset) {
        compose(p.type, at: p.startOffset)
      } else {
        break
      }
    }
  }

}
