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

  /// Returns the region enclosing the offset
  public func region(enclosing a: Offset) -> ComposedRegion? {
    let i = composedRegions.partitioningIndex { $0.offset > a }
    if i == 0 { return nil }
    let o = composedRegions[i - 1].offset
    let n = typeLayouts.pointee[composedRegions[i - 1].type].size
    if o + n > a {
      return composedRegions[i - 1]
    }
    return nil
  }

  /// Returns true iff the given `part` of some type at `baseOffset` is
  /// represented as the `n`th composed region.
  public func isComposed(
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
  public mutating func composeUnchecked(_ t: AnyType, at a: Offset) throws {
    precondition(canCompose(t, at: a))
    let t = typeLayouts.pointee[t]
    let i = composedRegions.partitioningIndex { $0.offset >= a }
    composedRegions.replaceSubrange(
      i..<(i + t.storedPartCount),
      with: CollectionOfOne(.init(offset: a, type: t.type)))
  }

  /// Throws iff the given `part` of some type at `baseOffset` is not represented as the `n`th
  /// composed region.
  public func requireComposed(
    part: TypeLayout.Part.Parentage,
    baseOffset: Offset,
    region n: Int
  ) throws {
    let p = part.parent.parts[part.partIndex]
    let partOffset = baseOffset + p.offset
    let partAddress = Memory.Address(allocation: allocation.pointee.id, offset: partOffset)
    guard let r = composedRegions.dropFirst(n).first,
      r.offset == partOffset
    else {
      throw Memory.Error.noComposedPart(at: partAddress, part)
    }
    if r.type != p.type {
      throw Memory.Error.partType(r.type, part: part)
    }
  }

  /// Replaces the initialization records starting at `a` for the
  /// parts of a `t` instance with the initialization record for a
  /// `t` instance.
  public mutating func compose(_ t: AnyType, at a: Offset) throws {
    try allocation.pointee.checkAlignmentAndAllocationBounds(at: a, for: typeLayouts.pointee[t])
    let t = typeLayouts.pointee[t]
    let i = composedRegions.partitioningIndex { $0.offset >= a }

    if t.isUnionLayout {
      let dc = t.discriminator
      try requireComposed(
        part: t.discriminatorParentage, baseOffset: a,
        region: dc.offset == 0 ? i : i + 1)

      let dv = allocation.pointee.unsignedIntValue(
        at: dc.offset + a, ofType: t.discriminator.type.base as! BuiltinType)

      try requireComposed(
        part: .init(t, Int(dv)), baseOffset: a,
        region: dc.offset == 0 ? i + 1 : i)
    } else {
      for n in t.parts.indices {
        try requireComposed(part: .init(t, n), baseOffset: a, region: i + n)
      }
    }

    composedRegions.replaceSubrange(
      i..<(i + t.storedPartCount),
      with: CollectionOfOne(.init(offset: a, type: t.type)))
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

  private func checkDecomposable(_ t: TypeLayout, at a: Offset) throws -> Int {
    if let i = decomposable(t, at: a) { return i }
    throw Memory.Error.noDecomposable(
      t,
      at: .init(allocation: allocation.pointee.id, offset: a))
  }

  /// Replaces the initialization record for a `t` instance at `a` with
  /// the initialization records for any parts of that instance.
  public mutating func decompose(_ t: AnyType, at a: Offset) throws {
    let t = typeLayouts.pointee[t]
    let i = try checkDecomposable(t, at: a)
    decompose(t, inRegion: i)
  }
}
