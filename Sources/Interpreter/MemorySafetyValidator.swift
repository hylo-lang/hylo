import FrontEnd
import Utils

/// A runtime validator that validates the safe use of memory within an allocation.
///
/// The validator does not manage memory. Instead, it observes and validates
/// accesses to an existing allocation.
struct MemorySafetyValidator {

  // Class Invariants:
  //   - If a region `d` has an active `.sink` access, then no strict ancestor
  //     of `d` is present in `composedRegions`.

  public enum Error: Regular, Swift.Error {
    case noTypeBound(at: Memory.Address)
    case notContained(Memory.Place, in: Memory.Place)
    case readFromIncomplete(Memory.Place)
    case accessToIncomplete(Memory.Place, kind: AccessEffect)
    case setAccessToPartiallyComplete(Memory.Place)
    case endAccessToIncomplete(Memory.Place, kind: AccessEffect)
  }

  /// A region within an `Allocation`, identified by a starting offset
  /// and the type layout associated with that location.
  typealias TypedRegion = Memory.Allocation.TypedRegion

  /// The type bindings associated with regions of `allocation`.
  private var typeBindings: ReservedTypeRegions = ReservedTypeRegions()

  /// The composed regions of `allocation`.
  private var composedRegions: ComposedRegions = ComposedRegions()

  /// Accesses for each region in `typeBindings`.
  private var regionAccesses: [TypedRegion: AccessTracker<TypedRegion>] = [:]

  /// Allocation whose access is being validated by `self`.
  public var allocation: Memory.Allocation.ID

  /// Creates and instance of validator for `allocation`.
  public init(_ allocation: Memory.Allocation.ID) {
    self.allocation = allocation
  }

  /// Starts an access of type `k` at `p`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  public mutating func beginAccess(
    _ k: AccessEffect, at p: Memory.Place,
    in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws -> Access {
    precondition(allocation == a.id)
    precondition(allocation == p.allocation)

    if isZeroSized(p, typeLayouts: &l) { return Access(effect: k) }

    if typeBindings.region(enclosing: p.offset, typeLayouts: &l) == nil {
      if k != .set {
        throw Error.noTypeBound(at: .init(allocation: p.allocation, offset: p.offset))
      }
      try typeBindings.bind(p.type, at: p.offset, in: a, typeLayouts: &l)
      let r = TypedRegion(offset: p.offset, type: p.type)
      regionAccesses[r] = .init(r, with: .sink)
    }

    let ps = try path(to: p.type, at: p.offset, in: a, typeLayouts: &l)

    if k != .set && !composedRegions.isComplete(p.typedRegion, typeLayouts: &l) {
      throw Error.accessToIncomplete(p, kind: k)
    }
    if k == .set && !composedRegions.isFullyUninitialized(p.typedRegion, typeLayouts: &l) {
      throw Error.setAccessToPartiallyComplete(p)
    }

    if k == .sink {
      for e in ps.lazy.dropLast() {
        _ = composedRegions.tryDecompose(e.type, at: e.offset, in: a, typeLayouts: &l)
      }
    }

    return try regionAccesses[ps.first!]!.begin(k, at: ps.dropFirst())
  }

  /// Ends `a` at `p`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  /// - Precondition: All allocations follow type layouts from `l`.
  ///
  /// - Postcondition: Provides strong exception safety guarantees.
  public mutating func endAccess(
    _ a: Access, at p: Memory.Place,
    in m: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)
    precondition(allocation == p.allocation)

    if isZeroSized(p, typeLayouts: &l) { return }

    let ps = try path(to: p.type, at: p.offset, in: m, typeLayouts: &l)
    if a.effect != .sink {
      if !composedRegions.isComplete(p.typedRegion, typeLayouts: &l) {
        throw Error.endAccessToIncomplete(p, kind: a.effect)
      }
    } else {
      composedRegions.decomposeSubtree(of: p.typedRegion, typeLayouts: &l)
    }
    try regionAccesses[ps.first!]!.end(a, at: ps.dropFirst())
  }

  /// Marks `p` as initialized.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  public mutating func markInitialized(
    _ p: Memory.Place, in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == a.id)
    precondition(allocation == p.allocation)

    if isZeroSized(p, typeLayouts: &l) { return }

    let ps = try path(to: p.type, at: p.offset, in: a, typeLayouts: &l)
    let i = regionAccesses[ps.first!]!.accesses(along: ps.dropFirst()).lastIndex {
      $0.contains { $0.effect == .sink }
    }!  // unwrap as every allocation base has sink access.
    composedRegions.compose(ps.last!.type, at: ps.last!.offset, typeLayouts: &l)
    composedRegions.composeUpwards(along: ps[i...], in: a, typeLayouts: &l)
  }

  /// Throws iff it is not valid to read bytes from `p` using `a`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  /// - Precondition: All allocations follow type layouts from `l`.
  public func requireCanRead(
    from p: Memory.Place, using a: Access,
    in m: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)
    precondition(allocation == p.allocation)

    if isZeroSized(p, typeLayouts: &l) { return }

    let ps = try path(to: p.type, at: p.offset, in: m, typeLayouts: &l)
    if !composedRegions.isComplete(p.typedRegion, typeLayouts: &l) {
      throw Error.readFromIncomplete(p)
    }
    try regionAccesses[ps.first!]!.requireIsActive(a, in: ps.dropFirst())
  }

  /// Throws iff it is not valid to write bytes to `p` using `a`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  /// - Precondition: All allocations follow type layouts from `l`.
  public func requireCanWrite(
    to p: Memory.Place, using a: Access,
    in m: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)
    precondition(allocation == p.allocation)

    if isZeroSized(p, typeLayouts: &l) { return }

    let ps = try path(to: p.type, at: p.offset, in: m, typeLayouts: &l)
    try regionAccesses[ps.first!]!.requireIsActive(a, in: ps.dropFirst())
  }

  /// Returns the sequence of typed regions leading to a value of `t` located at `i`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  func path(
    to t: AnyType, at i: Int, in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws -> [TypedRegion] {
    precondition(allocation == a.id)
    guard var r = typeBindings.region(enclosing: i, typeLayouts: &l) else {
      throw Error.noTypeBound(at: .init(allocation: allocation, offset: i))
    }
    var path: [TypedRegion] = []
    while r.offset != i || r.type != t {
      path.append(r)
      guard let c = try childRegion(of: r, containing: i, in: a, typeLayouts: &l) else {
        throw Error.notContained(
          .init(allocation: allocation, offset: r.offset, type: r.type),
          in: .init(allocation: allocation, offset: i, type: t),
        )
      }
      r = c
    }
    path.append(r)
    return path
  }

  /// Returns the immediate child region of `r` that contains `i`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  private func childRegion(
    of r: TypedRegion, containing i: Int,
    in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws -> TypedRegion? {
    precondition(allocation == a.id)
    if r.type.isBuiltin || r.type.isVoidOrNever {
      return nil
    }
    if l[r.type].isUnionLayout {
      return try unionChildRegion(of: r, containing: i, in: a, typeLayouts: &l)
    }
    return productChildRegion(of: r, containing: i, typeLayouts: &l)
  }

  /// Returns the immediate child region of the union `r` that contains `i`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  private func unionChildRegion(
    of r: TypedRegion, containing i: Int,
    in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws -> TypedRegion? {
    precondition(allocation == a.id)
    let t = l[r.type]
    let d = t.discriminator

    let discriminatorStart = r.offset + d.offset
    let discriminatorEnd = discriminatorStart + l[d.type].size
    if i >= discriminatorStart && i < discriminatorEnd {
      guard
        let c = composedRegions.region(enclosing: discriminatorStart, typeLayouts: &l),
        c.type == d.type
      else {
        return nil
      }
      return .init(offset: c.offset, type: c.type)
    }

    let dv = a.unsignedIntValue(
      at: discriminatorStart, ofType: d.type.base as! BuiltinType
    )
    let p = t.parts[Int(dv)]
    let activeCaseStart = r.offset + p.offset
    let activeCaseEnd = activeCaseStart + l[p.type].size
    guard i >= activeCaseStart && i < activeCaseEnd else {
      return nil
    }
    return TypedRegion(offset: r.offset + p.offset, type: p.type)
  }

  /// Returns the immediate child region of the product `r` that contains `a`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  private func productChildRegion(
    of region: TypedRegion, containing offset: Int, typeLayouts l: inout TypeLayoutCache
  ) -> TypedRegion? {
    let t = l[region.type]
    let d = offset - region.offset

    let i = t.parts.partitioningIndex { $0.offset > d } - 1

    guard i >= t.parts.startIndex else {
      return nil
    }

    let part = t.parts[i]

    let childStart = region.offset + part.offset
    let childEnd = childStart + l[part.type].size
    guard offset >= childStart && offset < childEnd else {
      return nil
    }

    return TypedRegion(offset: childStart, type: part.type)
  }

  /// Returns true iff `p.type` has size `0`.
  ///
  /// - Precondition: All allocations follow type layouts from `l`.
  private func isZeroSized(
    _ p: Memory.Place, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    l[p.type].size == 0
  }
}

private extension Memory.Place {
  /// `TypedRegion` inside `self.allocation`.
  var typedRegion: Memory.Allocation.TypedRegion {
    .init(offset: self.offset, type: self.type)
  }
}
