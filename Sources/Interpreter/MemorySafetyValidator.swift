import FrontEnd
import Utils

/// Validates safe use of memory within an `Allocation`.
struct MemorySafetyValidator {

  // Class Invariants:
  //   - If a region `r` has an active `.sink` access, then no strict ancestor
  //     of `r` is present in `composedRegions`.

  public enum Error: Regular, Swift.Error {
    case noTypeBound(at: Memory.Address)
    case notContained(Memory.Place, in: Memory.Place)
    case readFromIncomplete(Memory.Place)
    case accessToIncomplete(Memory.Place, effect: AccessEffect)
    case setAccessToPartiallyComplete(Memory.Place)
    case endAccessToIncomplete(Memory.Place, effect: AccessEffect)
  }

  /// A region within an `Allocation`, identified by a starting offset
  /// and the type associated with that location.
  typealias TypedRegion = Memory.Allocation.TypedRegion

  /// Non-overlapping type bindings reserving regions of `allocation`
  /// for values of particular types.
  private var typeBindings: ReservedTypeRegions = ReservedTypeRegions()

  /// The composed regions of `allocation`.
  private var composedRegions: ComposedRegions = ComposedRegions()

  /// Validates accesses for objects in `typeBindings`.
  private var accessValidators: [TypedRegion: AccessValidator<TypedRegion>] = [:]

  /// Allocation whose access is being validated by `self`.
  public let allocation: Memory.Allocation.ID

  /// Creates an instance to enforce memory safety in `allocation`.
  public init(_ allocation: Memory.Allocation.ID) {
    self.allocation = allocation
  }

  /// Starts and returns a new access to `r` in `a` having effect `e`.
  ///
  /// - Precondition: `a.id == allocation`.
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  public mutating func begin(
    _ e: AccessEffect,
    to r: TypedRegion, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws -> Access<TypedRegion> {
    precondition(allocation == a.id)

    if isZeroSized(r, typeLayouts: &l) { return Access<TypedRegion>(to: r, effect: e) }

    if typeBindings.region(enclosing: r.offset, typeLayouts: &l) == nil {
      if e != .set {
        throw Error.noTypeBound(at: .init(allocation: allocation, offset: r.offset))
      }
      try typeBindings.bind(r.type, at: r.offset, in: a, typeLayouts: &l)
      let r = TypedRegion(offset: r.offset, type: r.type)
      accessValidators[r] = .init(r, effect: .sink)
    }

    let ps = try path(to: r.type, at: r.offset, in: a, typeLayouts: &l)

    if e != .set && !composedRegions.isComplete(r, typeLayouts: &l) {
      throw Error.accessToIncomplete(asPlace(r), effect: e)
    }
    if e == .set && !composedRegions.isFullyUninitialized(r, typeLayouts: &l) {
      throw Error.setAccessToPartiallyComplete(asPlace(r))
    }

    if e == .sink {
      for e in ps.lazy.dropLast() {
        _ = composedRegions.tryDecompose(e.type, at: e.offset, in: a, typeLayouts: &l)
      }
    }

    return try accessValidators[ps.first!]!.begin(e, at: ps.dropFirst())
  }

  /// Ends `a` in `m`.
  ///
  /// - Precondition: `m.id == allocation`.
  /// - Precondition: Allocations in `m` conform to type layouts from `l`.
  ///
  /// - Postcondition: Provides strong exception safety guarantees.
  public mutating func end(
    _ a: Access<TypedRegion>, in m: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)

    let r = a.location
    if isZeroSized(r, typeLayouts: &l) { return }

    let ps = try path(to: r.type, at: r.offset, in: m, typeLayouts: &l)
    if a.effect != .sink {
      if !composedRegions.isComplete(r, typeLayouts: &l) {
        throw Error.endAccessToIncomplete(asPlace(r), effect: a.effect)
      }
    } else {
      markDeinitialized(r, typeLayouts: &l)
    }
    try accessValidators[ps.first!]!.end(a, at: ps.dropFirst())
  }

  /// Marks `r` in `a` as initialized.
  ///
  /// - Precondition: `a.id == allocation`.
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  ///
  /// - Postcondition: Provides strong exception safety guarantee.
  public mutating func markInitialized(
    _ r: TypedRegion, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == a.id)

    if isZeroSized(r, typeLayouts: &l) { return }

    let ps = try path(to: r.type, at: r.offset, in: a, typeLayouts: &l)
    let i = accessValidators[ps.first!]!.accesses(along: ps.dropFirst()).lastIndex {
      $0.contains { $0.effect == .sink }
    }!  // unwrap as every allocation base has sink access.
    for e in ps.lazy.dropLast() {
      _ = composedRegions.tryDecompose(e.type, at: e.offset, in: a, typeLayouts: &l)
    }
    composedRegions.markInitialized(ps.last!.type, at: ps.last!.offset, typeLayouts: &l)
    composedRegions.composeAncestors(along: ps[i...], in: a, typeLayouts: &l)
  }

  /// Throws iff it is not valid to read from `source` in `m`.
  ///
  /// - Precondition: `allocation == m.id`.
  /// - Precondition: Allocations in `m` conform to type layouts from `l`.
  public func requireCanRead(
    from source: Access<TypedRegion>, in m: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)

    let r = source.location
    if isZeroSized(r, typeLayouts: &l) { return }

    let ps = try path(to: r.type, at: r.offset, in: m, typeLayouts: &l)
    if !composedRegions.isComplete(r, typeLayouts: &l) {
      throw Error.readFromIncomplete(asPlace(r))
    }
    try accessValidators[ps.first!]!.requireIsActive(source, in: ps.dropFirst())
  }

  /// Throws iff it is not valid to write to `destination` in `m`.
  ///
  /// - Precondition: `allocation == m.id`.
  /// - Precondition: Allocations in `m` conform to type layouts from `l`.
  public func requireCanWrite(
    to destination: Access<TypedRegion>, in m: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == m.id)

    let r = destination.location
    if isZeroSized(r, typeLayouts: &l) { return }

    let ps = try path(to: r.type, at: r.offset, in: m, typeLayouts: &l)
    try accessValidators[ps.first!]!.requireIsActive(destination, in: ps.dropFirst())
  }

  /// Registers region starting at `i` in `a` to be interpreted as `t`.
  ///
  /// - Precondition: `allocation == a.id`.
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  public mutating func bindRegion(
    startingAt i: Int, to t: AnyType, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws {
    precondition(allocation == a.id)

    try typeBindings.bind(t, at: i, in: a, typeLayouts: &l)
    let r = TypedRegion(offset: i, type: t)
    accessValidators[r] = AccessValidator(r, effect: .sink)
  }

  /// Marks `r` as deinitialized.
  ///
  /// - Precondition: Allocations in `allocation` conform to type layouts from `l`.
  public mutating func markDeinitialized(
    _ r: TypedRegion, typeLayouts l: inout TypeLayoutCache
  ) {
    composedRegions.removeRegions(in: r, typeLayouts: &l)
  }

  /// Returns the sequence of typed regions leading to a value of `t` located at `i`.
  ///
  /// - Precondition: `allocation == a.id`.
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  private func path(
    to t: AnyType, at i: ReservedTypeRegions.Offset, in a: Memory.Allocation,
    typeLayouts l: inout TypeLayoutCache
  ) throws -> [TypedRegion] {
    precondition(allocation == a.id)

    guard var r = typeBindings.region(enclosing: i, typeLayouts: &l) else {
      throw Error.noTypeBound(at: .init(allocation: allocation, offset: i))
    }
    var path: [TypedRegion] = []
    while r.offset != i || r.type != t {
      path.append(r)
      guard let c = try child(r, enclosing: i, in: a, typeLayouts: &l) else {
        throw Error.notContained(
          .init(allocation: allocation, offset: i, type: t), in: asPlace(r)
        )
      }
      r = c
    }
    path.append(r)
    return path
  }

  /// Returns immediate child of `r` in `a` enclosing `i` of `typeBindings`.
  ///
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  private func child(
    _ r: TypedRegion, enclosing i: ReservedTypeRegions.Offset,
    in a: Memory.Allocation, typeLayouts l: inout TypeLayoutCache
  ) throws -> TypedRegion? {
    precondition(allocation == a.id)
    if r.type.isBuiltin || r.type.isVoidOrNever {
      return nil
    }
    if l[r.type].isUnionLayout {
      return try child(union: r, enclosing: i, in: a, typeLayouts: &l)
    }
    return child(product: r, enclosing: i, typeLayouts: &l)
  }

  /// Returns immediate child of `r` in `a` enclosing `i` of `typeBindings`.
  ///
  /// - Precondition: `allocation == a.id`
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  private func child(
    union r: TypedRegion, enclosing i: ReservedTypeRegions.Offset,
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

  /// Returns immediate child of `r` enclosing `i` of `typeBindings`.
  ///
  /// - Precondition: Allocations in `a` conform to type layouts from `l`.
  private func child(
    product r: TypedRegion, enclosing offset: Int, typeLayouts l: inout TypeLayoutCache
  ) -> TypedRegion? {
    let t = l[r.type]
    let d = offset - r.offset

    let i = t.parts.partitioningIndex { $0.offset > d } - 1

    guard i >= t.parts.startIndex else {
      return nil
    }

    let part = t.parts[i]

    let childStart = r.offset + part.offset
    let childEnd = childStart + l[part.type].size
    guard offset >= childStart && offset < childEnd else {
      return nil
    }

    return TypedRegion(offset: childStart, type: part.type)
  }

  /// Returns true iff `r.type` has size `0` according to `l`.
  private func isZeroSized(
    _ r: TypedRegion, typeLayouts l: inout TypeLayoutCache
  ) -> Bool {
    l[r.type].size == 0
  }

  /// Interprets `r` as `Memory.Place` using `allocation` for `Memory.Place.allocation`.
  private func asPlace(_ r: TypedRegion) -> Memory.Place {
    return .init(allocation: allocation, offset: r.offset, type: r.type)
  }

}

