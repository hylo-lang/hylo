import FrontEnd
import Utils

/// A runtime validator that validates the safe use of memory within an allocation.
///
/// The validator does not manage memory. Instead, it observes and validates
/// accesses to an existing allocation.
struct MemorySafetyValidator {

  /// Class Invariants:
  ///   - If a region `d` has an active `.sink` access, then no strict ancestor
  ///     of `d` is present in `composedRegions`.

  public enum Error: Regular, Swift.Error {
    case noTypeBound(at: Memory.Address)
    case notContains(Memory.Place, in: Memory.Place)
    case readFromIncomplete(Memory.Place)
    case accessToIncomplete(Memory.Place, kind: AccessKind)
    case setAccessToPartiallyComplete(Memory.Place)
    case endAccessToIncomplete(Memory.Place, kind: AccessKind)
  }

  /// A region within an `Allocation`, identified by a starting offset
  /// and the type layout associated with that location.
  typealias TypedRegion = Memory.Allocation.TypedRegion

  private let allocation: UnsafePointer<Memory.Allocation>

  private let typeLayouts: UnsafeMutablePointer<TypeLayoutCache>

  /// The type bindings associated with regions of `allocation`.
  private var typeBindings: ReservedTypeRegions

  /// The composed regions of `allocation`.
  private var composedRegions: ComposedRegions

  /// Accesses for each region in `typeBindings`.
  private var regionAccesses: [TypedRegion: AccessTracker<TypedRegion>]

  /// Creates a validator for accesses within `allocation`, using `typeLayouts`
  /// to resolve layout information.
  public init(
    allocation: UnsafePointer<Memory.Allocation>,
    typeLayouts: UnsafeMutablePointer<TypeLayoutCache>
  ) {
    self.allocation = allocation
    self.typeLayouts = typeLayouts
    typeBindings = ReservedTypeRegions(typeLayouts: typeLayouts)
    composedRegions = ComposedRegions(allocation: allocation, typeLayouts: typeLayouts)
    regionAccesses = [:]
  }

  /// Starts an access of type `k` at `p`.
  public mutating func beginAccess(_ k: AccessKind, at p: Memory.Place) throws -> Access {
    if typeBindings.region(enclosing: p.offset) == nil {
      if k != .set {
        throw Error.noTypeBound(at: .init(allocation: allocation.pointee.id, offset: p.offset))
      }
      try typeBindings.bind(p.type, at: p.offset)
      let r = TypedRegion(startOffset: p.offset, type: p.type)
      regionAccesses[r] = .init(r, with: .sink)
    }

    let ps = try path(to: p.type, at: p.offset)

    if k != .set && !composedRegions.isComplete(p) {
      throw Error.accessToIncomplete(p, kind: k)
    }
    if k == .set && !composedRegions.isFullyUninitialized(p) {
      throw Error.setAccessToPartiallyComplete(p)
    }

    if k == .sink {
      composedRegions.decompose(upto: p, along: ps)
    }

    return try regionAccesses[ps.first!]!.begin(k, at: ps.dropFirst())
  }

  /// Ends `a` at `p`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  public mutating func endAccess(_ a: Access, at p: Memory.Place) throws {
    let ps = try path(to: p.type, at: p.offset)
    try regionAccesses[ps.first!]!.end(a, at: ps.dropFirst())
    if a.kind != .sink {
      if !composedRegions.isComplete(p) {
        throw Error.endAccessToIncomplete(p, kind: a.kind)
      }
    } else {
      composedRegions.decomposeSubtree(of: p)
    }
  }

  /// Marks `p` as initialized.
  public mutating func markInitialized(_ p: Memory.Place) throws {
  }

  /// Throws iff it is not valid to read bytes from `p` using `a`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  public func requireCanRead(from p: Memory.Place, using a: Access) throws {
    let ps = try path(to: p.type, at: p.offset)
    if !composedRegions.isComplete(p) {
      throw Error.readFromIncomplete(p)
    }
    try regionAccesses[ps.first!]!.requireIsActive(a, at: ps.dropFirst())
  }

  /// Throws iff it is not valid to write bytes to `p` using `a`.
  ///
  /// - Precondition: Access `a` exists at `p`.
  public func requireCanWrite(to p: Memory.Place, using a: Access) throws {
    let ps = try path(to: p.type, at: p.offset)
    try regionAccesses[ps.first!]!.requireIsActive(a, at: ps.dropFirst())
  }

  /// Returns the sequence of typed regions leading to a value of `t` located at `a`.
  func path(to t: AnyType, at a: Int) throws -> [TypedRegion] {
    guard var r = typeBindings.region(enclosing: a) else {
      throw Error.noTypeBound(at: .init(allocation: allocation.pointee.id, offset: a))
    }
    var path: [TypedRegion] = []
    while r.startOffset != a || r.type != t {
      path.append(r)
      guard let c = try childRegion(of: r, containing: a) else {
        throw Error.notContains(
          .init(allocation: allocation.pointee.id, offset: r.startOffset, type: r.type),
          in: .init(allocation: allocation.pointee.id, offset: a, type: t),
        )
      }
      r = c
    }
    path.append(r)
    return path
  }

  /// Returns the immediate child region of `r` that contains `a`.
  private func childRegion(of r: TypedRegion, containing a: Int) throws -> TypedRegion? {
    if r.type.isBuiltin || r.type.isVoidOrNever {
      return nil
    }
    if typeLayouts.pointee[r.type].isUnionLayout {
      return try unionChildRegion(of: r, containing: a)
    }
    return productChildRegion(of: r, containing: a)
  }

  /// Returns the immediate child region of the union `r` that contains `a`.
  private func unionChildRegion(of r: TypedRegion, containing a: Int) throws -> TypedRegion? {
    let l = typeLayouts.pointee[r.type]
    let d = l.discriminator

    let discriminatorStart = r.startOffset + d.offset
    let discriminatorEnd = discriminatorStart + typeLayouts.pointee[d.type].size
    if a >= discriminatorStart && a < discriminatorEnd {
      guard
        let c = composedRegions.region(enclosing: discriminatorStart),
        c.type == d.type
      else {
        return nil
      }
      return .init(startOffset: c.offset, type: c.type)
    }

    let dv = allocation.pointee.unsignedIntValue(
      at: discriminatorStart,
      ofType: d.type.base as! BuiltinType
    )
    let p = l.parts[Int(dv)]
    let activeCaseStart = r.startOffset + p.offset
    let activeCaseEnd = activeCaseStart + typeLayouts.pointee[p.type].size
    guard a >= activeCaseStart && a < activeCaseEnd else {
      return nil
    }
    return TypedRegion(startOffset: r.startOffset + p.offset, type: p.type)
  }

  /// Returns the immediate child region of the product `r` that contains `a`.
  private func productChildRegion(of region: TypedRegion, containing offset: Int) -> TypedRegion? {
    let l = typeLayouts.pointee[region.type]
    let d = offset - region.startOffset

    let i = l.parts.partitioningIndex { $0.offset > d } - 1

    guard i >= l.parts.startIndex else {
      return nil
    }

    let part = l.parts[i]

    let childStart = region.startOffset + part.offset
    let childEnd = childStart + typeLayouts.pointee[part.type].size
    guard offset >= childStart && offset < childEnd else {
      return nil
    }

    return TypedRegion(startOffset: childStart, type: part.type)
  }
}
