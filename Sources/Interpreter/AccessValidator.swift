import FrontEnd
import Utils

/// Validates accesses to an object and its subobjects, where subobjects
/// are identified by paths of `PathComponent` values.
///
/// Invariants:
///  - Accesses are active when they are created.
///  - If `x` is an active exclusive access (`sink`, `set`, or `inout`) of an
///    object `o`, no other access that includes `o` is active.
struct AccessValidator<PathComponent: Regular & Comparable> {

  // The object `(b: (c: C, d: D), f: F)` may be represented as:
  //
  //             root
  //            /    \
  //           b      f
  //         /   \
  //        c     d
  //
  // Each part stores the accesses currently associated with it.
  // Parts without accesses or accessed descendants does not exist.

  /// A subobject's accesses and immediate subparts.
  ///
  /// - Invariant: Has atleast one access or one child.
  private struct Part {

    /// The step from parent subobject to this subobject.
    let step: PathComponent

    /// Accesses ordered by creation time, where `let` accesses are
    /// always active, and all other accesses are active only when they
    /// are the most recently created access and this part has no subparts.
    var accesses: [Access<PathComponent>]

    /// The immediate subparts.
    var subparts: [PartID]

    /// Number of non-`let` accesses in `self`'s descendant parts.
    var descendantExclusiveAccessCount: Int

    /// Creates an instance reached from its parent by `step`, having no accesses or subparts.
    init(_ step: PathComponent) {
      self.step = step
      self.accesses = []
      self.subparts = []
      descendantExclusiveAccessCount = 0
    }

  }

  /// Allocated parts, including free entries no longer in use.
  private typealias PartStorage = Array<Part>

  /// Identity of a `Part` in `parts`.
  private typealias PartID = PartStorage.Index

  /// Allocated parts, including free entries no longer in use.
  private var parts: PartStorage = []

  /// Elements of `parts` not currently in use.
  private var freeList: [PartStorage.Index] = []

  /// Creates an instance storing a `c` access to `object`.
  public init(_ object: PathComponent, effect c: AccessEffect) {
    self.parts.append(Part(object))
    parts[0].accesses.append(Access<PathComponent>(to: object, effect: c))
  }

  /// An invalid access operation.
  public enum Error: Swift.Error, Regular {
    case overlappingExclusiveAccess(Path)
  }

  /// A route to subpart of some object.
  ///
  /// - An empty sequence identifies the whole object being tracked.
  /// - Each subsequent element identifies a part of the region identified
  ///   by the previous elements.
  ///
  /// For example, given a structure `(b: (c: C, d: D), e: E)`, the path
  /// `[b, d]` identifies `d`.
  public typealias Path = ArraySlice<PathComponent>

  /// Starts and returns a new access having effect `e` at `p`.
  public mutating func begin(_ e: AccessEffect, at p: Path) throws -> Access<PathComponent> {
    let n = demandParts(p)
    guard let r = begin(e, at: n) else {
      throw Error.overlappingExclusiveAccess(p)
    }
    if e != .let {
      beginExclusiveAccess(at: p)
    }
    return r
  }

  /// Starts and returns new access having effect `e` for `p`;
  /// returns nil in case of error.
  private mutating func begin(_ e: AccessEffect, at p: PartID) -> Access<PathComponent>? {
    if !canBegin(e, at: p) {
      return nil
    }
    let r = Access<PathComponent>(to: parts[p].step, effect: e)
    self.parts[p].accesses.append(r)
    return r
  }

  /// Ends `a` at `p`.
  ///
  /// - Postcondition: Provides strong exception safety guarantee.
  public mutating func end(_ a: Access<PathComponent>, at p: Path) throws {
    try requireIsActive(a, in: p)

    let ns = partIDs(p)
    parts[ns.last!].accesses.removeAll { $0 == a }
    releaseMaximalEmptySuffix(ns)
    if a.effect != .let {
      endExclusiveAccess(at: p)
    }
  }

  /// Throws iff `a` at `p` is not active.
  public func requireIsActive(_ a: Access<PathComponent>, in p: Path) throws {
    if a.effect == .let { return }

    let i = firstPart(in: p, containing: a)
    if !parts[i].subparts.isEmpty || parts[i].accesses.last != a {
      throw Error.overlappingExclusiveAccess(p)
    }
  }

  /// Returns the accesses encountered along `p`, including the root.
  public func accesses(along p: Path) -> [[Access<PathComponent>]] {
    partIDs(p).map { parts[$0].accesses }
  }

  /// Returns the part corresponding to `p`, creating any missing
  /// intermediate parts along `p` if needed.
  private mutating func demandParts(_ p: Path) -> PartID {
    var i = 0
    for c in p {
      i = part(reachedBy: c, from: i) ?? addChild(c, to: i)
    }
    return i
  }

  /// Returns first part along `p` containing `a`.
  private func firstPart(in p: Path, containing a: Access<PathComponent>) -> PartID {
    var i = 0
    for c in p {
      if parts[i].accesses.contains(a) { break }
      i = part(reachedBy: c, from: i)!
    }
    return i
  }

  /// Returns parts along longest existing prefix of `p`, starting from root.
  ///
  /// The returned array always begins with ID of root part and has length equal
  /// to the number of matched path components plus one.
  private func partIDs(_ p: Path) -> [PartID] {
    var r: [PartID] = []
    forEachExistingPart(p) { r.append($0) }
    return r
  }

  /// Adds `c` as child to `p` and returns part corresponding to `c`.
  private mutating func addChild(_ c: PathComponent, to p: PartID) -> PartID {
    let r: PartID

    if let last = freeList.popLast() {
      r = last
      parts[r] = Part(c)
    } else {
      r = parts.endIndex
      parts.append(Part(c))
    }

    let j = parts[p].subparts.partitioningIndex { parts[$0].step >= c }
    parts[p].subparts.insert(r, at: j)
    return r
  }

  /// Removes trailing parts of `ps` that contain neither accesses nor subparts.
  private mutating func releaseMaximalEmptySuffix(_ ps: [PartID]) {
    var reclaimedChild: PartID? = nil

    for i in ps.reversed() {
      parts[i].subparts.removeAll { $0 == reclaimedChild }
      if !parts[i].accesses.isEmpty || !parts[i].subparts.isEmpty {
        break
      }
      freeList.append(i)
      reclaimedChild = i
    }
  }

  /// Returns true iff starting an access at `p` with effect `e` would
  /// not conflict with active accesses in subparts of `p`.
  private func canBegin(_ e: AccessEffect, at p: PartID) -> Bool {
    if e == .let {
      parts[p].descendantExclusiveAccessCount == 0
    } else {
      parts[p].subparts.isEmpty
    }
  }

  /// Calls `f` with each existing part along `p` in `Path` order, stopping at
  /// the first missing part.
  private func forEachExistingPart(_ p: Path, _ f: (PartID) -> ()) {
    var i = 0
    f(i)
    for c in p {
      guard let j = part(reachedBy: c, from: i)
      else { break }
      i = j
      f(i)
    }
  }

  /// Returns the immediate subpart of `i` reached by `step`, if any.
  private func part(reachedBy step: PathComponent, from p: PartID) -> PartID? {
    let j = parts[p].subparts.partitioningIndex { parts[$0].step >= step }
    if j == parts[p].subparts.endIndex { return nil }
    return parts[p].subparts[j]
  }

  /// Increment exclusive access count in ancestors of `p.last`, if any.
  private mutating func beginExclusiveAccess(at p: Path) {
    guard !p.isEmpty else { return }
    forEachExistingPart(p.dropLast()) {
      parts[$0].descendantExclusiveAccessCount += 1
    }
  }

  /// Decrement exclusive access count in ancestors of `p.last`, if any.
  private mutating func endExclusiveAccess(at p: Path) {
    guard !p.isEmpty else { return }
    forEachExistingPart(p.dropLast()) {
      parts[$0].descendantExclusiveAccessCount -= 1
    }
  }
}
