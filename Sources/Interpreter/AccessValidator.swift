import FrontEnd
import Utils

/// Validates accesses to an object and its subobjects, where subobjects
/// are identified by paths of `PathComponent` values.
///
/// `PathComponent` describes one step in a path to a subobject. For example,
/// components for a tuple might be field indices, while components for a
/// struct might be property names.
///
/// Invariants:
///  - Accesses are active when they are created.
///  - If `x` is an active exclusive access (`sink`, `set`, or `inout`) of an
///    object `y`, no other access that includes `y` is active.
struct AccessValidator<PathComponent: Regular & Comparable> {

  // The object `(b: (c: C, d: D), f: F)` is represented as:
  //             root
  //            /    \
  //           b      f
  //         /   \
  //        c     d
  // where each part has list of accesses it currently has.

  // Class invariants:
  //   1. Every part has at least one access or one child.
  //
  //   2. A `let` access is always active.
  //
  //   3. An `inout`/`set`/`sink` access is active iff:
  //      - it's the last element of `associatedAccesses` of the corresponding part, and
  //      - the part has no descendants.

  /// A subobject together with its associated accesses and immediate subparts.
  private struct Part {

    /// The step from parent subobject to this subobject.
    let step: PathComponent

    /// Stack of accesses in creation order, where `let` accesses are always
    /// active and other accesses are active only at the top of the stack.
    var associatedAccesses: [Access<PathComponent>]

    /// IDs of immediate subparts in global parts storage.
    var subparts: [PartID]

    /// Creates a `Part` reached from its parent by `step` with no accesses or subparts.
    init(_ step: PathComponent) {
      self.step = step
      self.associatedAccesses = []
      self.subparts = []
    }

  }

  /// Allocated parts, including reusable entries.
  private var storage: [Part] = []

  /// Identity of a `Part` in `storage`.
  private typealias PartID = Int

  /// IDs of reusable entries in `storage`.
  private var freeParts: [PartID] = []

  /// Creates a tracker for `object` with an initial access having capability `c`.
  public init(_ object: PathComponent, capability c: AccessEffect) {
    self.storage.append(Part(object))
    storage[0].associatedAccesses.append(Access<PathComponent>(to: object, effect: c))
  }

  /// An invalid access operation.
  public enum Error: Swift.Error, Regular {
    case overlappingExclusiveAccess(Path)
  }

  /// A path describing a part of an object.
  ///
  /// A `Path` is a sequence of `Component` values.
  ///
  /// - An empty sequence identifies the whole object being tracked.
  /// - Each subsequent element identifies a component of the region selected
  ///   by the previous elements.
  ///
  /// A path therefore describes a sequence of nested components, starting
  /// from a specific object and refining down to a subcomponent.
  ///
  /// For example, given a structure `(b: (c: C, d: D), e: E)`, the path
  /// `[b, d]` identifies `d`.
  public typealias Path = ArraySlice<PathComponent>

  /// Starts a new access having capability `a` at `p`.
  public mutating func begin(_ a: AccessEffect, at p: Path) throws -> Access<PathComponent> {
    let n = demandParts(p)
    guard let r = begin(a, at: n) else {
      throw Error.overlappingExclusiveAccess(p)
    }
    return r
  }

  /// Starts and returns new access having capability `a` for part at `i`;
  /// returns nil in case of error.
  private mutating func begin(_ a: AccessEffect, at i: PartID) -> Access<PathComponent>? {
    if !canBegin(a, at: i) {
      return nil
    }
    let r = Access<PathComponent>(to: storage[i].step, effect: a)
    self.storage[i].associatedAccesses.append(r)
    return r
  }

  /// Ends `a` at `p`.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public mutating func end(_ a: Access<PathComponent>, at p: Path) throws {
    try requireIsActive(a, in: p)

    let ns = partIDs(p)
    storage[ns.last!].associatedAccesses.removeAll { $0 == a }
    removeEmptySuffix(from: ns)
  }

  /// Throws iff `a` at `p` is not active.
  ///
  /// - Precondition: `a` is present at the part identified by `p`.
  public func requireIsActive(_ a: Access<PathComponent>, in p: Path) throws {
    if a.effect == .let { return }

    let i = firstPart(in: p, containing: a)
    if !storage[i].subparts.isEmpty || storage[i].associatedAccesses.last != a {
      throw Error.overlappingExclusiveAccess(p)
    }
  }

  /// Returns the accesses encountered along the path `p`, including the root.
  ///
  /// - Precondition: `p` corresponds to a valid path.
  public func accesses(along p: Path) -> [[Access<PathComponent>]] {
    partIDs(p).map { storage[$0].associatedAccesses }
  }

  /// Returns the part corresponding to path `p`, creating any missing
  /// intermediate parts along the path if needed.
  private mutating func demandParts(_ p: Path) -> PartID {
    var i = 0
    for c in p {
      i = part(reachedBy: c, from: i) ?? addChild(c, to: i)
    }
    return i
  }

  /// Returns the identity of the first part along `p` that contains `a`.
  private func firstPart(in p: Path, containing a: Access<PathComponent>) -> PartID {
    var i = 0
    for c in p {
      if storage[i].associatedAccesses.contains(a) { break }
      i = part(reachedBy: c, from: i)!
    }
    return i
  }

  /// Returns the `ID`s of parts along the longest existing prefix of `p`,
  /// starting from the root.
  ///
  /// The returned array always begins with ID of root part and has length equal
  /// to the number of matched path components plus one.
  private func partIDs(_ p: Path) -> [PartID] {
    var r = [0]
    for c in p {
      guard let i = part(reachedBy: c, from: r.last!)
      else { break }
      r.append(i)
    }
    return r
  }

  /// Adds `c` as a child to the `i`th part and returns identity of part corresponding
  /// to `c`.
  private mutating func addChild(_ c: PathComponent, to i: PartID) -> PartID {
    let r: PartID

    if let last = freeParts.popLast() {
      r = last
      storage[r] = Part(c)
    } else {
      r = storage.endIndex
      storage.append(Part(c))
    }

    let j = storage[i].subparts.partitioningIndex { storage[$0].step >= c }
    storage[i].subparts.insert(r, at: j)
    return r
  }

  /// Returns true iff all parts in subtree at `i` satisfy `predicate`.
  private func subtree(
    at i: PartID,
    satisfies predicate: (PartID) throws -> Bool
  ) rethrows -> Bool {
    try predicate(i)
      && (try storage[i].subparts.allSatisfy {
        try subtree(at: $0, satisfies: predicate)
      })
  }

  /// Removes the maximal suffix of `p` consisting of parts that have neither
  /// active accesses nor children.
  private mutating func removeEmptySuffix(from p: [PartID]) {
    var previousRemoved: PartID? = nil
    for i in p.reversed() {
      storage[i].subparts.removeAll { $0 == previousRemoved }
      if !storage[i].associatedAccesses.isEmpty || !storage[i].subparts.isEmpty {
        break
      }
      freeParts.append(i)
      previousRemoved = i
    }
  }

  /// Returns whether `a` may begin at `i` without overlapping exclusive
  /// descendant accesses.
  private func canBegin(_ a: AccessEffect, at i: PartID) -> Bool {
    if a == .let {
      return storage[i].subparts.allSatisfy {
        subtree(at: $0) {
          storage[$0].associatedAccesses.first?.effect == .let
        }
      }
    } else {
      return storage[i].subparts.isEmpty
    }
  }

  /// Returns the immediate subpart of `i` reached by `step`, if any.
  private func part(reachedBy step: PathComponent, from i: PartID) -> PartID? {
    let j = storage[i].subparts.partitioningIndex { storage[$0].step >= step }
    if j == storage[i].subparts.endIndex { return nil }
    return storage[i].subparts[j]
  }
}
