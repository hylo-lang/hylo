import FrontEnd
import Utils

public struct Memory {

  public init() {}

  public enum Error: Swift.Error, Regular {
    case partUninitialized(Address, TypeLayout.Component.ID)
    case alignment(Address, for: TypeLayout)
    case bounds(Address, for: TypeLayout, allocationSize: Int)
    case partType(AnyType, part: TypeLayout.Component.ID)
    case partOffset(Int, part: TypeLayout.Component.ID)
    case overlap(TypeLayout, InitializedRegion)
    case deallocationNotAtStartOfAllocation(Address)
    case noLongerAllocated(Address)
    case noDecomposable(TypeLayout, at: Address)
  }

  public typealias Offset = Int

  /// A region of raw memory in the interpreter
  public typealias Storage = [UInt8]

  /// A region of some raw memory that has been initialized with one
  /// or more instances of a single type.
  public struct InitializedRegion: Regular {
    /// Where the region begins relative to the `Allocation`'s `baseOffset`.
    let offset: Storage.Index

    /// The type with which the memory has been initialized.
    let type: AnyType
  }

  public struct Allocation {
    public typealias ID = Offset

    var storage: Storage
    public let baseOffset: Offset
    public let size: Int
    public let id: ID

    fileprivate typealias InitializedRegions = [InitializedRegion]
    private var initializedRegions = InitializedRegions()

    /// `n` bytes with alignment `m`.
    public init(_ n: Int, bytesWithAlignment m: Int, id: ID) {
      precondition(n >= 0)
      precondition(m > 0)

      storage = Storage(repeating: 0, count: n)
      // If we didn't get suitably-aligned storage, allocate enough to
      // ensure we can find a suitably-aligned region of the right
      // size.
      if storage.withUnsafeBytes({ UInt(bitPattern: $0.baseAddress) % UInt(m) != 0 }) {
        storage = Storage(repeating: 0, count: n + m - 1)
      }
      baseOffset = storage.withUnsafeBytes { $0.firstOffsetAligned(to: m) }
      size = n
      self.id = id
    }

    private func address(at o: Offset) -> Address { .init(allocation: id, offset: o) }

    public func requireInitialized(
      part partID: TypeLayout.Component.ID,
      baseOffset: Offset,
      region n: Int
    ) throws {
      let part = partID.layout.components[partID.component]
      let partOffset = baseOffset + part.offset
      let partAddress = address(at: partOffset)
      guard let r = initializedRegions.dropFirst(n).first,
            r.offset == partOffset else {
        throw Error.partUninitialized(partAddress, partID)
      }
      if r.type != part.type {
        throw Error.partType(r.type, part: partID)
      }
    }

    fileprivate func checkAlignmentAndAllocationBounds(at a: Offset, for t: TypeLayout) throws {
      guard offset(a, hasAlignment: t.alignment) else {
        throw Error.alignment(address(at: a), for: t)
      }
      guard a + t.size <= self.size else {
        throw Error.bounds(address(at: a), for: t, allocationSize: self.size)
      }
    }

    /// Replaces the initialization records starting at `a` for the
    /// parts of a `t` instance, with the initialization record for a
    /// `t` instance.
    public mutating func compose(_ t: TypeLayout, at a: Offset) throws {
      try checkAlignmentAndAllocationBounds(at: a, for: t)

      let i = initializedRegions.partitioningIndex { $0.offset >= a }

      if t.isUnionLayout {
        let dc = t.discriminator
        try requireInitialized(
          part: t.discriminatorID, baseOffset: a,
          region: dc.offset == 0 ? i : i + 1)

        let dv = unsignedIntValue(
          at: dc.offset + a, storedAs: t.discriminator.type.base as! BuiltinType)

        try requireInitialized(
          part: .init(t, Int(dv)), baseOffset: a,
          region: dc.offset == 0 ? i + 1 : i)
      }
      else {
        for n in t.components.indices {
          try requireInitialized(part: .init(t, n), baseOffset: a, region: i + n)
        }
      }

      initializedRegions.replaceSubrange(
        i..<(i + t.storedComponents),
        with: CollectionOfOne(.init(offset: a, type: t.type)))
    }

    mutating func withMutableUnsafeStorage<R>(_ a: Offset, _ body: (UnsafeMutableRawPointer)->R) -> R {
      storage.withUnsafeMutableBytes { p in body(p.baseAddress! + baseOffset + a) }
    }

    func withUnsafeStorage<R>(_ a: Offset, _ body: (UnsafeRawPointer)->R) -> R {
      storage.withUnsafeBytes { p in body(p.baseAddress! + baseOffset + a) }
    }

    /// Returns the unsigned interpretation of the builtin integer value at `a`;
    private func unsignedIntValue(at a: Offset, storedAs t: BuiltinType) -> UInt {
      if case .i(let n) = t {
        return switch n {
        case 8: UInt(withUnsafeStorage(a) { $0.assumingMemoryBound(to: UInt8.self).pointee })
        case 16: UInt(withUnsafeStorage(a) { $0.assumingMemoryBound(to: UInt16.self).pointee })
        case 32: UInt(withUnsafeStorage(a) { $0.assumingMemoryBound(to: UInt32.self).pointee })
        case 64: UInt(withUnsafeStorage(a) { $0.assumingMemoryBound(to: UInt64.self).pointee })
        default: fatalError("Unknown builtin integer size \(n)")
        }
      } else {
        fatalError("Unrecognized builtin integer type \(t)")
      }

    }

    /// Returns true if `o` is alinged to an `n` byte boundary.
    public func offset(_ o: Offset, hasAlignment n: Int) -> Bool {
      storage.withUnsafeBytes {
        UInt(bitPattern: $0.baseAddress! + baseOffset + o) % UInt(n) == 0
      }
    }

    fileprivate func decomposable(_ t: TypeLayout, at a: Offset) -> InitializedRegions.Index? {
      let i = initializedRegions.partitioningIndex { $0.offset >= a }
      guard let r0 = initializedRegions[i...].first,
            r0.offset == a,
            r0.type == t.type
      else {
        return nil
      }
      assert(
        initializedRegions[i...].dropFirst().first.map {
          $0.offset >= r0.offset + t.size
        } ?? true
      )
      return i
    }

    /// Replaces the initialization record for a `t` instance at `a` with
    /// the initialization records for any parts of that instance.
    fileprivate mutating func decompose(_ t: TypeLayout, inRegion i: InitializedRegions.Index) {
      let a = initializedRegions[i].offset

      if t.isUnionLayout {
        let expectedDiscriminator = t.components.last!
        let discriminator = InitializedRegion.init(offset: expectedDiscriminator.offset + a, type: expectedDiscriminator.type)
        let d = unsignedIntValue(at: discriminator.offset, storedAs: discriminator.type.base as! BuiltinType)
        let expectedPayload = t.components[Int(d)]
        let payload = InitializedRegion(offset: expectedPayload.offset + a, type: expectedPayload.type)
        let newRecords = expectedDiscriminator.offset == 0 ? [discriminator, payload] : [payload, discriminator]
        initializedRegions.replaceSubrange(i..<i + 1, with: newRecords)
      }
      else {
        initializedRegions.replaceSubrange(
          i..<i + 1, with: t.components.lazy.map { .init(offset: a + $0.offset, type: $0.type) })
      }
    }
  }

  public struct Address: Regular, CustomStringConvertible {

    public let allocation: Allocation.ID
    public let offset: Storage.Index

    public init(allocation: Allocation.ID, offset: Storage.Index) {
      self.allocation = allocation
      self.offset = offset
    }

    public var description: String { "@\(allocation):0x\(String(offset, radix: 16))" }
  }

  public private(set) var allocation: [Allocation.ID: Allocation] = [:]
  var nextAllocation = 0

  /// Allocates `n` bytes with alignment `m`.
  public mutating func allocate(_ n: Int, bytesWithAlignment m: Int) -> Address {
    let a = nextAllocation
    nextAllocation += 1
    allocation[a] = Allocation(n, bytesWithAlignment: m, id: a)
    return .init(allocation: a, offset: 0)
  }

  /// Deallocates the allocated memory starting at `a`.
  public mutating func deallocate(_ a: Address) throws {
    if a.offset != 0 {
      throw Error.deallocationNotAtStartOfAllocation(a)
    }
    let v = allocation.removeValue(forKey: a.allocation)
    if v == nil {
      throw Error.noLongerAllocated(a)
    }
  }

  /// Replaces the initialization records starting at `a` for the
  /// parts of a `t` instance, with the initialization record for a
  /// `t` instance.
  public mutating func compose(_ t: TypeLayout, at a: Address) throws {
    try allocation[a.allocation]!.compose(t, at: a.offset)
  }

  /// Returns true if `a` is alinged to an `n` byte boundary.
  public func address(_ a: Address, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
  }

  /// Replaces the initialization record for a `t` instance at `a` with
  /// the initialization records for any parts of that instance.
  public mutating func decompose(_ t: TypeLayout, at a: Address) throws {
    let i = try checkDecomposable(t, at: a)
    allocation[a.allocation]!.decompose(t, inRegion: i)
  }

  private func checkDecomposable(_ t: TypeLayout, at a: Address) throws -> Allocation.InitializedRegions.Index {
    guard let block = allocation[a.allocation] else {
      throw Error.noLongerAllocated(a)
    }
    try block.checkAlignmentAndAllocationBounds(at: a.offset, for: t)
    if let i = block.decomposable(t, at: a.offset) { return i }
    throw Error.noDecomposable(t, at: a)
  }

  /// Allocation at given address.
  public subscript(_ i: Allocation.ID) -> Allocation {
    _read {
      yield allocation[i]!
    }
    _modify {
      yield &allocation[i]!
    }
  }

}

public extension Memory.Address {

  static func +(l: Self, r: Int) -> Self {
    .init(allocation: l.allocation, offset: l.offset + r)
  }

  static func -(l: Self, r: Int) -> Self {
    .init(allocation: l.allocation, offset: l.offset - r)
  }

  static func +(l: Int, r: Self) -> Self {
    .init(allocation: r.allocation, offset: l + r.offset)
  }

  static func +=(l: inout Self, r: Int) { l = l + r }

  static func -=(l: inout Self, r: Int)  { l = l - r }

}
