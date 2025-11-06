import FrontEnd
import Utils

public struct Memory {

  public init() {}

  public enum Error: Swift.Error, Regular {
    case noComposedPart(at: Address, TypeLayout.Part.ID)
    case alignment(Address, for: TypeLayout)
    case bounds(Address, for: TypeLayout, allocationSize: Int)
    case partType(AnyType, part: TypeLayout.Part.ID)
    case partOffset(Int, part: TypeLayout.Part.ID)
    case deallocationNotAtStartOfAllocation(Address)
    case noLongerAllocated(Address)
    case noDecomposable(TypeLayout, at: Address)
  }

  public typealias Offset = Int

  /// A region of raw memory in the interpreter
  public typealias Storage = [UInt8]

  public struct Allocation {
    public typealias ID = Offset

    var storage: Storage
    public let baseOffset: Offset
    public let size: Int
    public let id: ID


    /// A region of some allocation that contains a complete instance of
    /// some type, ready to be a part of a new composition.
    ///
    /// See `compose(:at:)`.
    public struct ComposedRegion: Regular {

      /// Where the region begins relative to an `Allocation`'s `baseOffset`.
      let offset: Storage.Index

      /// The type in the region.
      let type: AnyType

    }

    fileprivate typealias ComposedRegions = [ComposedRegion]
    private var composedRegions = ComposedRegions()

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

    public func requireComposed(
      part partID: TypeLayout.Part.ID,
      baseOffset: Offset,
      region n: Int
    ) throws {
      let part = partID.layout.parts[partID.part]
      let partOffset = baseOffset + part.offset
      let partAddress = address(at: partOffset)
      guard let r = composedRegions.dropFirst(n).first,
            r.offset == partOffset else {
        throw Error.noComposedPart(at: partAddress, partID)
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

      let i = composedRegions.partitioningIndex { $0.offset >= a }

      if t.isUnionLayout {
        let dc = t.discriminator
        try requireComposed(
          part: t.discriminatorID, baseOffset: a,
          region: dc.offset == 0 ? i : i + 1)

        let dv = unsignedIntValue(
          at: dc.offset + a, ofType: t.discriminator.type.base as! BuiltinType)

        try requireComposed(
          part: .init(t, Int(dv)), baseOffset: a,
          region: dc.offset == 0 ? i + 1 : i)
      }
      else {
        for n in t.parts.indices {
          try requireComposed(part: .init(t, n), baseOffset: a, region: i + n)
        }
      }

      composedRegions.replaceSubrange(
        i..<(i + t.storedPartCount),
        with: CollectionOfOne(.init(offset: a, type: t.type)))
    }

    mutating func withUnsafeMutablePointer<T, R>(to _: T.Type, at a: Offset, _ body: (UnsafeMutablePointer<T>)->R) -> R {
      precondition(a + MemoryLayout<T>.size < size)
      return storage.withUnsafeMutableBytes { p in
        body((p.baseAddress! + baseOffset + a).assumingMemoryBound(to: T.self))
      }
    }

    func withUnsafePointer<T, R>(to _: T.Type, at a: Offset, _ body: (UnsafePointer<T>)->R) -> R {
      precondition(a + MemoryLayout<T>.size < size)
      return storage.withUnsafeBytes { p in
        body((p.baseAddress! + baseOffset + a).assumingMemoryBound(to: T.self))
      }
    }

    /// Returns the unsigned interpretation of the builtin integer value at `a`;
    private func unsignedIntValue(at a: Offset, ofType t: BuiltinType) -> UInt {
      if case .i(let n) = t {
        return switch n {
        case 8: UInt(withUnsafePointer(to: UInt8.self, at: a) { $0.pointee })
        case 16: UInt(withUnsafePointer(to: UInt16.self, at: a) { $0.pointee })
        case 32: UInt(withUnsafePointer(to: UInt32.self, at: a) { $0.pointee })
        case 64: UInt(withUnsafePointer(to: UInt64.self, at: a) { $0.pointee })
        default: fatalError("Unknown builtin integer size \(n)")
        }
      } else {
        fatalError("Unrecognized builtin integer type \(t)")
      }
    }

    /// Initializes the raw storage of builtin integer type `t` at `a` to `x`.
    ///
    /// - Note: performs no alignment, size, initialization, or type
    ///   checking. Use with care!
    internal mutating func initialize(_ t: BuiltinType, at a: Offset, to x: UInt) {
      if case .i(let n) = t {
        switch n {
        case 8: withUnsafeMutablePointer(to: UInt8.self, at: a) { $0.initialize(to: .init(x)) }
        case 16: withUnsafeMutablePointer(to: UInt16.self, at: a) { $0.initialize(to: .init(x)) }
        case 32: withUnsafeMutablePointer(to: UInt32.self, at: a) { $0.initialize(to: .init(x)) }
        case 64: withUnsafeMutablePointer(to: UInt64.self, at: a) { $0.initialize(to: .init(x)) }
        default: fatalError("Unknown builtin integer size \(n)")
        }
      } else {
        fatalError("Unrecognized builtin integer type \(t)")
      }
    }

    /// Returns true if `o` is aligned to an `n` byte boundary.
    public func offset(_ o: Offset, hasAlignment n: Int) -> Bool {
      storage.withUnsafeBytes {
        UInt(bitPattern: $0.baseAddress! + baseOffset + o) % UInt(n) == 0
      }
    }

    fileprivate func decomposable(_ t: TypeLayout, at a: Offset) -> ComposedRegions.Index? {
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
    fileprivate mutating func decompose(_ t: TypeLayout, inRegion i: ComposedRegions.Index) {
      let a = composedRegions[i].offset

      if t.isUnionLayout {
        let expectedDiscriminator = t.parts.last!
        let discriminator = ComposedRegion.init(offset: expectedDiscriminator.offset + a, type: expectedDiscriminator.type)
        let d = unsignedIntValue(at: discriminator.offset, ofType: discriminator.type.base as! BuiltinType)
        let expectedPayload = t.parts[Int(d)]
        let payload = ComposedRegion(offset: expectedPayload.offset + a, type: expectedPayload.type)
        let newRecords = expectedDiscriminator.offset == 0 ? [discriminator, payload] : [payload, discriminator]
        composedRegions.replaceSubrange(i..<i + 1, with: newRecords)
      }
      else {
        composedRegions.replaceSubrange(
          i..<i + 1, with: t.parts.lazy.map { .init(offset: a + $0.offset, type: $0.type) })
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

  /// Returns true if `a` is aligned to an `n` byte boundary.
  public func address(_ a: Address, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
  }

  /// Replaces the initialization record for a `t` instance at `a` with
  /// the initialization records for any parts of that instance.
  public mutating func decompose(_ t: TypeLayout, at a: Address) throws {
    let i = try checkDecomposable(t, at: a)
    allocation[a.allocation]!.decompose(t, inRegion: i)
  }

  private func checkDecomposable(_ t: TypeLayout, at a: Address) throws -> Allocation.ComposedRegions.Index {
    guard let block = allocation[a.allocation] else {
      throw Error.noLongerAllocated(a)
    }
    try block.checkAlignmentAndAllocationBounds(at: a.offset, for: t)
    if let i = block.decomposable(t, at: a.offset) { return i }
    throw Error.noDecomposable(t, at: a)
  }

  /// The allocation identified by `i`.
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
