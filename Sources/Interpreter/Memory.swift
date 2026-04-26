import FrontEnd
import Utils

/// The memory of an interpreted process.
public struct Memory {

  /// An empty instance whose allocation and access semantics are defined by
  /// the types in `p` and the target `abi`.
  public init(typesIn p: TypedProgram, for abi: any TargetABI) {
    typeLayouts = .init(typesIn: p, for: abi)
  }

  /// An incorrect use of memory.
  public enum Error: Swift.Error, Regular {
    case noComposedPart(at: Address, TypeLayout.Part.Parentage)
    case alignment(Address, for: TypeLayout)
    case bounds(Address, for: TypeLayout, allocationSize: Int)
    case partType(AnyType, part: TypeLayout.Part.Parentage)
    case partOffset(Int, part: TypeLayout.Part.Parentage)
    case deallocationNotAtStartOfAllocation(Address)
    case noLongerAllocated(Address)
    case noDecomposable(TypeLayout, at: Address)
    case regionAlreadyReserved(for: AnyType)
    case noTypedRegion(at: Address)
    case regionAlreadyComposed(Place)
    case notContained(Place, in: Place)
  }

  /// The type layouts that been computed so far.
  ///
  /// Invariant: All allocations in `self` obeys type layout from `typeLayouts`.
  var typeLayouts: TypeLayoutCache

  /// A position in some allocation.
  public typealias Offset = Int

  /// The bytes of an `Allocation` preceded by zero or more bytes of initial padding for alignment
  /// purposes.
  public typealias Storage = [UInt8]

  /// A usable region.
  public struct Allocation {

    /// A unique `Allocation` identifier.
    public typealias ID = Int

    /// The bytes preceded by zero or more bytes of initial padding for alignment purposes.
    var storage: Storage

    /// The number of bytes in `storage` before `self` logically begins.
    public let baseOffset: Offset

    /// The number of usable bytes of `self`.
    public let size: Int

    /// The identity of `self`, unique throughout time for a given `Memory`.
    public let id: ID

    /// A region within an `Allocation`, identified by a starting offset
    /// and the type layout associated with that location.
    public struct TypedRegion: Regular {

      /// Start offset relative to base offset.
      let startOffset: Offset

      /// The type describing the layout of the region starting at `startOffset`.
      let type: AnyType

    }

    /// `n` bytes with alignment `m` and the given `id`.
    private init(_ n: Int, bytesWithAlignment m: Int, id: ID) {
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

    /// An allocation for `n` contiguous `t`s with the given `id`.
    public init(_ t: TypeLayout, count n: Int, id: ID) {
      self.init(t.size * n, bytesWithAlignment: t.alignment, id: id)
    }

    /// The address of the `o`th byte.
    private func address(at o: Offset) -> Address { .init(allocation: id, offset: o) }

    /// Throws iff there is not enough allocated space for a `t` at `a`, or if it would not be
    /// properly aligned.
    internal func checkAlignmentAndAllocationBounds(at a: Offset, for t: TypeLayout) throws {
      guard offset(a, hasAlignment: t.alignment) else {
        throw Error.alignment(address(at: a), for: t)
      }
      guard a + t.size <= self.size else {
        throw Error.bounds(address(at: a), for: t, allocationSize: self.size)
      }
    }

    /// Returns the result of calling `body` on the storage for a `T` instance at `a`.
    ///
    /// - Precondition: the storage exists and is properly aligned.
    mutating func withUnsafeMutablePointer<T, R>(
      to _: T.Type, at a: Offset, _ body: (UnsafeMutablePointer<T>)->R
    ) -> R {
      precondition(a + MemoryLayout<T>.size <= size)
      precondition(offset(a, hasAlignment: MemoryLayout<T>.alignment))
      return storage.withUnsafeMutableBytes { p in
        body((p.baseAddress! + baseOffset + a).assumingMemoryBound(to: T.self))
      }
    }

    /// Returns the result of calling `body` on the storage for a `T` instance at `a`.
    ///
    /// - Precondition: the storage exists and is properly aligned.
    func withUnsafePointer<T, R>(
      to _: T.Type, at a: Offset, _ body: (UnsafePointer<T>)->R
    ) -> R {
      precondition(a + MemoryLayout<T>.size <= size)
      return storage.withUnsafeBytes { p in
        body((p.baseAddress! + baseOffset + a).assumingMemoryBound(to: T.self))
      }
    }

    /// Returns the unsigned interpretation of the builtin integer value at `a`;
    func unsignedIntValue(at a: Offset, ofType t: BuiltinType) -> UInt {
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

    /// Returns `true` iff `o` is aligned to an `n` byte boundary.
    public func offset(_ o: Offset, hasAlignment n: Int) -> Bool {
      storage.withUnsafeBytes {
        UInt(bitPattern: $0.baseAddress! + baseOffset + o) % UInt(n) == 0
      }
    }

  }

  /// A memory location.
  public struct Address: Regular, CustomStringConvertible {

    /// The containing allocation.
    public let allocation: Allocation.ID

    /// The offset from the beginning of that `allocation`.
    public let offset: Storage.Index

    /// An instance in the given `allocation` at `offset`.
    public init(allocation: Allocation.ID, offset: Storage.Index) {
      self.allocation = allocation
      self.offset = offset
    }

    public var description: String { "@\(allocation):0x\(String(offset, radix: 16))" }

    /// Returns a `Place` referring to the same `allocation` and `offset` as `self`,
    /// viewed as having type `t`.
    public func asPlace(of t: AnyType) -> Place {
      Place(allocation: allocation, offset: offset, type: t)
    }
  }

  /// A typed location in memory.
  public struct Place: Regular, CustomStringConvertible {

    /// The containing allocation.
    public let allocation: Allocation.ID

    /// The offset from the beginning of that `allocation`.
    public let offset: Storage.Index

    /// The type to be accessed at `offset` in `allocation`.
    public let type: AnyType

    /// Address having same `allocation` and `offset` as of `self`.
    public var address: Address {
      .init(allocation: allocation, offset: offset)
    }

    /// An instance in the given `allocation` at `offset` to be accessed as `type`.
    public init(allocation: Allocation.ID, offset: Storage.Index, type: AnyType) {
      self.allocation = allocation
      self.offset = offset
      self.type = type
    }

    public var description: String { "@\(allocation):0x\(String(offset, radix: 16))[\(type)]" }
  }

  /// The live allocations, by ID
  public private(set) var allocation: [Allocation.ID: Allocation] = [:]

  /// The ID of the next allocated block.
  private var nextAllocation = 0

  public mutating func allocate(_ t: AnyType, count n: Int = 1) -> Place {
    let a = nextAllocation
    nextAllocation += 1
    allocation[a] = Allocation(typeLayouts[t], count: n, id: a)
    return .init(allocation: a, offset: 0, type: t)
  }

  /// Deallocates the allocated memory starting at `a`.
  public mutating func deallocate(_ a: Place) throws {
    if a.offset != 0 {
      throw Error.deallocationNotAtStartOfAllocation(a.address)
    }
    let v = allocation.removeValue(forKey: a.allocation)
    if v == nil {
      throw Error.noLongerAllocated(a.address)
    }
  }

  /// Returns true if `a` is aligned to an `n` byte boundary.
  public func place(_ a: Place, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
  }

  /// Returns true if `a` is aligned to an `n` byte boundary.
  public func address(_ a: Address, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
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

  /// Returns `l` offset by `r` bytes.
  static func +(l: Self, r: Int) -> Self {
    .init(allocation: l.allocation, offset: l.offset + r)
  }

  /// Returns `l` offset by `-r` bytes.
  static func -(l: Self, r: Int) -> Self {
    .init(allocation: l.allocation, offset: l.offset - r)
  }

  /// Returns `r` offset by `l` bytes.
  static func +(l: Int, r: Self) -> Self {
    .init(allocation: r.allocation, offset: l + r.offset)
  }

  ///  Offsets `l` by `r` bytes.
  static func +=(l: inout Self, r: Int) { l = l + r }

  ///  Offsets `l` by `-r` bytes.
  static func -=(l: inout Self, r: Int)  { l = l - r }

}

extension Memory.Allocation {

  /// Stores `v` at `o`.
  private mutating func store<T>(_ v: T, at o: Memory.Offset) throws {
    // TODO: throw in case of call to store with wrong type by unsafe code.
    withUnsafeMutablePointer(to: T.self, at: o) { $0.pointee = v }
  }

  /// Stores `v` at `o`.
  mutating func store(_ v: BuiltinValue, at o: Memory.Offset) throws {
    switch v {
    case .i1(let x): try store(x, at: o)
    case .i8(let x): try store(x, at: o)
    case .i16(let x): try store(x, at: o)
    case .i32(let x): try store(x, at: o)
    case .i64(let x): try store(x, at: o)
    case .i128(let x): try store(x, at: o)
    }
  }

}

extension Memory {
  /// Returns the address of `subPart` in `whole`.
  public mutating func location(of subPart: RecordPath, in whole: Place) -> Place {
    let (o, t) = typeLayouts.layout(of: subPart, in: typeLayouts[whole.type])
    return .init(allocation: whole.allocation, offset: o + whole.offset, type: t.type)
  }

  /// Stores `v` in `target`.
  mutating func store(_ v: BuiltinValue, in target: Place) throws {
    try self[target.allocation].store(v, at: target.offset)
  }

  /// Returns the result of calling `body` with raw buffer pointer to bytes in `p`.
  ///
  /// - Precondition: `typeLayouts[p.type] == l`.
  private func withUnsafeBytes<R>(
    _ p: Place, havingLayout l: TypeLayout, _ body: (UnsafeRawBufferPointer) -> R
  ) -> R {
    let o = self[p.allocation].baseOffset
    return self[p.allocation].storage.withUnsafeBytes {
      let s = $0.baseAddress!.advanced(by: o + p.offset)
      return body(UnsafeRawBufferPointer(start: s, count: l.size))
    }
  }

  /// Returns the result of calling `body` with mutable raw buffer pointer to bytes in `p`.
  private mutating func withUnsafeMutableBytes<R>(
    _ p: Place, _ body: (UnsafeMutableRawBufferPointer) -> R
  ) -> R {
    let o = self[p.allocation].baseOffset
    let n = typeLayouts[p.type].size
    return self[p.allocation].storage.withUnsafeMutableBytes {
      let s = $0.baseAddress!.advanced(by: o + p.offset)
      return body(UnsafeMutableRawBufferPointer(start: s, count: n))
    }
  }

  /// Copies the bytes of `source` to `destination`.
  public mutating func copy(_ source: Place, to destination: Place) throws {
    precondition(
      source.type == destination.type,
      "Copy source type \(source.type) must match with destination type \(destination.type).")
    // TODO: throw when source is not in composed regions and mark destination region composed.
    self.withUnsafeBytes(source, havingLayout: typeLayouts[source.type]) { a in
      self.withUnsafeMutableBytes(destination) {
        var b = $0
        b.copyElements(from: a)
      }
    }
  }

  /// Returns the builtin value stored in `p`.
  func builtinValue(in p: Place) throws -> BuiltinValue {
    precondition(p.type.isBuiltin);
    precondition(allocation[p.allocation] != nil)

    // TODO: throw if `p` doesn't have a composed builtin value.
    let o = p.offset;
    let a = allocation[p.allocation]!
    let t = p.type.base as! BuiltinType
    return switch t {
    case .i(1): a.withUnsafePointer(to: Bool.self, at: o) { .i1($0.pointee) }
    case .i(8): a.withUnsafePointer(to: UInt8.self, at: o) { .i8($0.pointee) }
    case .i(16): a.withUnsafePointer(to: UInt16.self, at: o) { .i16($0.pointee) }
    case .i(32): a.withUnsafePointer(to: UInt32.self, at: o) { .i32($0.pointee) }
    case .i(64): a.withUnsafePointer(to: UInt64.self, at: o) { .i64($0.pointee) }
    case .i(128): a.withUnsafePointer(to: UInt128.self, at: o) { .i128($0.pointee) }
    default: fatalError("Unsupported builtin type \(t).")
    }
  }
}

