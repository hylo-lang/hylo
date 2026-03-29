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
    case typeAlreadyBinded(to: AnyType)
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

    /// The type layouts computed so far.
    ///
    /// `self` obeys type layout from `typeLayouts`.
    let typeLayouts: UnsafeMutablePointer<TypeLayoutCache>

    /// A region of memory that is reserved for storing a specific type.
    public struct TypedRegion: Regular {

      /// Where the region begins relative to an `Allocation`'s `baseOffset`.
      let offset: Storage.Index

      /// The type in the region.
      let type: AnyType

    }

    /// The typed regions of an `Allocation`.
    fileprivate typealias TypedRegions = [TypedRegion]

    /// The typed regions, in ascending order.
    private var typedRegions = TypedRegions()

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

    /// The composed regions of an `Allocation`.
    fileprivate typealias ComposedRegions = [ComposedRegion]

    /// The composed regions, in ascending order.
    private var composedRegions = ComposedRegions()

    /// `n` bytes with alignment `m` and the given `id`.
    private init(
      _ n: Int, bytesWithAlignment m: Int, id: ID,
      havingLayoutsFrom l: UnsafeMutablePointer<TypeLayoutCache>
    ) {
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
      self.typeLayouts = l
    }

    /// An allocation for `n` contiguous `t`s with the given `id`.
    public init(
      _ t: AnyType, count n: Int, id: ID,
      havingLayoutsFrom typeLayouts: UnsafeMutablePointer<TypeLayoutCache>
    ) {
      let l = typeLayouts.pointee[t]
      self.init(l.size * n, bytesWithAlignment: l.alignment, id: id, havingLayoutsFrom: typeLayouts)
    }

    /// The address of the `o`th byte.
    private func address(at o: Offset) -> Address { .init(allocation: id, offset: o) }

    /// Throws iff `a` lies inside any composed region.
    private func requireNotComposed(_ a: Offset) throws {
      let i = composedRegions.partitioningIndex { $0.offset > a }
      if i == 0 { return }
      let o = composedRegions[i - 1].offset
      let n = typeLayouts.pointee[composedRegions[i - 1].type].size
      if o + n > a {
        throw Error.regionAlreadyComposed(
          .init(
            allocation: self.id,
            offset: o,
            type: composedRegions[i - 1].type)
        )
      }
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

    /// Throws iff the given `part` of some type at `baseOffset` is not represented as the `n`th
    /// composed region.
    public func requireComposed(
      part: TypeLayout.Part.Parentage,
      baseOffset: Offset,
      region n: Int
    ) throws {
      let p = part.parent.parts[part.partIndex]
      let partOffset = baseOffset + p.offset
      let partAddress = address(at: partOffset)
      guard let r = composedRegions.dropFirst(n).first,
            r.offset == partOffset else {
        throw Error.noComposedPart(at: partAddress, part)
      }
      if r.type != p.type {
        throw Error.partType(r.type, part: part)
      }
    }

    /// Throws iff there is not enough allocated space for a `t` at `a`, or if it would not be
    /// properly aligned.
    fileprivate func checkAlignmentAndAllocationBounds(at a: Offset, for t: TypeLayout) throws {
      guard offset(a, hasAlignment: t.alignment) else {
        throw Error.alignment(address(at: a), for: t)
      }
      guard a + t.size <= self.size else {
        throw Error.bounds(address(at: a), for: t, allocationSize: self.size)
      }
    }

    /// Returns true iff initialization records starting at `a` for the parts
    /// of a `t` can be replaced with initialization record of `t` instance.
    public func canCompose(_ t: TypeLayout, at a: Offset) -> Bool {
      let i = composedRegions.partitioningIndex { $0.offset >= a }

      if t.isUnionLayout {
        let dc = t.discriminator
        if isComposed(
          part: t.discriminatorParentage, baseOffset: a,
          region: dc.offset == 0 ? i : i + 1) == false
        {
          return false
        }

        let dv = unsignedIntValue(
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
    public mutating func compose(_ t: TypeLayout, at a: Offset) throws {
      try checkAlignmentAndAllocationBounds(at: a, for: t)

      let i = composedRegions.partitioningIndex { $0.offset >= a }

      if t.isUnionLayout {
        let dc = t.discriminator
        try requireComposed(
          part: t.discriminatorParentage, baseOffset: a,
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

    /// Returns the typed region enclosing `a`.
    private func typedRegion(enclosing a: Offset) throws -> TypedRegion {
      let i = typedRegions.partitioningIndex { $0.offset > a }
      if i == 0 {
        throw Error.noTypedRegion(at: .init(allocation: self.id, offset: a))
      }
      let n = typedRegions[i - 1].offset + typeLayouts.pointee[typedRegions[i - 1].type].size
      if n < a
        || (n == a && typeLayouts.pointee[typedRegions[i - 1].type].size != 0)  // For void types
      {
        throw Error.noTypedRegion(at: .init(allocation: self.id, offset: a))
      }
      return typedRegions[i - 1]
    }

    /// Returns the sequence of stored parts from the root type of the enclosing
    /// `TypedRegion` to `t` at offset `a`.
    func pathFromRoot(to t: TypeLayout, at a: Offset) throws -> [TypedRegion] {
      var x: [TypedRegion] = []
      var r = try typedRegion(enclosing: a)
      let root = Place(allocation: self.id, offset: r.offset, type: r.type)
      let target = Place(allocation: self.id, offset: a, type: t.type)
      while !(r.type == t.type && r.offset == a) {
        x.append(r)
        let l = typeLayouts.pointee[r.type]
        if r.type.isBuiltin || r.type.isVoidOrNever || r.offset > a {
          throw Error.notContained(target, in: root)
        } else if l.isUnionLayout {
          let d = l.discriminator
          let i = composedRegions.partitioningIndex { $0.offset >= r.offset + d.offset }
          if i == composedRegions.endIndex || composedRegions[i].type != d.type {
            throw Error.notContained(target, in: root)
          }
          let dv = unsignedIntValue(
            at: d.offset + r.offset, ofType: l.discriminator.type.base as! BuiltinType)
          let p = l.parts[Int(dv)]
          r = TypedRegion(offset: r.offset + p.offset, type: p.type)
        } else {
          let d = a - r.offset
          let p = l.parts.partitioningIndex { $0.offset > d } - 1
          if p < l.parts.startIndex {
            throw Error.notContained(target, in: root)
          }
          r = TypedRegion(offset: r.offset + l.parts[p].offset, type: l.parts[p].type)
        }
      }
      x.append(r)
      return x
    }

    /// Marks region starting at `a` as initialized with `t`.
    ///
    /// The operation triggers recursive composition: if the newly
    /// initialized region completes the requirements of any enclosing
    /// type, those parts are replaced by their parent, continuing
    /// transitively up the layout hierarchy as long as possible.
    public mutating func markInitialized(_ t: TypeLayout, at a: Offset) throws {
      try checkAlignmentAndAllocationBounds(at: a, for: t)
      try requireNotComposed(a)
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

    /// Returns the region index of the top-level composed instance of `t` at `a`, if any, or `nil`
    /// otherwise.
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

    /// Constrains accesses to the region starting at `o` only according to
    /// structure of `t`, disallowing incompatible representations.
    public mutating func bind(type t: AnyType, at o: Offset) throws {
      let l = typeLayouts.pointee[t]
      try checkAlignmentAndAllocationBounds(at: o, for: l)
      let i = typedRegions.partitioningIndex { $0.offset > o }
      if i != 0 {
        let x = typedRegions[i - 1].offset
        let n = typeLayouts.pointee[typedRegions[i - 1].type].size
        if x + n > o {
          throw Error.typeAlreadyBinded(to: typedRegions[i - 1].type)
        }
      }
      typedRegions.insert(.init(offset: o, type: t), at: i)
    }

    /// Removes the type-based access constraints for the region starting at `o`.
    public mutating func unbindType(from o: Offset) {
      let i = typedRegions.partitioningIndex { $0.offset >= 0 }
      let j = typedRegions.partitioningIndex { $0.offset > 0 }
      typedRegions.removeSubrange(i..<j)
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
    withUnsafeMutablePointer(to: &typeLayouts) {
      allocation[a] = Allocation(t, count: n, id: a, havingLayoutsFrom: $0)
    }
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

  /// Replaces the initialization records starting at `a` for the
  /// parts of a `t` instance, with the initialization record for a
  /// `t` instance.
  public mutating func compose(_ t: AnyType, at a: Address) throws {
    try allocation[a.allocation]!.compose(typeLayouts[t], at: a.offset)
  }

  /// Returns true if `a` is aligned to an `n` byte boundary.
  public func place(_ a: Place, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
  }

  /// Returns true if `a` is aligned to an `n` byte boundary.
  public func address(_ a: Address, hasAlignment n: Int) -> Bool {
    allocation[a.allocation]!.offset(a.offset, hasAlignment: n)
  }

  /// Replaces the initialization record for a `t` instance at `a` with
  /// the initialization records for any parts of that instance.
  public mutating func decompose(_ t: AnyType, at a: Address) throws {
    let i = try checkDecomposable(typeLayouts[t], at: a)
    allocation[a.allocation]!.decompose(typeLayouts[t], inRegion: i)
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

  /// Constrains accesses to the region at `a` only according to
  /// structure of `t`, disallowing incompatible representations.
  public mutating func bind(type t: AnyType, at a: Address) throws {
    try allocation[a.allocation]!.bind(type: t, at: a.offset)
  }

  /// Removes the type-based access constraints for the region starting at `a`.
  public mutating func unbindType(from a: Address) {
    allocation[a.allocation]!.unbindType(from: a.offset)
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

