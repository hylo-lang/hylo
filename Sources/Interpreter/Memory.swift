import FrontEnd

struct Memory {

  typealias Offset = Int

  /// A region of raw memory in the interpreter
  typealias Storage = [UInt8]

  /// A region of some raw memory that has been initialized with one
  /// or more instances of a single type.
  struct InitializedRegion {
    /// Where the region begins relative to the `Allocation`'s `baseOffset`.
    let offset: Storage.Index

    /// The type with which the memory has been initialized.
    let type: AnyType
  }

  struct Allocation {
    typealias ID = Offset

    var storage: Storage
    var baseOffset: Offset
    var initializedRegions: [InitializedRegion] = []

    /// Replaces the initialization records starting at `a` for the
    /// parts of a `t` instance, with the initialization record for a
    /// `t` instance.
    public mutating func finishInitialization(at a: Offset, to t: TypeLayout) {
      precondition(address(a, hasAlignment: t.alignment))
      let i = initializedRegions.partitioningIndex { $0.offset >= a }
      var j = i

      if t.isUnionLayout {
        let expectedDiscriminator = t.components.last!
        let discriminator = initializedRegions[expectedDiscriminator.offset == 0 ? i : i + 1]
        precondition(discriminator.type == expectedDiscriminator.type)
        precondition(discriminator.offset == expectedDiscriminator.offset + a)
        let d = unsignedIntValue(at: discriminator.offset, storedAs: discriminator.type.base as! BuiltinType)
        let expectedPayload = t.components[Int(d)]
        let payload = initializedRegions[expectedDiscriminator.offset == 0 ? i + 1 : i]
        precondition(payload.type == expectedPayload.type)
        precondition(payload.offset == expectedPayload.offset + a)
        initializedRegions.formIndex(&j, offsetBy: 2)
      }
      else {
        for c in t.components {
          precondition(initializedRegions[j].type == c.type)
          precondition(initializedRegions[j].offset == a + c.offset)
          initializedRegions.formIndex(after: &j)
        }
      }
      precondition(
        j == initializedRegions.count
          || initializedRegions[j].offset >= a + t.size)
      initializedRegions.replaceSubrange(i..<j, with: CollectionOfOne(.init(offset: a, type: t.type)))
    }

    mutating func withMutableAddress<R>(_ a: Offset, _ body: (UnsafeMutableRawPointer)->R) -> R {
      storage.withUnsafeMutableBytes { p in body(p.baseAddress! + baseOffset + a) }
    }

    func withAddress<R>(_ a: Offset, _ body: (UnsafeRawPointer)->R) -> R {
      storage.withUnsafeBytes { p in body(p.baseAddress! + baseOffset + a) }
    }

    /// Returns the unsigned interpretation of the builtin integer value at `a`;
    func unsignedIntValue(at a: Offset, storedAs t: BuiltinType) -> UInt {
      switch t {
      case .i(8): UInt(withAddress(a) { $0.assumingMemoryBound(to: UInt8.self).pointee })
      case .i(16): UInt(withAddress(a) { $0.assumingMemoryBound(to: UInt16.self).pointee })
      case .i(32): UInt(withAddress(a) { $0.assumingMemoryBound(to: UInt32.self).pointee })
      case .i(64): UInt(withAddress(a) { $0.assumingMemoryBound(to: UInt64.self).pointee })
      default: fatalError("Unrecognized builtin integer type \(t)")
      }
    }

    public func address(_ place: Offset, hasAlignment alignment: Int) -> Bool {
      storage.withUnsafeBytes {
        UInt(bitPattern: $0.baseAddress! + baseOffset + place) % UInt(alignment) == 0
      }
    }

    /// Replaces the initialization record for a `t` instance at `a` with
    /// the initialization records for any parts of that instance.
    public mutating func startDeinitialization(at a: Offset, of t: TypeLayout) {
      precondition(address(a, hasAlignment: t.alignment))
      let i = initializedRegions.partitioningIndex { $0.offset >= a }
      precondition(initializedRegions[i].offset == a)
      precondition(initializedRegions[i].type == t.type)
      precondition(i + 1 == initializedRegions.count || initializedRegions[i + 1].offset > t.size)
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

  struct Address {
    let allocation: Allocation.ID
    let offset: Storage.Index
  }

  var allocation: [Allocation.ID: Allocation] = [:]
  var nextAllocation = 0

  mutating func allocate(_ n: Int, bytesAlignedTo alignment: Int) -> Address {
    precondition(n >= 0)
    precondition(alignment > 0)

    var s = Storage(repeating: 0, count: n)
    if s.withUnsafeBytes({ UInt(bitPattern: $0.baseAddress) % UInt(alignment) != 0 }) {
      s = Storage(repeating: 0, count: n + alignment - 1)
    }
    let a = nextAllocation
    nextAllocation += 1
    allocation[a] = Allocation(storage: s, baseOffset: s.withUnsafeBytes { $0.firstOffsetAligned(to: alignment) })
    return .init(allocation: a, offset: 0)
  }

  mutating func deallocate(_ a: Allocation.ID) {
    precondition(allocation[a] != nil)
    allocation[a] = nil
  }

}
