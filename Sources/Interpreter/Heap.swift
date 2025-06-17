import FrontEnd

struct Heap {

  /// A region of raw memory in the interpreter
  typealias RawMemory = [UInt8]

  /// A region of some raw memory that has been initialized with one
  /// or more instances of a single type.
  struct InitializedRegion {
    /// Where the region begins in the raw memory block.
    let baseOffset: RawMemory.Index

    /// The type with which the memory has been initialized.
    let type: AnyType

    /// The number of consecutive `type` instances that begin at `baseOffset`.
    var count: Int
  }

  struct Allocation {
    typealias ID = Array<Allocation>.Index

    var memory: RawMemory
    var initializedRegions: [InitializedRegion]
  }

  struct Address {
    let allocation: Allocation.ID
    let offset: RawMemory.Index
  }

  var allocation: [Allocation] = []

}
