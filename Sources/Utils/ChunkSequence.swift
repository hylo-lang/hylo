/// A sequence of chunks partitioning a collection.
public struct ChunkSequence<Base: Collection>: IteratorProtocol, Sequence {

  /// A chunk in the base collection.
  public typealias Element = Base.SubSequence

  /// The collection being partitioned into chunks.
  private let base: Base

  /// The maximum number of elements in a single chunk.
  private let chunkCapacity: Int

  /// The index of the next chunk.
  private var nextChunkStart: Base.Index

  /// Creates an instance partitioning `base` into at most `maxChunkCount` chunks.
  public init(partitioning base: Base, intoMax maxChunkCount: Int) {
    self.base = base
    let q = base.count / maxChunkCount
    self.chunkCapacity = (base.count % maxChunkCount == 0) ? q : q + 1
    self.nextChunkStart = base.startIndex
  }

  /// Returns the next chunk in the sequence or `nil` if the sequence is empty.
  public mutating func next() -> Element? {
    if nextChunkStart == base.endIndex { return nil }
    let end = base.index(
      nextChunkStart, offsetBy: chunkCapacity, limitedBy: base.endIndex) ?? base.endIndex
    defer { nextChunkStart = end }
    return base[nextChunkStart ..< end]
  }

}

extension Collection {

  /// Returns a partition of self with at most `maxSplits` chunks.
  public func chunked(maxSplits: Int) -> ChunkSequence<Self> {
    .init(partitioning: self, intoMax: maxSplits)
  }

}
