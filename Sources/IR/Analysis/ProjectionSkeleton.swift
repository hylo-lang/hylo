/// The skeleton of a projection declaration, used to analyze the projection and expand it.
internal struct ProjectionSkeleton {

  /// The projection's identifier.
  public let id: Function.ID

  /// The yield points in the projection, in the order they appear in the function.
  public let yieldPoints: [InstructionID]

  /// The blocks that are part of the ramp, i.e., the blocks that must be executed before an yield instruction.
  public let rampBlocks: [Block.ID]

  /// The blocks that are part of the slide, i.e., the blocks that must be executed after an yield instruction.
  public let slideBlocks: [Block.ID]

  /// The instructions in the yield blocks, partitioned in three groups: instructions before the yield point,
  /// instructions after the yield point that locally belong to the ramp, and instructions after the yield.
  public let yieldBlockInstructions: [InstructionID: BlockSplit]

}
