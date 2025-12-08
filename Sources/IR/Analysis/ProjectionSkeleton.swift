/// The skeleton of a projection declaration, used to analyze the projection and expand it.
internal struct ProjectionSkeleton {

  /// The yield points in the projection, in the order they appear in the function.
  public let yieldPoints: [InstructionID]

  /// The blocks that are part of the ramp, i.e., the blocks that must be executed before an yield instruction.
  public let rampBlocks: [Block.ID]

  /// The blocks that are part of the slide, i.e., the blocks that must be executed after an yield instruction.
  public let slideBlocks: [Block.ID]

}
