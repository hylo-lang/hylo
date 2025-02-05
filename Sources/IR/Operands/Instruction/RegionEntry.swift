/// An instruction marking the entry into a region within an IR function.
public protocol RegionEntry: Instruction, Sendable {

  /// The type of the instruction marking exits of this region.
  associatedtype Exit: Sendable,  Instruction

}
