import FrontEnd
import Utils

/// Tracks cross-region values that must be materialized in a function frame
/// and provides the metadata required to rewrite their uses.
struct FrameMaterializationInfo {

  /// Materialization metadata for instructions that must become frame-backed.
  fileprivate var instructionInfo: [InstructionID: InstructionMaterializationInfo] = [:]

  /// Collects instructions in `region` from `f` that must become frame-backed, excluding `ignored`.
  ///
  /// We collect all instructions from `region` that are used outside `region`, and all the
  /// instructions outside of `region` that are used by instructions in `region`.
  public mutating func collectCrossRegionInstructions<C: Sequence>(
    in f: Function, from region: C, ignoring ignored: [InstructionID] = []
  ) where C.Element == InstructionID {
    for i in region where !ignored.contains(i) {
      // Check if this is used outside the region, and thus it must become frame-backed.
      if isUsedOutsideRegion(i, in: f, region: region) {
        instructionInfo[i] = InstructionMaterializationInfo(id: i, in: f)
      }

      // Check if the defining instructions of operands must become frame-backed.
      for j in operandsOutsideOfRegion(i, in: f, region: region) where !ignored.contains(j) {
        instructionInfo[j] = instructionInfo[j] ?? InstructionMaterializationInfo(id: j, in: f)
      }
    }
  }

  /// Returns `true` if instruction `i` in `f` is used outside the region `r`.
  private func isUsedOutsideRegion<C: Sequence>(
    _ i: InstructionID, in f: Function, region r: C
  ) -> Bool where C.Element == InstructionID {
    for u in f.allUses(of: i) {
      if !r.contains(u.user) {
        return true
      }
    }
    return false
  }

  /// Checks if the instruction `i` in `f` has operands defined outside the region `r`, and returns
  /// the instructions that define them.
  private func operandsOutsideOfRegion<C: Sequence>(
    _ i: InstructionID, in f: Function, region r: C
  ) -> [InstructionID] where C.Element == InstructionID {
    f[i].operands.compactMap({ (o) -> InstructionID? in
      switch o {
      case .register(let def):
        return r.contains(def) ? nil : def
      case .parameter:
        // We assume parameters are always defined inside the region; emitter will generate local
        // code for them in the entry block.
        return nil
      case .constant:
        return nil
      }
    })
  }

}

/// Materialization metadata for a value that crosses region boundaries.
///
/// If instruction `j` uses instruction `i` and they are in different regions, then we need to
/// materialize `i` in the frame, and not `j`.
///
/// We keep track of the subfield path to get to the storage of `i` from the frame, and the
/// instruction that actually allocates the storage.
private struct InstructionMaterializationInfo {

  /// The ID of the instruction that must be materialized in the frame.
  let id: InstructionID

  /// The subfield path to get to the storage of `id` from the frame, if any.
  let subfield: RecordPath

  /// The instruction that actually allocates the storage.
  var storageInstruction: InstructionID

  /// Creates metadata for materializing `id` in the frame.
  public init(id: InstructionID, in f: Function) {
    self.id = id
    (self.subfield, self.storageInstruction) = Self.storageTrace(from: id, in: f)
  }

  /// Walks the instruction chain starting from `id` until an `AllocStack` is found,
  /// accumulating the subfield path, and returning the `(subfield, storageInstruction)` pair.
  private static func storageTrace(
    from id: InstructionID, in f: Function
  ) -> (subfield: RecordPath, storageInstruction: InstructionID) {
    var path: RecordPath = []
    var i = id
    while true {
      switch f[i] {
      case is AllocStack:
        return (path, i)
      case let s as SubfieldView:
        path.insert(contentsOf: s.subfield, at: 0)
        i = s.recordPlace.instruction!
      default:
        fatalError("Unexpected instruction in storage trace: \(f[i])")
      }
    }
  }
}

extension Module {

  /// Creates storage for `frame` in `f` and rewrites collected instructions to access it.
  ///
  /// Returns `nil` if no frame-backed values are required.
  mutating func materialize(
    _ frame: inout FrameMaterializationInfo, in f: Function.ID
  ) -> Operand? {
    // Get the storage instructions that need to be placed in the frame.
    // and the dictionary of their indices in the frame.
    var storageInstructions: [InstructionID] = []
    var indices: [InstructionID: Int] = [:]
    for i in frame.instructionInfo.values {
      indices[i.id] = storageInstructions.appendUnlessContained(i.storageInstruction)
    }

    if storageInstructions.isEmpty {
      // No instruction needs to be materialized, so no frame is required.
      return nil
    }

    // Put the frame at the beginning of the function.
    let t = Self.frameType(for: storageInstructions, in: self[f])
    let r = modifyIR(of: f, at: .start(of: self[f].entry!)) { (e) in
      e._alloc_stack(t)
    }

    // Replace the relevant instructions.
    for i in frame.instructionInfo.values {
      replaceInstruction(i, frame: r, index: indices[i.id]!, in: f)
    }
    return r
  }

  /// Returns the type of the frame for the given storage instructions.
  private static func frameType(for storageInstructions: [InstructionID], in f: Function)
    -> AnyType
  {
    var elements: [TupleType.Element] = []
    elements.append(
      contentsOf: storageInstructions.enumerated().lazy.map({ (index, i) -> TupleType.Element in
        TupleType.Element(label: "t\(index)", type: f[i].result!.ast)
      }))
    return AnyType(TupleType(elements))
  }

  /// Replaces instruction `i` with a subfield view into the `index`th field of `frame`.
  private mutating func replaceInstruction(
    _ i: InstructionMaterializationInfo, frame: Operand, index: Int, in f: Function.ID
  ) {
    // Add a SubfieldView before each use of `i`, to load the value from the frame.
    for u in self[f].allUses(of: i.id) {
      // For other instructions, add a SubfieldView, to access the value from the frame.
      let r = modifyIR(of: f, at: .before(u.user)) { (e) in
        e._subfield_view(frame, at: [index] + i.subfield)
      }
      self[f][u.user].replaceOperand(at: u.index, with: r)
    }
    self[f].uses[self[f].result(of: i.id)!]?.removeAll()
    self[f].remove(i.id)
  }

}
