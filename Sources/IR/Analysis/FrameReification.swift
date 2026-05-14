import OrderedCollections
import FrontEnd
import Utils

/// Tracks cross-region values that must be reified in a function frame and provides the metadata
/// required to rewrite their uses.
///
/// A "region", in this context, is a set of instructions (maybe from different blocks). For
/// example, the instructions from the beginning of a simple projection to the `yield` form a region
/// (the ramp), and the instructions from the `yield` to the end of the projection form another
/// region (the slide).
/// 
/// If one instruction from the slide of the projection uses the value of an instruction from the
/// ramp, the value must be reified in a frame that is passed from the ramp to the slide. The frame
/// reification is the process of detecting which values need to be reified into a frame and
/// performing the appropriate transformations, such as creating the frame storage and replacing
/// the relevant instructions to access the frame.
///
/// We say that an instruction is "frame-backed" if its value is stored in the frame.
struct FrameReification {

  /// Materialization metadata for instructions that must become frame-backed.
  fileprivate var instructionMetadata: [InstructionID: InstructionMaterialization] = [:]

  /// Collects instructions in `region` from `f` that must become frame-backed, excluding `ignored`.
  ///
  /// After this call, `instructionMetadata` contains all instructions from `region` that are used
  /// outside `region`, and all the instructions outside of `region` that are used by instructions
  /// in `region`.
  public mutating func collectCrossRegionInstructions(
    in f: Function,
    from region: some Sequence<InstructionID>,
    ignoring predicate: (InstructionID) -> Bool = { (_) in false }
  ) {
    let regionElements = Set(region)
    for i in region where !predicate(i) {
      // Check if this is used outside the region, and thus it must become frame-backed.
      if isUsedOutsideRegion(i, in: f, region: regionElements) {
        instructionMetadata.assignIfAbsent(forKey: i, InstructionMaterialization(source: i, in: f))
      }

      // Check if the defining instructions of operands must become frame-backed.
      for j in operandsOutsideOfRegion(i, in: f, region: regionElements) where !predicate(j) {
        instructionMetadata.assignIfAbsent(forKey: j, InstructionMaterialization(source: j, in: f))
      }
    }
  }

  /// Returns `true` if instruction `i` in `f` is used outside the region `r`.
  private func isUsedOutsideRegion(
    _ i: InstructionID, in f: Function, region r: Set<InstructionID>
  ) -> Bool {
    f.allUses(of: i).contains(where: { (u) in !r.contains(u.user) })
  }

  /// Returns the instructions outside `r` that define operands of `i` in `f`.
  private func operandsOutsideOfRegion(
    _ i: InstructionID, in f: Function, region r: Set<InstructionID>
  ) -> [InstructionID] {
    f[i].operands.compactMap({ (o) -> InstructionID? in
      switch o {
      case .register(let def):
        return r.contains(def) ? nil : def
      case .parameter:
        // TODO: implement this
        return nil
      case .constant:
        return nil
      }
    })
  }

}

/// Materialization metadata for a value that crosses region boundaries.
///
/// If instruction `j` uses instruction `i` and they are in different regions,
/// the storage associated with `i` gets materialized in the frame.
///
/// Includes the subfield path to get to the storage of `i` from the frame, and the instruction
/// that allocates the storage.
private struct InstructionMaterialization {

  /// The ID of the instruction that must be materialized in the frame.
  fileprivate let source: InstructionID

  /// The subfield path to get to the storage of `id` from the frame, if any.
  fileprivate let subfield: RecordPath

  /// The instruction that allocates the storage.
  fileprivate var storageInstruction: InstructionID

  /// Creates metadata for materializing `id` in the frame.
  public init(source: InstructionID, in f: Function) {
    self.source = source
    (self.subfield, self.storageInstruction) = Self.storagePath(of: source, in: f)
  }

  /// Returns the subfield path and the `AllocStack` instruction at the base of the storage chain
  /// rooted at `source` in `f`.
  ///
  /// At the moment, we only support storage chains containing `SubfieldView` and `AllocStack`
  /// instructions.
  private static func storagePath(
    of source: InstructionID, in f: Function
  ) -> (subfield: RecordPath, storageInstruction: InstructionID) {
    var path: RecordPath = []
    var i = source
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
    _ frame: inout FrameReification, in f: Function.ID
  ) -> Operand? {
    // Get the storage instructions that need to be placed in the frame and the dictionary of their
    // indices in the frame.
    var storageInstructions = OrderedSet<InstructionID>()
    var indices: [InstructionID: Int] = [:]
    for i in frame.instructionMetadata.values {
       (_, indices[i.source]) = storageInstructions.append(i.storageInstruction)
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
    for i in frame.instructionMetadata.values {
      replaceInstruction(i, frame: r, index: indices[i.source]!, in: f)
    }
    return r
  }

  /// Returns the type of the frame needed to store `storageInstructions`.
  private static func frameType(
    for storageInstructions: OrderedSet<InstructionID>, in f: Function
  ) -> AnyType {
    var elements: [TupleType.Element] = []
    elements.append(
      contentsOf: storageInstructions.enumerated().lazy.map({ (index, i) -> TupleType.Element in
        TupleType.Element(label: "t\(index)", type: f[i].result!.ast)
      }))
    return AnyType(TupleType(elements))
  }

  /// Replaces instruction `i` with a subfield view into the `index`th field of `frame`.
  private mutating func replaceInstruction(
    _ i: InstructionMaterialization, frame: Operand, index: Int, in f: Function.ID
  ) {
    // Add a SubfieldView before each use of `i`, to load the value from the frame.
    for u in self[f].allUses(of: i.source) {
      let r = modifyIR(of: f, at: .before(u.user)) { (e) in
        e._subfield_view(frame, at: [index] + i.subfield)
      }
      self[f][u.user].replaceOperand(at: u.index, with: r)
    }
    self[f].uses[self[f].result(of: i.source)!]?.removeAll()
    self[f].remove(i.source)
  }

}
