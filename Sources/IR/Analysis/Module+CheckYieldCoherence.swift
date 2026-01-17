import FrontEnd

extension Module {

  /// Check if projection `f` is well-formed, raising diagnostics to `log` as needed, and populates `projectionSkeletons` with
  /// the relevant information for expanding projections.
  ///
  /// A well formed projection must have exactly one yield on each path through the function.
  mutating func checkYieldCoherence(
    _ f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    // Nothing to do if `f` is not a subscript.
    if !self[f].isSubscript { return }

    if let p = try? self[f].checkYieldCoherence(reportingDiagnosticsTo: &log) {
      projectionSkeletons[f] = p
    }
  }

}

extension Function {

  /// Check if projection `self` is well-formed, raising diagnostics to `log` as needed,
  /// and returns the relevant information for expanding projections.
  fileprivate func checkYieldCoherence(
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> ProjectionSkeleton {
    // Gather all the yield points in the projection.
    let yieldPoints: [InstructionID] = instructionIdentities.filter({ self[$0] is Yield })
    let yieldBlocks = yieldPoints.map({ block(of: $0) })

    // Check that there aren't two yields in the same block.
    // In that case, we would get two yield blocks one after the other.
    if yieldBlocks.count >= 2 {
      for i in 1 ..< yieldBlocks.count {
        if yieldBlocks[i] == yieldBlocks[i - 1] {
          log.insert(
            .multipleYields(
              at: self[yieldPoints[i]].site,
              previousYieldSite: self[yieldPoints[i - 1]].site))
        }
      }
    }

    // From now on, we can perform all the checks on blocks.
    let cfg = cfg()

    // The slide blocks (every block reachable from a yield block).
    var slide: [Block.ID] = []
    // The ramp blocks (every block reachable before a yield block).
    var ramp: [Block.ID] = []

    // Phase 1: Fully explore the ramp blocks.
    // Check for any paths that don't contain yields.
    // Also compute the direct successors for all encountered yield blocks.
    cfg.withBFS([entry!]) { (b, successors) in
      // Have we encountered a yield block?
      if yieldBlocks.contains(b) {
        // Move the direct successors to the slide.
        for s in successors { slide.appendUnlessContained(s) }
        // Don't follow this path any further.
        return .skip
      }

      // If this is not a yield block, then it's part of the ramp (we don't explore past yield blocks).
      ramp.appendUnlessContained(b)

      // If we reached the end of the exploration, this is a terminal ramp block.
      // This means that there is a path without yields.
      if successors.isEmpty {
        log.insert(.pathWithoutYield(at: self[terminator(of: b)!].site))
        return .stop
      }
      return .continue
    }
    try log.throwOnError()

    // Phase 2: Fully explore the slide.
    // Make sure that the slide blocks don't contain yields.
    cfg.withBFS(slide) { (b, successors) in
      slide.appendUnlessContained(b)
      // Make sure there isn't another yield in the block.
      if yieldBlocks.contains(b) {
        // We have slide blocks that contain yields.
        // Reconstruct the yield information and report diagnostics.
        let site = self[yieldForBlock(b)!].site
        let previousYieldBlock = yieldBlockForSlide(b, cfg: cfg, yieldBlocks: yieldBlocks)!
        let previousSite = self[yieldForBlock(previousYieldBlock)!].site

        log.insert(.multipleYields(at: site, previousYieldSite: previousSite))
        return .stop
      }

      return .continue
    }
    try log.throwOnError()

    return ProjectionSkeleton(
      yieldPoints: yieldPoints,
      rampBlocks: ramp,
      slideBlocks: slide)
  }

  /// Returns the block containing a yield that has `b` as a slide block, if any.
  ///
  /// - Complexity: O(n) in the number of blocks in the CFG.
  private func yieldBlockForSlide(_ b: Block.ID, cfg: ControlFlowGraph, yieldBlocks: [Block.ID]) -> Block.ID? {
    cfg.iterateFrom([b], forward: false).first(where: { yieldBlocks.contains($0) })
  }

  /// Returns the first yield instruction in block `b`.
  private func yieldForBlock(_ b: Block.ID) -> InstructionID? {
    instructions(in: b).first(where: { (s) in self[s] is Yield })
  }

}

extension Array where Element : Equatable {

    /// Appends `element` to `self` iff it is not already contained in `self`.
    ///
    /// - Complexity: O(n) where n is the length of `self`.
    fileprivate mutating func appendUnlessContained(_ element: Element) {
    if !self.contains(element) {
      self.append(element)
    }

  }
}

extension Diagnostic {

  fileprivate static func multipleYields(at site: SourceRange, previousYieldSite: SourceRange) -> Diagnostic {
    .error(
      "multiple yields on the same path", at: site,
      notes: [.note("see previous yield", at: previousYieldSite)])
  }

  fileprivate static func pathWithoutYield(at site: SourceRange) -> Diagnostic {
    .error("not all paths of the projection contain a yield", at: site)
  }

}
