import FrontEnd

extension Module {

  /// Check if projection `f` is well-formed, raising diagnostics to `log` as needed, and populates `projectionSkeletons` with
  /// the relevant information for expanding projections.
  ///
  /// A well formed projection must have exactly one yield on each path through the function.
  mutating func analyzeProjection(
    _ f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if !self[f].isSubscript {
      return
    }

    if let p = try? self[f].analyzeProjection(reportingDiagnosticsTo: &log) {
      projectionSkeletons[f] = p
    }
  }

}

extension Function {

  /// Check if projection `self` is well-formed, raising diagnostics to `log` as needed,
  /// and returns the relevant information for expanding projections.
  fileprivate func analyzeProjection(
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> ProjectionSkeleton {
    // Gather all the yield points in the projection.
    let yieldPoints: [InstructionID] = instructionIdentities.filter({ self[$0] is Yield })
    let yieldBlocks = yieldPoints.map({ block(of: $0) })

    // Check that there aren't two yields in the same block.
    // In that case, we would get two yield blocks one after the other.
    if yieldBlocks.count >= 2 {
      for i in 1..<yieldBlocks.count {
        if yieldBlocks[i] == yieldBlocks[i - 1] {
          log.insert(
            .multipleYields(
              at: site,
              firstYieldSite: self[yieldPoints[i - 1]].site,
              secondYieldSite: self[yieldPoints[i]].site))
        }
      }
    }

    // From now on, we can perform all the checks on blocks.
    let cfg = cfg()

    // Find the ramp and slide blocks.
    let rampBlocks = Set(yieldBlocks.lazy.flatMap({ cfg.predecessors(of: $0) }))
    let slideBlocks = Set(yieldBlocks.lazy.flatMap({ cfg.successors(of: $0) }))

    // If we have any blocks that are not in either the ramp or the slide, the projection is ill-formed.
    let rest = Set(blockIDs).subtracting(slideBlocks).subtracting(rampBlocks).subtracting(yieldBlocks)
    if !rest.isEmpty {
      let b = lastBlock(start: rest.first!, cfg: cfg)
      log.insert(.pathWithoutYield(at: self[terminator(of: b)!].site))
      try log.throwOnError()
    }

    // If we have any yields on the slide blocks, the projection is ill-formed.
    let slidesWithYields = slideBlocks.intersection(yieldBlocks)
    if !slidesWithYields.isEmpty {
      // We have slide blocks that contain yields.
      // Reconstruct the yield information and report diagnostics.
      let b2 = slidesWithYields.first!
      let b1 = Set(yieldBlocks).intersection(cfg.predecessors(of: b2)).first!

      log.insert(
        .multipleYields(
          at: site,
          firstYieldSite: self[yieldForBlock(b1)!].site,
          secondYieldSite: self[yieldForBlock(b2)!].site))
      try log.throwOnError()
    }

    return ProjectionSkeleton(
      yieldPoints: yieldPoints,
      rampBlocks: Array(rampBlocks),
      slideBlocks: Array(slideBlocks),
    )
  }

  /// Returns the last block reachable from `b` by following the first successor.
  private func lastBlock(start b: Block.ID, cfg: ControlFlowGraph) -> Block.ID {
    var current = b
    var visited: Set<Block.ID> = []
    while true {
      visited.insert(current)
      let ss = Set(cfg.successors(of: current)).subtracting(visited)
      if ss.isEmpty {
        return current
      }
      current = ss.first!
    }
  }

  /// Returns the first yield instruction in block `b`.
  private func yieldForBlock(_ b: Block.ID) -> InstructionID? {
    instructions(in: b).first(where: { (s) in self[s] is Yield })
  }

}

extension Diagnostic {

  fileprivate static func multipleYields(
    at site: SourceRange, firstYieldSite: SourceRange, secondYieldSite: SourceRange
  ) -> Diagnostic {
    .error(
      "multiple yields on the same path", at: site,
      notes: [
        .note("see first yield", at: firstYieldSite),
        .note("and second yield", at: secondYieldSite),
      ])
  }

  fileprivate static func pathWithoutYield(at site: SourceRange) -> Diagnostic {
    .error("not all paths of the projection contain a yield", at: site)
  }

}
