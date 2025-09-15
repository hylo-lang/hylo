import Foundation
import FrontEnd
import Utils

extension Module {

  /// Analyzes projection in `f`, populating `projectionSkeletons` with the relevant information for expanding projections.
  mutating func analyzeProjection(
    _ f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if !self[f].isSubscript {
      return
    }

    if let p = try? self[f].analyzeProjection(id: f, reportingDiagnosticsTo: &log) {
      projectionSkeletons[f] = p
    }
  }

}

extension Function {

  /// Analyzes projection in `f`, raising diagnostics as needed and returning the projection skeleton.
  fileprivate func analyzeProjection(
    id: Function.ID,
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> ProjectionSkeleton {
    // Gather all the yield points in the projection.
    let yieldPoints: [InstructionID] = instructionIDs.filter({ self[$0] is Yield })
    let yieldBlocks = yieldPoints.map({ block(of: $0) })

    // Check that we have exactly one yield for each terminating path.
    let cfg = cfg()
    checkMultipleYields(yieldPoints, reportingDiagnosticsTo: &log)
    checkAllPaths(yieldBlocks, reportingDiagnosticsTo: &log)
    checkYieldsInLoops(yieldPoints, reportingDiagnosticsTo: &log)
    try log.throwOnError()

    // Find the blocks that need to be in the ramp, and the blocks that need to be in the slide.
    let slideBlocks = Array(
      yieldBlocks.lazy.flatMap({ cfg.successors(of: $0) }).uniqued()
    )
    let rampBlocks = Array(
      blockIDs
        .filter({
          !slideBlocks.contains($0) && !yieldBlocks.contains($0)
        })
        .uniqued()
    )

    // Partition the instructions in the yield blocks.
    var yieldBlockInstructions: [InstructionID: BlockSplit] = [:]
    for y in yieldPoints {
      yieldBlockInstructions[y] = splitBlock(at: y, consideringTail: canBeYieldTail)
    }

    return ProjectionSkeleton(
      id: id,
      yieldPoints: yieldPoints,
      rampBlocks: rampBlocks,
      slideBlocks: slideBlocks,
      yieldBlockInstructions: yieldBlockInstructions
    )
  }

  /// Returns `true` if the instruction `i` can be part of the yield tail,
  /// i.e., it appears after a yield, but still belong to the ramp of the projection.
  private func canBeYieldTail(_ i: InstructionID) -> Bool {
    let x = self[i]
    if let m = x as? MarkState {
      return !m.initialized
    }
    return x is EndAccess || x is DeallocStack
  }

  /// Checks that there are no multiple yields in the same path, and reports diagnostics to `log` if they are.
  private func checkMultipleYields(
    _ yieldPoints: [InstructionID], reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    // Check for multiple yields in the same path.
    for i in 0..<(yieldPoints.count - 1) {
      for j in (i + 1)..<yieldPoints.count {
        if precedesAcrossBlocks(yieldPoints[i], yieldPoints[j]) {
          log.insert(
            .multipleYields(
              at: site,
              firstYieldSite: self[yieldPoints[i]].site,
              secondYieldSite: self[yieldPoints[j]].site))
        } else if precedesAcrossBlocks(yieldPoints[j], yieldPoints[i]) {
          log.insert(
            .multipleYields(
              at: site,
              firstYieldSite: self[yieldPoints[j]].site,
              secondYieldSite: self[yieldPoints[i]].site))
        }
      }
    }
  }

  /// Checks that all the terminators are preceded by a yield.
  private func checkAllPaths(
    _ yieldBlocks: [Block.ID], reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    precondition(yieldBlocks.count > 0)
    let isYieldBlock = { (b: Block.ID) -> Bool in yieldBlocks.contains(b) }

    let cfg = cfg()
    let terminatesWithReturn = { (b: Block.ID) in
      let t = terminator(of: b)
      return t != nil && self[t!] is Return
    }
    for b in blockIDs.filter(terminatesWithReturn) {
      // If return point is the same block as a yield point, we are ok.
      if isYieldBlock(b) {
        continue
      }

      // If there is at least one path from the entry to the return point that doesn't contains a yield,
      // we have a problem.
      let paths = cfg.paths(to: b, from: entry!)
      for p in paths {
        if !p.contains(where: { isYieldBlock($0) }) {
          log.insert(.pathWithoutYield(at: self[terminator(of: b)!].site))
          break
        }
      }
    }
  }

  /// Checks that yields are not in loop constructs.
  private func checkYieldsInLoops(
    _ yieldPoints: [InstructionID], reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    precondition(yieldPoints.count > 0)

    let cfg = cfg()
    for y in yieldPoints {
      let b = block(of: y)
      // Compute the transitive predecessors of `b` in the CFG.
      var predecessors: [Block.ID] = []
      var toAdd = cfg.predecessors(of: b)
      while toAdd.count > 0 {
        predecessors.append(contentsOf: toAdd)
        let addingNow = toAdd
        toAdd = []
        for b1 in addingNow {
          for b2 in cfg.predecessors(of: b1) {
            if !predecessors.contains(b2) && !toAdd.contains(b2) {
              toAdd.append(b2)
            }
          }
        }
      }
      // If the predecessors contain the block itself, then we have a loop.
      if predecessors.contains(b) {
        log.insert(.yieldInLoop(at: self[y].site))
      }
    }
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

  fileprivate static func yieldInLoop(at site: SourceRange) -> Diagnostic {
    .error("yields cannot be part of loops", at: site)
  }

}
