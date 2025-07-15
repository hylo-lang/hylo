import FrontEnd

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
  public let yieldBlockInstructions: [InstructionID: YieldBlockInstructions]

  /// The AllocStack instructions in the ramp part that are used in the slide part.
  public let transYieldAllocas: [InstructionID]

  /// The type of the extra parameter that contains the state to be shared between the ramp and the slide.
  public let extraParameterType: TupleType?

  /// The instructions in a yield block, partitioned in three groups: instructions before the yield point,
  /// instructions after the yield point that locally belong to the ramp, and instructions after the yield point.
  internal struct YieldBlockInstructions {

    /// The instructions before the yield point.
    public let beforeYield: [InstructionID]

    /// The instructions after the yield point that locally belong to the ramp.
    public let yieldTail: [InstructionID]

    /// The instructions after the yield point that are part of the slide.
    public let afterYield: [InstructionID]

  }

  /// Analyzes the projection `p` in module `m`, collecting relevant information for the expansion pass,
  /// performing correctness checks and reporting errors to `log`.
  public init(
    _ p: Function.ID, in m: Module, reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws {
    self.id = p

    // Gather all the yield points in the projection.
    let yieldPoints: [InstructionID] = Array(
      m.blocks(in: p)
        .flatMap({ m.instructions(in: $0) })
        .filter({ m[$0] is Yield }))
    self.yieldPoints = yieldPoints

    // Check that we have exactly one yield for each terminating path.
    let cfg = m.functions[p]!.cfg()
    Self.checkMultipleYields(yieldPoints, in: m, reportingDiagnosticsTo: &log)
    Self.checkAllPaths(yieldPoints, in: m, reportingDiagnosticsTo: &log)
    Self.checkYieldsInLoops(yieldPoints, in: m, reportingDiagnosticsTo: &log)
    try log.throwOnError()

    // Find the blocks that need to be in the ramp, and the blocks that need to be in the slide.
    let slideBlocks = Array(
      yieldPoints.lazy
        .flatMap({ cfg.successors(of: $0.block) })
        .map({ Block.ID(p, $0) }))
    let rampBlocks = Array(
      m.blocks(in: p)
        .filter({
          !slideBlocks.contains($0) && !yieldPoints.map({ Block.ID(p, $0.block) }).contains($0)
        }))
    self.rampBlocks = rampBlocks
    self.slideBlocks = slideBlocks

    // Partition the instructions in the yield blocks.
    var yieldBlockInstructions: [InstructionID: YieldBlockInstructions] = [:]
    for y in yieldPoints {
      yieldBlockInstructions[y] = Self.splitBlockAt(yield: y, in: m)
    }
    self.yieldBlockInstructions = yieldBlockInstructions

    // TODO: some of the end_access + dealloc_stack instructions can be moved before the yield point.

    // Determine the AllocStack instructions in the ramp part.
    var rampAllocas: [InstructionID] = Array(
      rampBlocks.lazy.flatMap({ m.instructions(in: $0) })
        .filter({ m[$0] is AllocStack }))
    rampAllocas.append(
      contentsOf: yieldPoints.lazy.flatMap({ yieldBlockInstructions[$0]!.beforeYield })
        .filter({ m[$0] is AllocStack })
    )

    // Determine which of these allocas are also used in the slide part.
    let transYieldAllocas = rampAllocas.filter({ (a) in
      m.allUses(of: a).contains { u in
        Self.belongsInSlide(u.user, rampBlocks, yieldBlockInstructions)
      }
    })
    self.transYieldAllocas = transYieldAllocas

    // Build the tuple type of the state parameter.
    var tupleElements: [TupleType.Element] = []
    if yieldPoints.count > 1 {
      tupleElements.append(
        TupleType.Element(label: "i", type: AnyType(m.program.ast.coreType("Int32")!)))
    }
    tupleElements.append(
      contentsOf: self.transYieldAllocas.enumerated().map({ (index, a) -> TupleType.Element in
        TupleType.Element(label: "t\(index)", type: m[a].result!.ast)
      }))
    self.extraParameterType = tupleElements.isEmpty ? nil : TupleType(tupleElements)
  }

  /// Split the block of `yield` into three parts, consisting of:
  /// - the instructions before the yield point,
  /// - the instructions after the yield point that locally belong to the ramp,
  /// - the instructions after the yield point that are part of the slide.
  private static func splitBlockAt(yield: InstructionID, in m: Module) -> YieldBlockInstructions {
    let block = m[Block.ID(yield.function, yield.block)]
    let all = m[Block.ID(yield.function, yield.block)].instructions.addresses

    let yieldIndex = all.firstIndex(of: yield.address)
    let remaining = all.suffix(from: yieldIndex!).dropFirst()
    let tailEndIndex = remaining.firstIndex(where: { !Self.canBeYieldTail(block.instructions[$0]) })
    // At least the block terminator must be outside the tail, so `tailEndIndex` is never `nil`.
    return YieldBlockInstructions(
      beforeYield: all.prefix(upTo: yieldIndex!)
        .map({ InstructionID(yield.function, yield.block, $0) }),
      yieldTail: all.suffix(from: yieldIndex!).prefix(upTo: tailEndIndex!)
        .map({ InstructionID(yield.function, yield.block, $0) }),
      afterYield: all.suffix(from: tailEndIndex!)
        .map({ InstructionID(yield.function, yield.block, $0) })
    )
  }

  /// Returns `true` if the instruction `i` can be part of the yield tail,
  /// i.e., it appears after a yield, but still belong to the ramp of the projection.
  private static func canBeYieldTail(_ i: Instruction) -> Bool {
    if let msi = i as? MarkState {
      return !msi.initialized
    }
    return i is EndAccess || i is DeallocStack
  }

  /// Returns `true` if `i` belongs to one of the slide blocks `slideBlocks`,
  /// or is part of `afterYield` instructions of a yield block defined by `yieldBlockInstructions`.
  private static func belongsInSlide(
    _ i: InstructionID, _ slideBlocks: [Block.ID],
    _ yieldBlockInstructions: [InstructionID: YieldBlockInstructions]
  ) -> Bool {
    slideBlocks.contains(Block.ID(i.function, i.block))
      || yieldBlockInstructions.keys.contains { y in
        yieldBlockInstructions[y]!.afterYield.contains(i)
      }
  }

  /// Checks that there are no multiple yields in the same path, and reports diagnostics to `log` if they are.
  private static func checkMultipleYields(
    _ yieldPoints: [InstructionID], in m: Module, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    // Check for multiple yields in the same path.
    for i in 0..<(yieldPoints.count - 1) {
      for j in (i + 1)..<yieldPoints.count {
        if m.dominates(yieldPoints[i], yieldPoints[j]) {
          log.insert(
            .multipleYields(
              at: m[yieldPoints[i].function].site,
              firstYieldSite: m[yieldPoints[i]].site,
              secondYieldSite: m[yieldPoints[j]].site))
        } else if m.dominates(yieldPoints[j], yieldPoints[i]) {
          log.insert(
            .multipleYields(
              at: m[yieldPoints[j].function].site,
              firstYieldSite: m[yieldPoints[j]].site,
              secondYieldSite: m[yieldPoints[i]].site))
        }
      }
    }
  }

  /// Checks that all the terminators are dominates by an yield.
  private static func checkAllPaths(
    _ yieldPoints: [InstructionID], in m: Module, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    precondition(yieldPoints.count > 0)
    let f = yieldPoints[0].function
    let returnPoints = m.blocks(in: f)
      .map({ m.terminator(of: $0) })
      .filter({ $0 != nil && m[$0!] is Return })
      .map({ $0! })

    let containsYield = { b -> Bool in
      yieldPoints.contains(where: { $0.block == b })
    }

    let cfg = m[f].cfg()
    for r in returnPoints {
      // If return point is the same block as a yield point, we are ok.
      if containsYield(r.block) {
        continue
      }

      // If there is at least one path from the entry to the return point that doesn't contains a yield,
      // we have a problem.
      let paths = cfg.paths(to: r.block, from: m.entry(of: f)!.address)
      for p in paths {
        if !p.contains(where: { containsYield($0) }) {
          log.insert(.pathWithoutYield(at: m[r].site))
          break
        }
      }
    }
  }

  /// Checks that yields are not in loop constructs.
  private static func checkYieldsInLoops(
    _ yieldPoints: [InstructionID], in m: Module, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    precondition(yieldPoints.count > 0)

    let cfg = m[yieldPoints[0].function].cfg()
    for y in yieldPoints {
      let b = y.block
      // Compute the transitive predecessors of `b` in the CFG.
      var predecessors: [Function.Blocks.Address] = []
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
        log.insert(.yieldInLoop(at: m[y].site))
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
