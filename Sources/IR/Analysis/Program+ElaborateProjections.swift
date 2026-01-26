import FrontEnd

extension IR.Program {

  /// Elaborates all projections in `self` into ramp + slide functions.
  public mutating func elaborateProjections() {
    for m in modules.values {
      elaborateProjections(in: m.id)

    }
  }
  /// Elaborates the projections in `m` into a pair of ramp+slide functions.
  private mutating func elaborateProjections(in m: Module.ID) {
    for f in modules[m]!.functions.keys where modules[m]![f].isSubscript {
      guard !modules[m]![f].blockIDs.isEmpty else { continue }
      guard let p = modules[m]!.projectionSkeletons[f] else {
        fatalError("missing projection skeleton for projection function \(modules[m]!.debugDescription(f))")
      }
      elaborateProjection(f, skeleton: p, in: m)
    }
  }

  /// Elaborates the projection function `f` with skeleton `skeleton` in module `m`.
  private mutating func elaborateProjection(
    _ f: Function.ID, skeleton s: IR.ProjectionSkeleton, in m: Module.ID
  ) {
    generateRamp(for: f, skeleton: s, in: m)
    generateSlide(for: f, skeleton: s, in: m)
    // TODO: delete old function
  }

  /// Generate ramp function corresponding to `f`, with skeleton `s`, in `m`.
  private mutating func generateRamp(for f: Function.ID, skeleton s: ProjectionSkeleton, in m: Module.ID) {
    let r = modules[m]!.demandProjectionRampDeclaration(for: f)
    // TODO
    modules[m]!.generateEmptyBody(for: r, copying: f)
  }

  /// Generate slide function corresponding to `f`, with skeleton `s`, in `m`.
  private mutating func generateSlide(for f: Function.ID, skeleton s: ProjectionSkeleton, in m: Module.ID) {
    let s = modules[m]!.demandProjectionSlideDeclaration(for: f)
    // TODO
    modules[m]!.generateEmptyBody(for: s, copying: f)
  }

}
