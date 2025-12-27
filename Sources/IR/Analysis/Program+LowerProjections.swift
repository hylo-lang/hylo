extension IR.Program {

  /// Lower all projections in `self` into ramp + slide functions.
  public mutating func lowerProjections() {
    for m in modules.values {
      lowerProjections(in: m)
    }
  }

  /// Lowers the projections in `m` into a pair of ramp+slide functions.
  private mutating func lowerProjections(in m: Module) {
    for f in m.functions.keys where m[f].isSubscript {
      guard !m[f].blockIDs.isEmpty else { continue }
      guard let p = m.projectionSkeletons[f] else {
        fatalError("missing projection skeleton for projection function \(m.debugDescription(f))")
      }
      lowerProjection(f, skeleton: p, in: m)
    }
  }

  /// Lowers the projection function `f` with skeleton `skeleton` in module `m`.
  private mutating func lowerProjection(
    _ f: Function.ID, skeleton: IR.ProjectionSkeleton, in m: Module
  ) {
    // print("Lowering projection @\(m[f].site):  \(m.debugDescription(f))")
    // TODO: generate ramp
    // TODO: generate slide
    // TODO: delete old function
  }

}
