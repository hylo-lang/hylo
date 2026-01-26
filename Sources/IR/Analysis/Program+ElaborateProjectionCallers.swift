extension IR.Program {

  /// Elaborates all functions in `self` that call projections to use regular function calls.
  public mutating func elaborateProjectionCallers() {
    for m in modules.values {
      elaborateProjectionCallers(in: m.id)
    }
  }

  /// Elaborates all functions in `m` that call projections to use regular function calls.
  private mutating func elaborateProjectionCallers(in m: Module.ID) {
    for f in modules[m]!.functions.keys {
      guard modules[m]![f].instructions.contains(where: { $0 is Project }) else { continue }
      elaborateProjectionCaller(f, in: m)
    }
  }

  /// Elaborates the projection caller function `f` in module `m`.
  private mutating func elaborateProjectionCaller(_ f: Function.ID, in m: Module.ID) {
    // TODO: properly implement this
    let p = modules[m]!.demandCallerPlateauDeclaration(for: f, region: 0)
    modules[m]!.generateEmptyBody(for: p, copying: f)
  }

}
