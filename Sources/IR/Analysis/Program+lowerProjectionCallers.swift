extension IR.Program {

  /// Lowers all functions in `self` that call projections to use regular function calls.
  public mutating func lowerProjectionCallers() {
    for m in modules.values {
      lowerProjectionCallers(in: m.id)
    }
  }

  /// Lowers all functions in `m` that call projections to use regular function calls.
  private mutating func lowerProjectionCallers(in m: Module.ID) {
    for f in modules[m]!.functions.keys {
      guard modules[m]![f].instructions.contains(where: { $0 is Project }) else { continue }
      lowerProjectionCaller(f, in: m)
    }
  }

  /// Lowers the projection caller function `f` in module `m`.
  private mutating func lowerProjectionCaller(_ f: Function.ID, in m: Module.ID) {
    // TODO: properly implement this
    let p = modules[m]!.demandCallerPlateauDeclaration(for: f, region: 0)
    modules[m]!.generateEmptyBody(for: p, copying: f)
  }

}
