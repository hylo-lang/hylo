extension IR.Program {

  /// Lowers all functions in `self` that call projections to use regular function calls.
  public mutating func lowerProjectionCallers() {
    for m in modules.values {
      lowerProjectionCallers(in: m)
    }
  }

  /// Lowers all functions in `m` that call projections to use regular function calls.
  private mutating func lowerProjectionCallers(in m: Module) {
    for f in m.functions.keys {
      guard m[f].instructions.contains(where: { $0 is Project }) else { continue }
      lowerProjectionCaller(f, in: m)
    }
  }

  /// Lowers the projection caller function `f` in module `m`.
  private mutating func lowerProjectionCaller(_ f: Function.ID, in m: Module) {
    // print("Lowering projection caller @\(m[f].site):  \(m.debugDescription(f))")
    // TODO
  }

}
