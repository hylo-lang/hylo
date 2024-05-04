import FrontEnd

extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled<T: DeclID>(_ d: T) -> String {
    mangled(d, applying: { (s, m, o) in m.mangle(decl: s, to: &o) })
  }

  /// Returns the mangled representation of `t`.
  public func mangled<T: TypeProtocol>(_ t: T) -> String {
    mangled(^t, applying: { (s, m, o) in m.mangle(type: s, to: &o) })
  }

  /// Returns the mangled representation of `w`.
  public func mangled(_ w: WitnessTable) -> String {
    mangled(w, applying: { (s, m, o) in m.mangle(table: s, to: &o) })
  }

  /// Returns the mangled representation of `f`.
  public func mangled(_ f: Function.ID) -> String {
    mangled(f, applying: { (s, m, o) in m.mangle(function: s, to: &o) })
  }

  /// Returns the mangled representation of `symbol` in `scopeOfuse`, applying `mangle` to compute
  /// that representation.
  private func mangled<T>(
    _ symbol: T, applying mangle: (T, inout Mangler, inout String) -> Void
  ) -> String {
    var output = ""
    var m = Mangler(self, in: AnyScopeID(ast.coreLibrary!))  // FIXME: use actual scope
    mangle(symbol, &m, &output)
    return output.assemblySanitized
  }

}
