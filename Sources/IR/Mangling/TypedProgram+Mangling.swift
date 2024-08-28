import FrontEnd

extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled<T: DeclID>(_ d: T) -> String {
    mangled(d, applying: { (s, m) in m.mangled(decl: s) })
  }

  /// Returns the mangled representation of `t`.
  public func mangled<T: TypeProtocol>(_ t: T) -> String {
    mangled(^t, applying: { (s, m) in m.mangled(type: s) })
  }

  /// Returns the mangled representation of `w`.
  public func mangled(_ w: WitnessTable) -> String {
    mangled(w, applying: { (s, m) in m.mangled(table: s) })
  }

  /// Returns the mangled representation of `f`.
  public func mangled(_ f: Function.ID) -> String {
    mangled(f, applying: { (s, m) in m.mangled(function: s) })
  }

  /// Returns the mangled representation of `s`, applying `mangle` to build it.
  private func mangled<T>(
    _ s: T, applying mangle: (T, inout Mangler) -> String
  ) -> String {
    var m = Mangler(self)
    return mangle(s, &m).assemblySanitized
  }

}
