extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled(_ d: AnyDeclID) -> String {
    var output = ""
    var m = Mangler<String>()
    m.mangle(d, of: self, to: &output)
    return output.assemblySanitized
  }

  /// Decodes and returns the symbol represented by the mangled string `s`, returning `nil` if
  /// decoding failed.
  public func demangle(_ s: String) -> DemangledSymbol? {
    guard let i = String(assemblySanitized: s) else { return nil }
    var m = Demangler(program: self)
    var x = i[...]
    return m.demangle(from: &x)
  }

}
