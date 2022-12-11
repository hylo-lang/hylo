/// A C++ scoped block -- multiple statements in curly braces
struct CXXScopedBlock: CXXRepresentable {

  public let stmts: [CXXRepresentable]

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("{\n")
    for stmt in stmts { stmt.writeCode(into: &target) }
    target.write("}\n")
  }

}
