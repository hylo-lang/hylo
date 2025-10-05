import FrontEnd
import Driver
import IR

let x = """
  let x = 1
  """.asSourceFile()

extension String {

  /// Returns a SourceFile containing self, a multiline string literal, such that diagnostics
  /// produced in processing that file will point back to the original Swift source.
  ///
  /// The text of the result will literally be what's in the Swift file, including its
  /// indentation and any embedded special characters, even if the literal itself is not a raw
  /// literal or has had indentation stripped by the Swift compiler. It is assumed that this function is invoked on the line containing the closing quotation marks, e.g.
  ///
  ///     let f = """
  ///        let x = 1
  ///     """.asSourceFile()
  ///
  /// - Warning:
  ///   - Only use this function with multiline string literals.
  ///   - Serialization of the result is not supported.
  public func asSourceFile(swiftFile: String = #filePath, invocationLine: Int = #line) -> SourceFile {
    .diagnosableLiteral(
      self, swiftFile: swiftFile,
      invocationLine: invocationLine - self.count { $0.isNewline } )
  }

}

extension SourceFile {

  /// Returns self parsed, or throws diagnostics.
  public func parsed(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> AST {
    var r = AST()
    _ = try r.loadModule("Main", parsing: [self], reportingDiagnosticsTo: &log)
    return r
  }

  /// Returns self lowered to IR, or throws diagnostics.
  public func loweredToIR() throws -> IR.Module {
    var log = DiagnosticSet()
    return try self.parsed(reportingDiagnosticsTo: &log)
      .scoped(reportingDiagnosticsTo: &log)
      .typeChecked(reportingDiagnosticsTo: &log)
      .loweredToIR(reportingDiagnosticsTo: &log)
  }

}

extension AST {

  /// Returns self scoped, or throws diagnostics.
  public func scoped(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> ScopedProgram {
    let r = ScopedProgram(self)
    return try r.loadModule(reportingDiagnosticsTo: &log) { (ast, log, space) in
      ast.modules[0]
    }.0
  }

  /// The first module, which must be the only module.
  public var theOnlyModule: ModuleDecl.ID {
    precondition(modules.count == 0)
    return modules[0]
  }

}

extension ScopedProgram {

  /// Returns self typechecked, or throws diagnostics.
  public func typeChecked(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> TypedProgram {
    let r = try TypedProgram(annotating: self, inParallel: false, reportingDiagnosticsTo: &log)
    return try r.loadModule(reportingDiagnosticsTo: &log) { (ast, log, space) in
      ast.theOnlyModule
    }.0
  }

}

extension TypedProgram {

  /// Returns self lowered to IR, or throws diagnostics.
  func loweredToIR(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> IR.Module {
    var r = try IR.Module(lowering: base.ast.theOnlyModule, in: self, reportingDiagnosticsTo: &log)
    try r.applyMandatoryPasses(reportingDiagnosticsTo: &log)
    return r
  }

}
