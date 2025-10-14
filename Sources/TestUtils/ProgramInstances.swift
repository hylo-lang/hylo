import FrontEnd
import Driver
import IR
import Utils

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
      invocationLine: invocationLine - self.count { $0.isNewline } - 3)
  }

}

extension SourceFile {

  /// Returns self parsed.
  public func parsed(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> AST {
    var r = AST()
    _ = try r.loadModule("Main", parsing: [self], reportingDiagnosticsTo: &log)
    return r
  }

  /// Returns `self` as a one-file module, lowered to IR.
  public func loweredToIR() throws -> IR.Module {
    var log = DiagnosticSet()
    return try self.typecheckedWithStandardLibrary(reportingDiagnosticsTo: &log)
      .latestModuleIR(reportingDiagnosticsTo: &log)
  }

  /// Returns `self` as a one-file module and the standard library,
  /// typechecked.
  ///
  /// The module for `self` is the result's `latestModule`.
  public func typecheckedWithStandardLibrary(
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> TypedProgram {
    log.formUnion(typecheckedStandardLibrary.diagnostics)
    return try typecheckedStandardLibrary.program
      .loadModule(reportingDiagnosticsTo: &log) { ast, log, nodeSpace in
        try ast.loadModule("Main", parsing: [self], reportingDiagnosticsTo: &log)
      }.0
  }

}

extension AST {

  /// The latest module loaded.
  public var latestModule: ModuleDecl.ID {
    return modules.last!
  }

}

extension TypedProgram {

  /// Returns the last-loaded module lowered to IR, or throws diagnostics.
  func latestModuleIR(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> IR.Module {
    var r = try IR.Module(lowering: base.ast.latestModule, in: self, reportingDiagnosticsTo: &log)
    try r.applyMandatoryPasses(reportingDiagnosticsTo: &log)
    return r
  }

  /// An instance with no modules.
  static let empty = makeEmpty()

  /// Returns an empty instance.
  private static func makeEmpty() -> Self {
    var log = DiagnosticSet()
    do {
      return try TypedProgram(annotating: ScopedProgram(AST()), reportingDiagnosticsTo: &log)
    }
    catch let e {
      fatalError("Typechecking an empty program failed with: \(e)")
    }
  }

}
