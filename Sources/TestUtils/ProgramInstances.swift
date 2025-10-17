import FrontEnd
import Driver
import IR
import Utils

extension SourceFile {

  /// Returns self parsed.
  public func parsed(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> AST {
    var r = AST()
    _ = try r.loadModule("Main", parsing: [self], reportingDiagnosticsTo: &log)
    return r
  }

  /// Returns `self` as a one-file module, lowered to IR.
  ///
  /// - Parameter `needsBuiltins`: `true` iff `self` should be allowed access to
  ///   builtin functions.
  public func loweredToIR(withBuiltinModuleAccess needsBuiltins: Bool = false) throws -> IR.Module {
    var log = DiagnosticSet()
    return try self.typecheckedWithStandardLibrary(reportingDiagnosticsTo: &log, withBuiltinModuleAccess: needsBuiltins)
      .latestModuleIR(reportingDiagnosticsTo: &log)
  }

  /// Returns `self` as a one-file module and the standard library,
  /// typechecked.
  ///
  /// The module for `self` is the result's `latestModule`.
  ///
  /// - Parameter `needsBuiltins`: `true` iff `self` should be allowed access to
  ///   builtin functions.
  public func typecheckedWithStandardLibrary(
    reportingDiagnosticsTo log: inout DiagnosticSet,
    withBuiltinModuleAccess needsBuiltins: Bool = false
  ) throws -> TypedProgram {
    log.formUnion(typecheckedStandardLibrary.diagnostics)
    return try typecheckedStandardLibrary.program
      .loadModule(reportingDiagnosticsTo: &log) { ast, log, nodeSpace in
        try ast.loadModule("Main", parsing: [self], withBuiltinModuleAccess: needsBuiltins, reportingDiagnosticsTo: &log)
      }.program
  }

}

extension AST {

  /// The latest module loaded.
  public var latestModule: ModuleDecl.ID { modules.last! }

}

extension TypedProgram {

  public var latestModule: ModuleDecl.ID { base.ast.latestModule }

  /// Returns the last-loaded module, lowered to IR.
  public func latestModuleIR(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> IR.Module {
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
