import FrontEnd
import Driver
import IR
import Utils

extension AST {

  /// A module within an AST.
  public typealias Module = Module_<Self>

}

extension SourceFile {

  /// Returns `self`, incorporated into `p` as its `Main` module.
  ///
  /// - Parameter needsBuiltins: whether `self` should be allowed access to
  ///   builtin functions.
  public func parsedAsMain(
    into p: inout AST,
    withBuiltinModuleAccess needsBuiltins: Bool = false,
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> ModuleDecl.ID {
    try p.loadModule("Main", parsing: [self], withBuiltinModuleAccess: needsBuiltins, reportingDiagnosticsTo: &log)
  }

  /// Returns `self` parsed as the `Main` module.
  ///
  /// - Parameter needsBuiltins: whether `self` should be allowed access to
  ///   builtin functions.
  public func parsedAsMain(
    withBuiltinModuleAccess needsBuiltins: Bool = false,
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws -> AST.Module {
    var target = AST()
    let m = try self.parsedAsMain(into: &target, withBuiltinModuleAccess: needsBuiltins, reportingDiagnosticsTo: &log)
    return .init(program: target, module: m)
  }

  /// Returns `self` as a one-file module and the hosted standard library,
  /// typechecked.
  ///
  /// The module for `self` is the result's `latestModule`.
  ///
  /// - Parameter `needsBuiltins`: `true` iff `self` should be allowed access to
  ///   builtin functions.
  public func loweredToIR(withBuiltinModuleAccess needsBuiltins: Bool = false) throws -> IR.Module {
    var log = DiagnosticSet()
    return try self.typecheckedWithStandardLibrary(reportingDiagnosticsTo: &log, withBuiltinModuleAccess: needsBuiltins)
      .loweredToIR(reportingDiagnosticsTo: &log)
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
  ) throws -> TypedProgram.Module {
    log.formUnion(typecheckedStandardLibrary.diagnostics)
    return try typecheckedStandardLibrary.program
      .loadModule(reportingDiagnosticsTo: &log) { ast, log, nodeSpace in
        try self.parsedAsMain(
          into: &ast, withBuiltinModuleAccess: needsBuiltins,
          reportingDiagnosticsTo: &log
        )
      }
  }

}

extension TypedProgram.Module {

  /// Returns self, lowered to IR.
  public func loweredToIR(reportingDiagnosticsTo log: inout DiagnosticSet) throws -> IR.Module {
    var r = try IR.Module(lowering: module, in: self.program, reportingDiagnosticsTo: &log)
    try r.applyMandatoryPasses(reportingDiagnosticsTo: &log)
    return r
  }

}

extension TypedProgram {

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
