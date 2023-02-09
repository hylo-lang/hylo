import Core
import Utils
import ValModule

extension AST {

  /// Imports the core library into `self`.
  ///
  /// - Requires: The Core library must not have been already imported.
  public mutating func importCoreModule() {
    precondition(!isCoreModuleLoaded, "Core library is already loaded")

    do {
      var diagnostics = Diagnostics()
      corelib = try makeModule(
        "Val", sourceCode: sourceFiles(in: [ValModule.core!]), diagnostics: &diagnostics)

      assert(isCoreModuleLoaded)
    } catch let error {
      fatalError("Error parsing the core module:\n\(error.localizedDescription)")
    }
  }

  /// Returns `self`, fully type-checked, with the core module imported.
  ///
  /// - Parameters
  ///   - standardLibrary: The module, if any, to be checked with the Builtin module visible.
  ///   - inferenceTracingRange: A range of source code in which to trace type inference, or `nil` 
  ///     to disable tracing.
  ///   - diagnostics: A channel for errors and warnings.
  public func typeChecked(
    standardLibrary: NodeID<ModuleDecl>?,
    tracingInferenceIn inferenceTracingRange: SourceRange? = nil,
    diagnostics: inout Diagnostics
  ) throws -> TypedProgram {
    var ast = self
    if !ast.isCoreModuleLoaded {
      ast.importCoreModule()
    }

    var checker = TypeChecker(
      program: ScopedProgram(ast),
      isBuiltinModuleVisible: true,
      tracingInferenceIn: inferenceTracingRange)

    for m in ast.modules {
      checker.isBuiltinModuleVisible = m == ast.corelib || m == standardLibrary
      checker.check(module: m)
    }

    diagnostics.report(checker.diagnostics)
    try diagnostics.throwOnError()

    return TypedProgram(
      annotating: checker.program,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      implicitCaptures: checker.implicitCaptures,
      referredDecls: checker.referredDecls,
      foldedSequenceExprs: checker.foldedSequenceExprs)
  }
}
