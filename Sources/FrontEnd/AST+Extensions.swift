import Core
import Utils
import ValModule

extension AST {

  /// Imports the core library into `self`.
  ///
  /// - Requires: The Core library must not have been already imported.
  public mutating func importCoreModule() {
    precondition(!isCoreModuleLoaded, "Core library is already loaded")
    corelib = try! insert(wellFormed: ModuleDecl(name: "Val"))

    withFiles(
      in: ValModule.core!,
      { (sourceURL) in
        if sourceURL.pathExtension != "val" { return true }

        // Parse the file.
        do {
          let sourceFile = try SourceFile(contentsOf: sourceURL)
          let diagnostics = Parser.parse(sourceFile, into: corelib!, in: &self).diagnostics

          // Note: the core module shouldn't produce any diagnostic.
          if !diagnostics.isEmpty {
            throw DiagnosedError(diagnostics)
          } else {
            return true
          }
        } catch let error as DiagnosedError {
          fatalError(error.diagnostics.first!.description)
        } catch let error {
          fatalError(error.localizedDescription)
        }
      })
  }

}
