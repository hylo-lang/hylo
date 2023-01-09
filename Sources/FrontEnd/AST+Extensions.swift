import Core
import Utils
import ValModule

extension AST {

  /// Imports the core library into `self`.
  ///
  /// - Requires: The Core library must not have been already imported.
  public mutating func importCoreModule() {
    precondition(!isCoreModuleLoaded, "Core library is already loaded")

    var diagnostics = Diagnostics()
    corelib = insert(ModuleDecl(name: "Val"), diagnostics: &diagnostics)
    if !diagnostics.log.isEmpty {
      fatalError("Error inserting the core module:\n\(diagnostics)")
    }

    withFiles(
      in: ValModule.core!,
      { (sourceURL) in
        if sourceURL.pathExtension != "val" { return true }

        // Parse the file.
        do {
          let sourceFile = try SourceFile(contentsOf: sourceURL)
          _ = try Parser.parse(sourceFile, into: corelib!, in: &self, diagnostics: &diagnostics)
          // Parsing the core module should generate no diagnostics, not even warnings.
          if diagnostics.log.isEmpty { return true }
          throw diagnostics
        } catch let error {
          fatalError("Error parsing the core module:\n\(error.localizedDescription)")
        }
      })
  }
}
