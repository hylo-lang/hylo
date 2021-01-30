import Foundation

import AST
import Parser
import Sema

/// A compile job.
public struct CompileJob: Job {

  /// The name of the module to compile.
  public var moduleName: String

  /// The path of the source files contained in the module.
  public var moduleFiles: [URL]

  /// A flag that indicates whether the compiler should only parse the module's files, without
  /// running semantic analysis.
  public var parseOnly: Bool

  /// A flag that indicates whether the module to compile is the standard library.
  public var isStdLib: Bool

  public init(
    moduleName: String, moduleFiles: [URL], parseOnly: Bool = false, isStdLib: Bool = false
  ) {
    self.moduleName = moduleName
    self.moduleFiles = moduleFiles
    self.parseOnly = parseOnly
    self.isStdLib = isStdLib
  }

  public init(
    moduleName: String, moduleRootDir: URL, parseOnly: Bool = false, isStdLib: Bool = false
  ) {
    var moduleFiles: [URL] = []
    if let enumerator = FileManager.default.enumerator(
            at: moduleRootDir,
            includingPropertiesForKeys: [.isRegularFileKey],
            options: [.skipsHiddenFiles, .skipsPackageDescendants])
    {
      moduleFiles = enumerator.compactMap({ (el: Any) -> URL? in
        guard let url = el as? URL,
              let attributes = try? url.resourceValues(forKeys: [.isRegularFileKey]),
              attributes.isRegularFile!,
              url.pathExtension == "val"
        else { return nil }
        return url
      })
    }

    self.init(
      moduleName: moduleName,
      moduleFiles: moduleFiles,
      parseOnly: parseOnly,
      isStdLib: isStdLib)
  }

  public func run(in context: Context) throws {
    guard context.modules[moduleName] == nil else {
      throw DriverError.moduleAlreadyLoaded(moduleName: moduleName)
    }

    // Create a module.
    let module = Module(id: moduleName, context: context)
    context.modules[module.id] = module

    if isStdLib {
      precondition(context.stdlib == nil, "standard library is already loaded")
      context.stdlib = module
      context.isCompilingStdLib = true
    }
    defer { context.isCompilingStdLib = false }

    // Parse the module's files.
    for url in moduleFiles {
      let source = try context.sourceManager.load(contentsOf: url)
      let parser = try ValParser(sourceFile: source)
      let parseTree = try parser.file()
      let transformer = ParseTreeTransformer(sourceFile: source, module: module, context: context)
      _ = parseTree.accept(transformer)
    }

    // Type-check the module, unless instructed otherwise.
    if !parseOnly {
      TypeChecker(context: context).check(module: module)
    }
  }

  public static func stdlib(path: URL, parseOnly: Bool = false) -> CompileJob {
    return CompileJob(moduleName: "Val", moduleRootDir: path, parseOnly: parseOnly, isStdLib: true)
  }

}
