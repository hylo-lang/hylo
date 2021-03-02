import Foundation

import AST
import Basic
import Eval
import Parser
import Sema
import VIL

/// Val's compiler driver, that manages the compilation process.
public struct Driver {

  /// The AST context.
  ///
  /// This is the central repository for long-lived objects (e.g., types and declarations) created
  /// throughout the compilation process.
  public let context: AST.Context

  public init(
    sourceManager: SourceManager? = nil,
    diagnosticConsumer: DiagnosticConsumer? = nil
  ) {
    // Create the driver's AST context.
    context = AST.Context(sourceManager: sourceManager ?? SourceManager())
    context.diagnosticConsumer = diagnosticConsumer
  }

  /// Loads the standard library into the driver's context.
  @discardableResult
  public func loadStdLib(path: URL) throws -> ModuleDecl  {
    let moduleDecl = try parse(moduleName: "Val", moduleRoot: path, isStdLib: true)
    typeCheck(moduleDecl: moduleDecl)
    return moduleDecl
  }

  /// Parse the given source files as a module declaration.
  @discardableResult
  public func parse(
    moduleName: String, moduleFiles: [URL], isStdLib: Bool = false
  ) throws -> ModuleDecl {
    guard context.modules[moduleName] == nil else {
      throw DriverError.moduleAlreadyLoaded(moduleName: moduleName)
    }

    // Create a module.
    let module = ModuleDecl(name: moduleName, generation: context.generation, context: context)
    context.modules[moduleName] = module

    if let stdlib = context.stdlib {
      // All modules but the standard library implicitly depend on the standard library.
      precondition(!isStdLib, "standard library is already loaded")
      module.dependencies = [stdlib]
    } else if isStdLib {
      context.stdlib = module
    }

    // Parse the module's files.
    for url in moduleFiles {
      let source = try context.sourceManager.load(contentsOf: url)
      let parser = try ValParser(sourceFile: source)
      let parseTree = try parser.file()
      let transformer = ParseTreeTransformer(sourceFile: source, module: module, context: context)
      _ = parseTree.accept(transformer)
    }

    return module
  }

  /// Parse the given source files as a module declaration.
  @discardableResult
  public func parse(
    moduleName: String, moduleRoot: URL, isStdLib: Bool = false
  ) throws -> ModuleDecl {
    var moduleFiles: [URL] = []
    if let enumerator = FileManager.default.enumerator(
            at: moduleRoot,
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

    return try parse(moduleName: moduleName, moduleFiles: moduleFiles, isStdLib: isStdLib)
  }

  /// Type checks the given module declaration.
  ///
  /// - Parameter moduleDecl: The declaration of the module to type check. `moduleDecl` must have
  ///   been loaded into the driver's context.
  @discardableResult
  public func typeCheck(moduleDecl: ModuleDecl) -> Bool {
    assert(context.modules.values.contains(moduleDecl))
    let tc = TypeChecker(context: context)
    context.isCompilingStdLib = (moduleDecl === context.stdlib)
    let result = tc.check(decl: moduleDecl)
    context.generation += 1
    return result
  }

  /// Type checks module identified by the given name.
  ///
  /// - Parameter moduleName: The name of the module to type check. The declaration must have been
  ///   loaded into the driver's context.
  @discardableResult
  public func typeCheck(moduleName: String) throws -> Bool {
    guard let moduleDecl = context.modules[moduleName] else {
      throw DriverError.moduleNotFound(moduleName: moduleName)
    }
    return typeCheck(moduleDecl: moduleDecl)
  }

  /// Lowers the given module declaration into a VIL module.
  ///
  /// - Parameter moduleDecl: The declaration of the module to lower. `moduleDecl` must have been
  ///   loaded into the driver's context.
  public func lower(moduleDecl: ModuleDecl) throws -> Module {
    assert(context.modules.values.contains(moduleDecl))
    guard moduleDecl.state == .typeChecked else {
      throw DriverError.moduleNotTypeChecked(moduleName: moduleDecl.name)
    }

    // Initialize the VIL emitter.
    let module  = Module(id: moduleDecl.name)
    let builder = VIL.Builder(module: module)
    let emitter = Emitter(context: context, builder: builder)

    // Emit the module declaration.
    emitter.emit(moduleDecl: moduleDecl)
    module.dump()

    return module
  }

  /// Evaluates the given VIL module.
  ///
  /// - Parameter module: The module to evaluate. `module` must define a main entry point (i.e., an
  ///   unmangled `main` function) and have been lowered in the same context as the driver's
  ///   loaded into the driver's context.
  public func eval(module: Module) throws {
    var interpreter = Interpreter(context: context)
    try interpreter.eval(module: module)
  }

  /// Dumps the driver's context into the standard output.
  public func dump() {
    var stream = StandardOutput()
    dump(to: &stream)
  }

  /// Dumps the driver's context into the given stream.
  ///
  /// - Parameter stream: A text output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    NodePrinter(context: context).print(to: &stream)
  }

}
