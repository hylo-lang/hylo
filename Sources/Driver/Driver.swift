import Foundation

@_exported import AST
@_exported import Basic
import Parse
import Sema
import VIL

/// A helper to manage the compilation of Val source files.
public struct Driver {

  /// The AST context.
  ///
  /// The context is the central repository for long-lived objects (e.g., types and declarations)
  /// created throughout the compilation process.
  public let context: AST.Context

  /// The home path for Val's runtime and standard library.
  public var home: URL

  /// Creates a new driver.
  ///
  /// - Parameters:
  ///   - context: An AST context.
  ///   - home: The root URL of Val's runtime environment.
  public init(context: Context? = nil, home: URL? = nil) {
    self.context = context ?? Context()

    // Set the home path.
    if let h = home {
      self.home = h
    } else if let p = ProcessInfo.processInfo.environment["VAL_HOME"] {
      self.home = URL(fileURLWithPath: p)
    } else {
      self.home = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
    }
  }

  /// Loads the standard library into the driver's context.
  ///
  /// - Parameter url: The root URL of the standard library. By default, the driver will look for
  ///   a directly `Stdlib` at the root of its home path.
  @discardableResult
  public func loadStdlib(url: URL? = nil) throws -> ModuleDecl  {
    let rootURL = url ?? home.appendingPathComponent("Stdlib")
    let moduleDecl = try parse(moduleName: "Val", moduleRoot: rootURL, isStdlib: true)
    typeCheck(moduleDecl: moduleDecl)
    return moduleDecl
  }

  /// Parses the given source files as a module declaration.
  @discardableResult
  public func parse(
    moduleName: String, moduleFiles: [URL], isStdlib: Bool = false
  ) throws -> ModuleDecl {
    guard context.modules[moduleName] == nil else {
      throw DriverError.moduleAlreadyLoaded(moduleName: moduleName)
    }

    // Create a module.
    let module = ModuleDecl(name: moduleName, generation: context.generation, context: context)
    context.modules[moduleName] = module

    if let stdlib = context.stdlib {
      // All modules but the standard library implicitly depend on the standard library.
      precondition(!isStdlib, "standard library is already loaded")
      module.dependencies = [stdlib]
    } else if isStdlib {
      context.stdlib = module
    }

    // Parse the module's files.
    for url in moduleFiles {
      let source = try context.sourceManager.load(contentsOf: url)
      let parser = Parser(context: context)
      let (unit, hasError) = parser.parse(source: source)

      module.units.append(unit)
      unit.parentDeclSpace = module
      if hasError {
        module.setState(.invalid)
      }
    }

    return module
  }

  /// Parse the given source files as a module declaration.
  @discardableResult
  public func parse(
    moduleName: String, moduleRoot: URL, isStdlib: Bool = false
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

    guard !moduleFiles.isEmpty else {
      throw DriverError.moduleNotFound(moduleName: moduleName)
    }

    return try parse(moduleName: moduleName, moduleFiles: moduleFiles, isStdlib: isStdlib)
  }

  /// Type checks the given module declaration.
  ///
  /// - Parameter moduleDecl: The declaration of the module to type check. `moduleDecl` must have
  ///   been loaded into the driver's context.
  @discardableResult
  public func typeCheck(moduleDecl: ModuleDecl) -> Bool {
    assert(context.modules.values.contains(moduleDecl))

    context.isCompilingStdlib = (moduleDecl === context.stdlib)
    TypeChecker.initialize(in: context)
    let result = TypeChecker.check(decl: moduleDecl)

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

    // Emit the module declaration.
    return Emitter.emit(module: moduleDecl)
  }

}
