import Foundation
import ValLibrary

@_exported import Compiler
@_exported import Utils

/// A helper to manage the compilation of Val source files.
public struct Driver {

  /// The compiler instance.
  public let compiler: Compiler

  /// The home path for Val's runtime and standard library.
  public var home: URL

  /// Creates a new driver with the specified compiler instance.
  ///
  /// - Parameters:
  ///   - compiler: A compiler instance.
  ///   - home: The root URL of Val's runtime environment.
  public init(compiler: Compiler = Compiler(), home: URL? = nil) {
    self.compiler = compiler

    // Set the home path.
    if let h = home {
      self.home = h
    } else if let h = ValLibrary.public {
      self.home = h
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
    let rootURL = url ?? home.appendingPathComponent("Core")
    let moduleDecl = try parse(moduleName: "Val", moduleRoot: rootURL, isStdlib: true)
    typeCheck(moduleDecl: moduleDecl)
    return moduleDecl
  }

  /// Parses the given source files as a module declaration.
  @discardableResult
  public func parse(
    moduleName: String, moduleFiles: [URL], isStdlib: Bool = false
  ) throws -> ModuleDecl {
    guard compiler.modules[moduleName] == nil else {
      throw DriverError.moduleAlreadyLoaded(moduleName: moduleName)
    }

    // Create a module.
    let module = ModuleDecl(name: moduleName, generation: compiler.generation, context: compiler)
    compiler.modules[moduleName] = module

    if let stdlib = compiler.stdlib {
      // All modules but the standard library implicitly depend on the standard library.
      precondition(!isStdlib, "standard library is already loaded")
      module.dependencies = [stdlib]
    } else if isStdlib {
      compiler.stdlib = module
    }

    // Parse the module's files.
    for url in moduleFiles {
      let parser = Parser(context: compiler)
      let (unit, hasError) = try parser.parse(contentsOf: url)

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
    assert(compiler.modules.values.contains(moduleDecl))

    compiler.isCompilingStdlib = (moduleDecl === compiler.stdlib)
    let result = TypeChecker.check(decl: moduleDecl)

    compiler.generation += 1
    return result
  }

  /// Type checks module identified by the given name.
  ///
  /// - Parameter moduleName: The name of the module to type check. The declaration must have been
  ///   loaded into the driver's context.
  @discardableResult
  public func typeCheck(moduleName: String) throws -> Bool {
    guard let moduleDecl = compiler.modules[moduleName] else {
      throw DriverError.moduleNotFound(moduleName: moduleName)
    }
    return typeCheck(moduleDecl: moduleDecl)
  }

  /// Lowers the given module declaration into a VIL module.
  ///
  /// - Parameter moduleDecl: The declaration of the module to lower. `moduleDecl` must have been
  ///   loaded into the driver's context.
  public func lower(moduleDecl: ModuleDecl) throws -> Module {
    assert(compiler.modules.values.contains(moduleDecl))
    guard moduleDecl.state == .typeChecked else {
      throw DriverError.moduleNotTypeChecked(moduleName: moduleDecl.name)
    }

    // Emit the module declaration.
    var module = Emitter.emit(module: moduleDecl)

    // Run VIL analysis passes.
    var lifetime = LifetimeAnalyis()
    var ownership = OwnershipAnalysis(context: compiler)
    for funName in module.functions.keys {
      lifetime.run(on: funName, in: &module)
      guard ownership.run(on: funName, in: &module) else {
        throw DriverError.loweringFailed(inFunction: funName, inModule: module.id)
      }
    }

    return module
  }

}
