import Core
import Foundation
import IR
import LLVM
import Utils

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The machine for which the program is compiled.
  public let target: LLVM.TargetMachine

  /// The LLVM modules in the program.
  public private(set) var llvmModules: [ModuleDecl.ID: LLVM.Module] = [:]

  /// Creates a transpiling `ir`, whose main module is `mainModule`, for `target`.
  ///
  /// - Parameters:
  ///   - target: The machine for which `ir` is transpiled. Defaults to the current host.
  public init(
    _ ir: LoweredProgram,
    mainModule: ModuleDecl.ID,
    for target: LLVM.TargetMachine? = nil
  ) throws {
    self.target = try target ?? LLVM.TargetMachine(for: .host())
    for m in ir.modules.keys {
      let transpilation = LLVM.Module(transpiling: m, from: ir)
      do {
        try transpilation.verify()
      } catch {
        print(transpilation)
        throw error
      }
      llvmModules[m] = transpilation
    }
  }

  /// Applies the mandatory IR simplication passes on each module in `self`.
  public mutating func applyMandatoryPasses() {
    for k in llvmModules.keys {
      llvmModules[k]!.runDefaultModulePasses(optimization: .none, for: target)
    }
  }

  /// Applies optimizations on each module in `self`.
  ///
  /// Optimization applied are similar to clang's `-O3`.
  public mutating func optimize() {
    for k in llvmModules.keys {
      llvmModules[k]!.runDefaultModulePasses(optimization: .aggressive, for: target)
    }
  }

  /// Compile the contents of this program to products of given `type` and writes theses products
  /// to `directory`, returning the URL of each written file.
  ///
  /// - Returns: The URL of each written product, one for each module in `self`.
  public func write(_ type: LLVM.CodeGenerationResultType, to directory: URL) throws -> [URL] {
    precondition(directory.hasDirectoryPath)
    var result: [URL] = []
    for m in llvmModules.values {
      let f = directory.appendingPathComponent(m.name).appendingPathExtension("o")
      try m.write(type, for: target, to: f.path)
      result.append(f)
    }
    return result
  }

}
