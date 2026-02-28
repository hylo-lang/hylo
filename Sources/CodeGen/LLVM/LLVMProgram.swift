import Foundation
import FrontEnd
import IR
import SwiftyLLVM
import Utils

public class ModuleWrapper {
  public var module: SwiftyLLVM.Module
  public init(_ module: consuming SwiftyLLVM.Module) {
    self.module = module
  }
}
/// A Hylo program transpiled to LLVM.
public struct LLVMProgram: ~Copyable {

  /// The machine for which the program is compiled.
  public let target: SwiftyLLVM.TargetMachine

  /// The LLVM modules in the program.
  public private(set) var llvmModules: [ModuleDecl.ID: ModuleWrapper] = [:]

  /// Creates a transpiling `ir`, whose main module is `mainModule`, for `target`.
  ///
  /// - Parameters:
  ///   - target: The machine for which `ir` is transpiled. Defaults to the current host.
  public init(
    _ ir: IR.Program,
    mainModule: ModuleDecl.ID,
    for target: consuming SwiftyLLVM.TargetMachine
  ) throws {
    self.target = target
    for m in ir.modules.keys {
      var context = CodeGenerationContext(forCompiling: m, of: ir)
      let transpilation = SwiftyLLVM.Module(transpiling: m, in: &context)
      do {
        try transpilation.verify()
      } catch {
        print(transpilation.description)
        throw error
      }
      llvmModules[m] = ModuleWrapper(transpilation)
    }
  }

  /// Applies the mandatory IR simplification passes on each module in `self`.
  public mutating func applyMandatoryPasses() {
    for k in llvmModules.keys {
      llvmModules[k]!.module.runDefaultModulePasses(optimization: .none, for: target)
    }
  }

  /// Applies optimizations on each module in `self`.
  ///
  /// Optimization applied are similar to clang's `-O3`.
  public mutating func optimize() {
    for k in llvmModules.keys {
      llvmModules[k]!.module.runDefaultModulePasses(optimization: .aggressive, for: target)
    }
  }

  /// Compile the contents of this program to products of given `type` and writes theses products
  /// to `directory`, returning the URL of each written file.
  ///
  /// - Returns: The URL of each written product, one for each module in `self`.
  public func write(
    _ type: SwiftyLLVM.CodeGenerationResultType, to directory: URL
  ) throws -> [URL] {
    precondition(directory.hasDirectoryPath)
    var result: [URL] = []
    for m in llvmModules.values {
      let f = directory.appendingPathComponent(m.module.name).appendingPathExtension("o")
      try m.module.write(type, for: target, to: f.fileSystemPath)
      result.append(f)
    }
    return result
  }

}
