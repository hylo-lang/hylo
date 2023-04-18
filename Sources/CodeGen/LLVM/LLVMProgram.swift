import Core
import Foundation
import IR
import LLVM

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The LLVM modules in the program.
  public private(set) var llvmModules: [ModuleDecl.ID: LLVM.Module] = [:]

  /// Creates an empty program.
  public init(_ ir: LoweredProgram, mainModule: ModuleDecl.ID) throws {
    for m in ir.modules.keys {
      let transpilation = ir.transpile(m)
      do {
        try transpilation.verify()
      } catch {
        print(transpilation)
        throw error
      }
      llvmModules[m] = transpilation
    }
  }

  public func write(_ type: LLVM.CodeGenerationResultType, to directory: URL) throws -> [URL] {
    precondition(directory.hasDirectoryPath)

    let host = try LLVM.Target.host()
    let machine = LLVM.TargetMachine(for: host)
    var result: [URL] = []
    for m in llvmModules.values {
      let f = directory.appendingPathComponent(m.name).appendingPathExtension("o")
      try m.write(type, for: machine, to: f.path)
      result.append(f)
    }

    return result
  }

}
