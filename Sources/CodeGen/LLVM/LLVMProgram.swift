import IR
import LLVM

/// A Val program transpiled to LLVM.
public struct LLVMProgram {

  /// The LLVM modules in the program.
  private var llvmModules: [LLVM.Module] = []

  /// Creates an empty program.
  public init(_ irProgram: [IR.Module]) {
    for m in irProgram {
      llvmModules.append(transpiled(m))
    }
  }

  /// The LLVM transpilation of the Val IR module `m`.
  private func transpiled(_ m: IR.Module) -> LLVM.Module {
    fatalError()
  }

}
