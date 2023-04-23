import Core
import IR
import LLVM

extension LoweredProgram {

  /// Returns the LLVM transpilation of the Val IR module `m`.
  func transpile(_ m: ModuleDecl.ID) -> LLVM.Module {
    let ir = modules[m]!
    var transpilation = LLVM.Module(ir.name)
    for g in ir.globals.indices {
      transpilation.incorporate(g, of: ir, from: self)
    }
    for f in ir.functions.keys {
      transpilation.incorporate(f, of: ir, from: self)
    }
    return transpilation
  }

}
