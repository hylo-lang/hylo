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

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm<T: TypeProtocol>(_ val: T, in module: inout LLVM.Module) -> LLVM.IRType {
    switch val {
    case let t as AnyType:
      return llvm(t.base, in: &module)
    case let t as BuiltinType:
      return llvm(builtinType: t, in: &module)
    case let t as LambdaType:
      return llvm(lambdaType: t, in: &module)
    case let t as ProductType:
      return llvm(productType: t, in: &module)
    case let t as TupleType:
      return llvm(tupleType: t, in: &module)
    default:
      notLLVMRepresentable(val)
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(builtinType val: BuiltinType, in module: inout LLVM.Module) -> LLVM.IRType {
    switch val {
    case .i(let width):
      return LLVM.IntegerType(width, in: &module)
    case .half:
      return LLVM.FloatingPointType.half(in: &module)
    case .float:
      return LLVM.FloatingPointType.float(in: &module)
    case .double:
      return LLVM.FloatingPointType.double(in: &module)
    case .fp128:
      return LLVM.FloatingPointType.fp128(in: &module)
    case .ptr:
      return LLVM.PointerType(in: &module)
    case .module:
      notLLVMRepresentable(val)
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(lambdaType val: LambdaType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])
    if val.environment == .void {
      return LLVM.PointerType(in: &module)
    } else {
      fatalError("not implemented")
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(productType val: ProductType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])

    let n = mangle(val)
    if let t = module.type(named: n) {
      assert(LLVM.StructType(t) != nil)
      return t
    }

    let l = AbstractTypeLayout(of: val, definedIn: syntax)
    var fields: [LLVM.IRType] = []
    for p in l.properties {
      fields.append(llvm(p.type, in: &module))
    }

    return LLVM.StructType(named: n, fields, in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(tupleType val: TupleType, in module: inout LLVM.Module) -> LLVM.IRType {
    var fields: [LLVM.IRType] = []
    for e in val.elements {
      fields.append(llvm(e.type, in: &module))
    }
    return LLVM.StructType(fields, in: &module)
  }

}

/// Traps indicating that `t` is not representable in LLVM.
private func notLLVMRepresentable<T: TypeProtocol>(_ t: T) -> Never {
  preconditionFailure("'\(t)' is not representable in LLVM")
}
