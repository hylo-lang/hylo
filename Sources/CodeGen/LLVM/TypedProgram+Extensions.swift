import Core
import IR
import LLVM
import Utils

extension LoweredProgram {

  /// Returns the name of `f` in the ABI.
  func abiName(of f: IR.Function.ID) -> String {
    switch f.value {
    case .lowered(let d):
      return abiName(of: d)
    case .constructor(let d):
      return abiName(of: d)
    case .synthesized:
      fatalError("not implemented")
    }
  }

  /// Returns the name of `d` in the ABI.
  func abiName<T: DeclID>(of d: T) -> String {
    let m = syntax.module(containing: syntax.declToScope[d]!)
    return "_V\(m.rawValue)_\(d.rawValue)"
  }

}

extension TypedProgram {

  /// Returns the name of `d` in the ABI.
  func abiName<T: DeclID>(of d: T) -> String {
    let m = module(containing: declToScope[d]!)
    return "_V\(m.rawValue)_\(d.rawValue)"
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
      return llvm(t, in: &module)
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

    guard val.environment == .void else { notLLVMRepresentable(val) }
    var parameters: [LLVM.IRType] = []
    for p in val.inputs {
      parameters.append(llvm(p.type, in: &module))
    }
    let r = llvm(val.output, in: &module)
    return LLVM.FunctionType(from: parameters, to: r, in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(productType val: ProductType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])

    let n = abiName(of: val.decl)
    if let t = module.type(named: n) {
      assert(LLVM.StructType(t) != nil)
      return t
    }

    let l = AbstractTypeLayout(of: val, definedIn: self)
    let fields: [LLVM.IRType] = modified([], { (fields) in
      for p in l.properties {
        fields.append(llvm(p.type, in: &module))
      }
    })

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
