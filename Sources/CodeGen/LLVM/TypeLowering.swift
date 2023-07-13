import Core
import IR
import LLVM
import Utils

extension LoweredProgram {

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm<T: TypeProtocol>(_ val: T, in module: inout LLVM.Module) -> LLVM.IRType {
    switch val {
    case let t as AnyType:
      return llvm(t.base, in: &module)
    case let t as BuiltinType:
      return llvm(builtinType: t, in: &module)
    case let t as BoundGenericType:
      return llvm(boundGenericType: t, in: &module)
    case let t as LambdaType:
      return llvm(lambdaType: t, in: &module)
    case is MetatypeType:
      return module.ptr
    case let t as ProductType:
      return llvm(productType: t, in: &module)
    case let t as SumType:
      return llvm(sumType: t, in: &module)
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
    case .word:
      return module.word()
    case .float16:
      return LLVM.FloatingPointType.half(in: &module)
    case .float32:
      return LLVM.FloatingPointType.float(in: &module)
    case .float64:
      return LLVM.FloatingPointType.double(in: &module)
    case .float128:
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
  func llvm(boundGenericType val: BoundGenericType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])

    let fields = syntax.storage(of: val.base).map { (_, t) in
      let u = syntax.monomorphize(t, for: val.arguments)
      return llvm(u, in: &module)
    }

    switch val.base.base {
    case let u as ProductType:
      return LLVM.StructType(named: u.name.value, fields, in: &module)
    case is TupleType:
      return LLVM.StructType(fields, in: &module)
    default:
      unreachable()
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(lambdaType val: LambdaType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])
    if val.environment != .void {
      notLLVMRepresentable(val)
    }

    return LLVM.PointerType(in: &module)
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
  func llvm(sumType val: SumType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])

    var payload: IRType = LLVM.StructType([], in: &module)
    if val == .never {
      return payload
    }

    for e in val.elements {
      let t = llvm(e, in: &module)
      if module.layout.storageSize(of: t) > module.layout.storageSize(of: payload) {
        payload = t
      }
    }
    return StructType([payload, module.ptr], in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(tupleType val: TupleType, in module: inout LLVM.Module) -> LLVM.IRType {
    precondition(val[.isCanonical])

    var fields: [LLVM.IRType] = []
    for e in val.elements {
      fields.append(llvm(e.type, in: &module))
    }

    return LLVM.StructType(fields, in: &module)
  }

}

/// Traps indicating that `t` is not representable in LLVM.
private func notLLVMRepresentable<T: TypeProtocol>(
  _ t: T, file: StaticString = #filePath, line: UInt = #line
) -> Never {
  preconditionFailure("'\(t)' is not representable in LLVM", file: (file), line: line)
}
