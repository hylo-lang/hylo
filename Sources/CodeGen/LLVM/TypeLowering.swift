import FrontEnd
import IR
import SwiftyLLVM
import Utils

extension IR.Program {

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm<T: TypeProtocol>(_ val: T, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    switch val {
    case let t as AnyType:
      return llvm(t.base, in: &module)
    case let t as ArrowType:
      return llvm(arrowType: t, in: &module)
    case let t as BufferType:
      return llvm(bufferType: t, in: &module)
    case let t as BuiltinType:
      return llvm(builtinType: t, in: &module)
    case let t as BoundGenericType:
      return llvm(boundGenericType: t, in: &module)
    case is MetatypeType:
      return module.ptr
    case let t as ProductType:
      return llvm(productType: t, in: &module)
    case is RemoteType:
      return module.ptr
    case let t as TupleType:
      return llvm(tupleType: t, in: &module)
    case let t as UnionType:
      return llvm(unionType: t, in: &module)
    default:
      notLLVMRepresentable(val)
    }
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(arrowType t: ArrowType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(t[.isCanonical])
    let e = llvm(t.environment, in: &module)
    return SwiftyLLVM.StructType([module.ptr, e], in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(bufferType val: BufferType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    let e = llvm(val.element, in: &module)
    guard let n = val.count.asCompilerKnown(Int.self) else {
      notLLVMRepresentable(val)
    }
    return SwiftyLLVM.ArrayType(n, e, in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(builtinType val: BuiltinType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    switch val {
    case .i(let width):
      return SwiftyLLVM.IntegerType(width, in: &module)
    case .word:
      return module.word()
    case .float16:
      return SwiftyLLVM.FloatingPointType.half(in: &module)
    case .float32:
      return SwiftyLLVM.FloatingPointType.float(in: &module)
    case .float64:
      return SwiftyLLVM.FloatingPointType.double(in: &module)
    case .float128:
      return SwiftyLLVM.FloatingPointType.fp128(in: &module)
    case .ptr:
      return module.ptr
    case .module:
      notLLVMRepresentable(val)
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(boundGenericType val: BoundGenericType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(val[.isCanonical])

    let fields = base.storage(of: val.base).map { (part) in
      let z = GenericArguments(val)
      let u = base.specialize(part.type, for: z, in: AnyScopeID(base.ast.coreLibrary!))
      return llvm(u, in: &module)
    }

    switch val.base.base {
    case let u as ProductType:
      return SwiftyLLVM.StructType(named: u.name.value, fields, in: &module)
    case is TupleType:
      return SwiftyLLVM.StructType(fields, in: &module)
    default:
      unreachable()
    }
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(productType val: ProductType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(val[.isCanonical])

    let n = base.mangled(val)
    if let t = module.type(named: n) {
      assert(SwiftyLLVM.StructType(t) != nil)
      return t
    }

    let l = AbstractTypeLayout(of: val, definedIn: base)
    var fields: [SwiftyLLVM.IRType] = []
    for p in l.properties {
      fields.append(llvm(p.type, in: &module))
    }

    return SwiftyLLVM.StructType(named: n, fields, in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(tupleType val: TupleType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(val[.isCanonical])

    var fields: [SwiftyLLVM.IRType] = []
    for e in val.elements {
      fields.append(llvm(e.type, in: &module))
    }

    return SwiftyLLVM.StructType(fields, in: &module)
  }

  /// Returns the LLVM form of `val` in `module`.
  ///
  /// - Requires: `val` is representable in LLVM.
  func llvm(unionType val: UnionType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(val[.isCanonical])

    var payload: SwiftyLLVM.IRType = SwiftyLLVM.StructType([], in: &module)
    if val == .never {
      return payload
    }

    for e in val.elements {
      let t = llvm(e, in: &module)
      if module.layout.storageSize(of: t) > module.layout.storageSize(of: payload) {
        payload = t
      }
    }
    return StructType([payload, module.word()], in: &module)
  }

}
