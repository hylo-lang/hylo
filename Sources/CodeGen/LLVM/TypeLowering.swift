import FrontEnd
import IR
import SwiftyLLVM
import Utils

extension IR.Program {

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm<T: TypeProtocol>(_ t: T, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    switch t {
    case let u as AnyType:
      return llvm(u.base, in: &module)
    case let u as ArrowType:
      return llvm(arrowType: u, in: &module)
    case let u as BufferType:
      return llvm(bufferType: u, in: &module)
    case let u as BuiltinType:
      return llvm(builtinType: u, in: &module)
    case let u as BoundGenericType:
      return llvm(boundGenericType: u, in: &module)
    case is MetatypeType:
      return module.ptr
    case let u as ProductType:
      return llvm(productType: u, in: &module)
    case is RemoteType:
      return module.ptr
    case let u as TupleType:
      return llvm(tupleType: u, in: &module)
    case let u as UnionType:
      return llvm(unionType: u, in: &module)
    default:
      notLLVMRepresentable(t)
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

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(bufferType t: BufferType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    let e = llvm(t.element, in: &module)
    guard let n = t.count.asCompilerKnown(Int.self) else {
      notLLVMRepresentable(t)
    }
    return SwiftyLLVM.ArrayType(n, e, in: &module)
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(builtinType t: BuiltinType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    switch t {
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
      notLLVMRepresentable(t)
    }
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(
    boundGenericType t: BoundGenericType, in module: inout SwiftyLLVM.Module
  ) -> SwiftyLLVM.IRType {
    precondition(t[.isCanonical])

    let fields = base.storage(of: t).map({ (p) in llvm(p.type, in: &module) })

    switch t.base.base {
    case let u as ProductType:
      return SwiftyLLVM.StructType(named: u.name.value, fields, in: &module)
    case is TupleType:
      return SwiftyLLVM.StructType(fields, in: &module)
    default:
      unreachable()
    }
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(productType t: ProductType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(t[.isCanonical])

    let n = base.mangled(t)
    if let u = module.type(named: n) {
      assert(SwiftyLLVM.StructType(u) != nil)
      return u
    }

    let l = AbstractTypeLayout(of: t, definedIn: base)
    var fields: [SwiftyLLVM.IRType] = []
    for p in l.properties {
      fields.append(llvm(p.type, in: &module))
    }

    return SwiftyLLVM.StructType(named: n, fields, in: &module)
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(tupleType t: TupleType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(t[.isCanonical])

    var fields: [SwiftyLLVM.IRType] = []
    for e in t.elements {
      fields.append(llvm(e.type, in: &module))
    }

    return SwiftyLLVM.StructType(fields, in: &module)
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(unionType t: UnionType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.IRType {
    precondition(t[.isCanonical])

    var payload: SwiftyLLVM.IRType = SwiftyLLVM.StructType([], in: &module)
    if t.isNever {
      return payload
    }

    for e in t.elements {
      let u = llvm(e, in: &module)
      if module.layout.storageSize(of: u) > module.layout.storageSize(of: payload) {
        payload = u
      }
    }
    return StructType([payload, module.word()], in: &module)
  }

}
