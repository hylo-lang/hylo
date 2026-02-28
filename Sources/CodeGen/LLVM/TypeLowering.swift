import FrontEnd
import IR
import SwiftyLLVM
import Utils

extension IR.Program {

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm<T: TypeProtocol>(_ t: T, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    switch t {
    case let u as FrontEnd.AnyType:
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
      return module.ptr.erased
    case let u as ProductType:
      return llvm(productType: u, in: &module)
    case is RemoteType:
      return module.ptr.erased
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
  func llvm(arrowType t: ArrowType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    precondition(t.isCanonical)
    let e = llvm(t.environment, in: &module)
    return module.structType((module.ptr, e)).erased
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(bufferType t: BufferType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    let e = llvm(t.element, in: &module)
    guard let n = ConcreteTerm(t.count)?.value as? Int else {
      notLLVMRepresentable(t)
    }
    return module.arrayType(n, e).erased
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(builtinType t: BuiltinType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    switch t {
    case .i(let width):
      return module.integerType(width).erased
    case .word:
      return module.word.erased
    case .float16:
      return module.half.erased
    case .float32:
      return module.float.erased
    case .float64:
      return module.double.erased
    case .float128:
      return module.fp128.erased
    case .ptr:
      return module.ptr.erased
    case .module:
      notLLVMRepresentable(t)
    }
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(
    boundGenericType t: BoundGenericType, in module: inout SwiftyLLVM.Module
  ) -> SwiftyLLVM.AnyType.UnsafeReference {
    precondition(t.isCanonical)
    precondition(t.base.base is ProductType)
    let s = demandStruct(named: base.mangled(t), in: &module) { (m) in
      llvm(fields: base.storage(of: t), in: &m)
    }
    return s.erased
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(productType t: ProductType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    precondition(t.isCanonical)
    return demandStruct(named: base.mangled(t), in: &module) { (m) in
      llvm(fields: AbstractTypeLayout(of: t, definedIn: base).properties, in: &m)
    }.erased
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(tupleType t: TupleType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    precondition(t.isCanonical)
    let fields = llvm(fields: t.elements, in: &module)
    return module.structType(fields).erased
  }

  /// Rethrns the LLVM forms of `fields` in `module`.
  private func llvm(
    fields: [TupleType.Element], in module: inout SwiftyLLVM.Module
  ) -> [SwiftyLLVM.AnyType.UnsafeReference] {
    fields.map({ (p) in llvm(p.type, in: &module) })
  }

  /// Returns the LLVM form of `t` in `module`.
  ///
  /// - Requires: `t` is representable in LLVM.
  func llvm(unionType t: UnionType, in module: inout SwiftyLLVM.Module) -> SwiftyLLVM.AnyType.UnsafeReference {
    precondition(t.isCanonical)

    var payload = module.structType([]).erased
    if t.isNever {
      return payload
    }

    for e in t.elements {
      let u = llvm(e, in: &module)
      if module.layout.storageSize(of: u) > module.layout.storageSize(of: payload) {
        payload = u
      }
    }
    return module.structType((payload, module.word)).erased
  }

  /// Returns a LLVM struct named `n` and having the fields returned by `fields`, declaring it in
  /// `module` if it does not already exist.
  private func demandStruct(
    named n: String, in module: inout SwiftyLLVM.Module,
    fields: (inout SwiftyLLVM.Module) -> [SwiftyLLVM.AnyType.UnsafeReference]
  ) -> SwiftyLLVM.StructType.UnsafeReference {
    if let u = module.type(named: n) {
      return StructType.UnsafeReference(u)!
    } else {
      let fs = fields(&module)
      return module.structType(named: n, fs)
    }
  }

}
