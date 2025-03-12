import FrontEnd
import IR
import SwiftyLLVM
import Utils

/// The concrete layout of a type, describing the byte offsets of its stored properties.
struct ConcreteTypeLayout {

  /// The contiguous memory footprint of the type's instances, in bytes.
  let size: Int

  /// The memory alignment of the type's instances, in bytes.
  let alignment: Int

  /// The number of bytes from the start of one instance to the start of the next when stored in
  /// contiguous memory.
  var stride: Int {
    max(1, size.rounded(upToNearestMultipleOf: alignment))
  }

  private func checkInvariant() {
    precondition(size >= 0, "negative size")
    precondition(alignment > 0, "alignment must be positive")
  }

  /// Creates an instance with the given properties.
  init(size: Int, alignment: Int) {
    self.size = size
    self.alignment = alignment
    checkInvariant()
  }

  /// Creates the concrete form of the layout `l` of a type defined in `ir`, for use in `m`.
  ///
  /// - Requires: `l.type` is representable in LLVM.
  init(_ l: AbstractTypeLayout, definedIn ir: IR.Program, forUseIn m: inout SwiftyLLVM.Module) {
    switch l.type.base {
    case let t as BuiltinType:
      self.init(of: t, forUseIn: &m)

    case is ProductType where l.properties.count == 1:
      // Note: Hack to get the correct alignment of built-in wrappers until we implement a proper
      // layout algorithm (#1067).
      self.init(of: l.properties[0].type, definedIn: ir, forUseIn: &m)

    default:
      let u = ir.llvm(l.type, in: &m)
      self.init(size: m.layout.storageSize(of: u), alignment: m.layout.preferredAlignment(of: u))
    }
    checkInvariant()
  }

  /// Creates the layout of `t`, which is defined in `ir`, for use in `m`.
  ///
  /// - Requires: `t` is representable in LLVM.
  init(of t: AnyType, definedIn ir: IR.Program, forUseIn m: inout SwiftyLLVM.Module) {
    self.init(AbstractTypeLayout(of: t, definedIn: ir.base), definedIn: ir, forUseIn: &m)
    checkInvariant()
  }

  /// Creates the layout of `t` for use in `m`.
  ///
  /// - Requires: `t` is not `.module`.
  init(of t: BuiltinType, forUseIn m: inout SwiftyLLVM.Module) {
    switch t {
    case .ptr:
      let s = m.layout.storageSize(of: m.ptr)
      self.init(size: s, alignment: min(s, 8))
    case .i(let width):
      let s = max(1, width / 8)
      self.init(size: s, alignment: min(s, 8))
    case .word:
      self.init(of: .ptr, forUseIn: &m)
    case .float16:
      self.init(size: 2, alignment: 2)
    case .float32:
      self.init(size: 4, alignment: 4)
    case .float64:
      self.init(size: 8, alignment: 8)
    case .float128:
      self.init(size: 16, alignment: 8)
    case .module:
      notLLVMRepresentable(^t)
    }
    checkInvariant()
  }

}
