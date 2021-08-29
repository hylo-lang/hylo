import AST
import VIL

/// A data structure that supplies information about the layout of a type at runtime.
struct DataLayout {

  /// Returns the number of bytes of a type's memory representation.
  func size(of type: VILType) -> Int {
    if type.isAddress || (type is VILFunType) {
      return MemoryLayout<ThickFunction>.size
    } else if type.isExistential {
      return MemoryLayout<ExistentialContainer>.stride
    } else {
      return size(of: type.valType)
    }
  }

  /// Returns the number of bytes of a type's memory representation.
  private func size(of type: ValType) -> Int {
    switch type {
    case is BuiltinIntLiteralType:
      return MemoryLayout<Int>.size

    case let ty as BuiltinIntType:
      return ty.bitWidth.roundedAwayFromZero(toNearestMultipleOf: 8) / 8

    case let ty as ProductType:
      return size(of: ty.decl.storedVars.map({ $0.type }))

    case let ty as TupleType:
      return size(of: ty.elems.map({ $0.type }))

    case is AsyncType:
      return MemoryLayout<VirtualThread.ID>.size

    default:
      fatalError("unexpected data type '\(type)'")
    }
  }

  /// Returns the number of bytes of the memory representation of a compound type.
  func size<S>(of compound: S) -> Int where S: Sequence, S.Element == VILType {
    var acc = 0
    for type in compound {
      let align = alignment(of: type)
      acc += size(of: type).roundedAwayFromZero(toNearestMultipleOf: align)
    }
    return acc
  }

  /// Returns the number of bytes of the memory representation of a compound type.
  private func size(of compound: [ValType]) -> Int {
    var acc = 0
    for type in compound {
      let align = alignment(of: type)
      acc += size(of: type).roundedAwayFromZero(toNearestMultipleOf: align)
    }
    return acc
  }

  /// Returns the default memory alignment of `type`, in bytes.
  func alignment(of type: VILType) -> Int {
    if type.isAddress {
      return MemoryLayout<ValueAddr>.alignment
    } else if type.isExistential {
      return MemoryLayout<ExistentialContainer>.alignment
    } else {
      return alignment(of: type.valType)
    }
  }

  /// Returns the default memory alignment of `type`, in bytes.
  private func alignment(of type: ValType) -> Int {
    switch type {
    case is BuiltinIntLiteralType:
      return MemoryLayout<Int>.alignment

    case let ty as BuiltinIntType:
      // If the size of the type is a power of two, use it as its alignment; otherwise, rounds the
      // size up to the nearest power of two.
      var s = ty.bitWidth.roundedAwayFromZero(toNearestMultipleOf: 8) / 8
      if s & (s - 1) != 0 {
        s = s.nextPowerOfTwo
      }
      return max(s, 1)

    case let ty as ProductType:
      return ty.decl.storedVars.reduce(1, { max($0, alignment(of: $1.type)) })

    case let ty as TupleType:
      return ty.elems.reduce(1, { max($0, alignment(of: $1.type)) })

    default:
      return MemoryLayout<Int>.alignment
    }
  }

  /// Returns the number of bytes from the start of one runtime instance of `type` to the start of
  /// the next when stored in contiguous memory.
  func stride(of type: VILType) -> Int {
    let s = size(of: type).roundedAwayFromZero(toNearestMultipleOf: alignment(of: type))
    return max(s, 1)
  }

  /// Returns the number of bytes from the start of one runtime instance of `type` to the start of
  /// the next when stored in contiguous memory.
  private func stride(of type: ValType) -> Int {
    let s = size(of: type).roundedAwayFromZero(toNearestMultipleOf: alignment(of: type))
    return max(s, 1)
  }

  /// Returns the offset of a stored property within a type's memory representation.
  func offset(of memberName: String, in type: VILType) -> Int? {
    guard type.isObject && !type.isExistential else { return nil }

    switch type.valType {
    case let ty as ProductType:
      var acc = 0
      for decl in ty.decl.storedVars {
        if decl.name == memberName { return acc }
        let align = alignment(of: decl.type)
        acc += size(of: decl.type).roundedAwayFromZero(toNearestMultipleOf: align)
      }

    case is TupleType:
      fatalError("not implemented")

    default:
      break
    }

    return nil
  }

  /// Returns the offset of an element at the given index within the memory representation of a
  /// compound type.
  func offset<S>(
    elementAtIndex index: Int, in compound: S
  ) -> Int? where S: Sequence, S.Element == VILType {
    var acc = 0
    for (i, type) in compound.enumerated() {
      if i == index { return acc }
      let align = alignment(of: type)
      acc += size(of: type).roundedAwayFromZero(toNearestMultipleOf: align)
    }
    return nil
  }

  /// Returns the offsets of every element within the memory representation of a compound type.
  ///
  /// The returned array contains 1 more element than `compound`, representing the total byte count
  /// of the compound type.
  func offsets<S>(in compound: S) -> [Int] where S: Sequence, S.Element == VILType {
    var result: [Int] = []
    var acc = 0
    for type in compound {
      result.append(acc)
      let align = alignment(of: type)
      acc += size(of: type).roundedAwayFromZero(toNearestMultipleOf: align)
    }
    result.append(acc)
    return result
  }

}

extension VILType {

  static func == (lhs: VILType, rhs: VILType) -> Bool {
    return (lhs.valType === rhs.valType) && (lhs.isAddress == rhs.isAddress)
  }

}
