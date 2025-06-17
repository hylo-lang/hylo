import FrontEnd

protocol TargetABI {

  func layout(_ t: BuiltinType) ->TypeLayout.Bytes

}

struct UnrealABI: TargetABI {

  init() {}

  let bitsInAWord = 64
  let maxAlignment = 128 / 8

  /// Returns the layout for a `bitWidth`-bit type.
  ///
  /// - Precondition: `bitWidth` is a power of 2.
  private func layout(bitWidth: Int) -> TypeLayout.Bytes {
    precondition(
      bitWidth > 0 && bitWidth.nonzeroBitCount == 1,
      "bit width \(bitWidth) is not a power of 2.")
    return .init(
      alignment: min(bitWidth / 8, maxAlignment),
      size: bitWidth / 8)
  }

  func layout(_ t: BuiltinType) -> TypeLayout.Bytes {
    let bitWidth = switch t {
    case .i(let w): w
    case .word: bitsInAWord
    case .float16: 16
    case .float32: 32
    case .float64: 64
    case .float128: 128
    case .ptr: bitsInAWord
    case .module: bitsInAWord
    }
    return layout(bitWidth: bitWidth)
  }

}

extension TargetABI {

  /// Returns a discriminator type for the union of `n` types.
  ///
  /// Precondition: `n` is positive
  func unionDiscriminator(count n: Int) -> AnyType {
    if n == 1 { return .void }
    let bitsNeeded = UInt(n - 1).bitsInRepresentation
    // Integer sizes are a contiguous range of powers of 2 starting with 8
    let integerSize = max(8, bitsNeeded.roundedUpToPowerOf2)
    return .init(BuiltinType.i(Int(integerSize)))
  }

}
