import FrontEnd

/// Types that describe the ABI for which we might interpret code.
public protocol TargetABI {

  /// Returns the layout of `t`.
  func layout(_ t: BuiltinType) -> TypeLayout.Bytes

}

/// An ABI we can use to interpret code when matching some real ABI doesn't matter.
public struct UnrealABI: TargetABI {

  /// An instance.
  public init() {}

  /// The size of a word in `bits`
  private let bitsInAWord = 64

  /// The maximal alignment of a builtin type in bytes.
  private let maxAlignment = 128 / 8

  /// Returns the layout for a `bitWidth`-bit type.
  ///
  /// - Precondition: `bitWidth` is a power of 2.
  private func layout(bitWidth: Int) -> TypeLayout.Bytes {
    precondition(
      bitWidth > 0 && bitWidth.nonzeroBitCount == 1,
      "bit width \(bitWidth) is not a power of 2.")
    let sizeInBytes = (bitWidth + 7) / 8
    return .init(
      alignment: min(sizeInBytes, maxAlignment),
      size: sizeInBytes)
  }

  public func layout(_ t: BuiltinType) -> TypeLayout.Bytes {
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
  public func unionDiscriminator(count n: Int) -> AnyType {
    if n == 1 { return .void }
    let bitsNeeded = UInt(n - 1).bitsInRepresentation
    // Integer sizes are a contiguous range of powers of 2 starting with 8
    let integerSize = max(8, bitsNeeded.roundedUpToPowerOf2)
    return .init(BuiltinType.i(Int(integerSize)))
  }

}
