import BigInt
import Utils

extension WideUInt {

  /// Creates an instance from a Hylo integer literal `s` with given `bitWidth`, applying two's
  /// complement to represent negative numbers; fails if the representation of `s` requires more
  /// too many bits or if `s` is negative and `signed` is false.
  ///
  /// - Requires: `s` must be a valid Hylo integer literal.
  init?<S: StringProtocol>(hyloLiteral s: S, signed: Bool, bitWidth: Int) {
    guard
      let bits = BigUInt.parse(
        hyloLiteral: String(s.filter({ $0 != "_" })), signed: signed, bitWidth: bitWidth)
    else { return nil }
    self.init(truncatingIfNeeded: bits, toWidth: bitWidth)
  }

}

extension BigUInt {

  /// Creates an instance from a Hylo integer literal `s` with given `bitWidth`, applying two's
  /// complement to represent negative numbers; fails if the representation of `s` requires more
  /// too many bits or if `s` is negative and `signed` is false.
  ///
  /// - Requires: `s` must be a valid Hylo integer literal.
  fileprivate static func parse<S: StringProtocol>(
    hyloLiteral s: S, signed: Bool, bitWidth: Int
  ) -> Self? {
    let result: BigUInt

    if s.starts(with: "-") {
      // Parse the positive part.
      guard
        signed,
        let p = BigUInt.parse(hyloLiteral: s.dropFirst(), signed: false, bitWidth: bitWidth + 1)
      else { return nil }

      // Nothing else to do if there are no digits in the pattern.
      guard let h = p.words.last else { return p }

      // Compute two's complement.
      var words = p.words.dropLast().map({ ~$0 })
      let x = p.words.count * BigUInt.Word.bitWidth
      if x > bitWidth {
        let usefulBits = ~BigUInt.Word() >> BigUInt.Word(x - bitWidth)
        if (p.bitWidth == bitWidth) && (h & (usefulBits >> 1) != .zero) { return nil }
        words.append(~h & usefulBits)
      } else {
        let padding = (bitWidth - 1) / BigUInt.Word.bitWidth + 1 - p.words.count
        words.append(~h)
        words.append(contentsOf: Array(repeating: ~BigUInt.Word(), count: padding))
      }

      result = .init(words: words) + 1
      return (result.bitWidth <= bitWidth) ? result : nil
    }

    if s.starts(with: "0b") {
      result = .init(s.dropFirst(2), radix: 2)!
    } else if s.starts(with: "0o") {
      result = .init(s.dropFirst(2), radix: 8)!
    } else if s.starts(with: "0x") {
      result = .init(s.dropFirst(2), radix: 16)!
    } else {
      result = .init(s)!
    }

    if signed {
      return (result.bitWidth < bitWidth) ? result : nil
    } else {
      return (result.bitWidth <= bitWidth) ? result : nil
    }
  }

}
