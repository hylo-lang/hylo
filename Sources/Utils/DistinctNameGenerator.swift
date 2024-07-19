public struct DistinctNameGenerator<Key: Equatable> {

  private var nameToOccurrences: [String: [Key]] = [:]

  public init() {}

  public mutating func name(_ n: String, keyedBy k: Key) -> String {
    modify(&nameToOccurrences[n, default: []]) { (occurrences) in
      if let i = occurrences.firstIndex(of: k) {
        return "\(n)\(Self.superscript(i))"
      } else {
        occurrences.append(k)
        return "\(n)\(Self.superscript(occurrences.count - 1))"
      }
    }
  }

  private static func superscript(_ n: Int) -> String {
    if n == 0 {
      return ""
    } else {
      return String(n).map(digitToSuperscript).joined()
    }
  }

  private static func digitToSuperscript(_ c: Character) -> String {
    switch c.asciiValue! {
    case 48: "⁰"
    case 49: "¹"
    case 50: "²"
    case 51: "³"
    case 52: "⁴"
    case 53: "⁵"
    case 54: "⁶"
    case 55: "⁷"
    case 56: "⁸"
    case 57: "⁹"
    default: unreachable()
    }
  }

}
