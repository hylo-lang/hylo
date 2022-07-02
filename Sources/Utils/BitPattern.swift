/// A bit pattern.
public struct BitPattern: Hashable {

  /// A buffer of words.
  fileprivate struct WordBuffer {

    var storage: ManagedBuffer<(count: Int, width: Int), UInt>

    init(count: Int, width: Int) {
      storage = .create(minimumCapacity: count, makingHeaderWith: { _ in (count, width) })
    }

  }

  /// The internal representation of the bit pattern.
  fileprivate enum Representation: Hashable {

    /// A pair of words.
    case pair(lower: UInt, upper: UInt, width: UInt8)

    /// An array of words, from the least significant to the most significant.
    case list(WordBuffer)

  }

  /// The representation of the pattern.
  private var representation: Representation

  /// Creates a bit pattern with the `width` least significant bits of `pattern`.
  ///
  /// - Requires: `width <= UInt.bitWidth`
  public init(pattern: UInt, width: Int) {
    precondition(width <= UInt.bitWidth)
    self.representation = .pair(
      lower: pattern & ~(~0 << width),
      upper: 0,
      width: UInt8(truncatingIfNeeded: width))
  }

  /// Creates a bit pattern with `width` least significant bits of `pattern`.
  ///
  /// - Requires: `width <= pattern.words.count * UInt.bitWidth`
  public init<T: BinaryInteger>(pattern: T, width: Int) {
    self.init(words: pattern.words, width: width)
  }

  /// Creates a bit pattern from a collection of words ordered from least significant to most
  /// significant, retaining only `width` least significant bits.
  ///
  /// - Requires: `width <= words.count * UInt.bitWidth`
  public init<T: Collection>(words: T, width: Int) where T.Element == UInt {
    precondition(width <= words.count * UInt.bitWidth)

    if (width == 0) || (words.count == 0) {
      representation = .pair(lower: 0, upper: 0, width: 0)
    } else {
      let count = (width + UInt.bitWidth - 1) / UInt.bitWidth
      assert(count > 0)

      func normalize(upper: inout UInt) {
        if 2 * UInt.bitWidth > width {
          let upperWidth = width - (count - 1) * UInt.bitWidth
          upper &= ~(~0 << upperWidth)
        }
      }

      if count == 1 {
        representation = .pair(
          lower: words[words.startIndex],
          upper: 0,
          width: UInt8(truncatingIfNeeded: width))
      } else if count == 2 {
        var upper = words[words.index(after: words.startIndex)]
        normalize(upper: &upper)
        representation = .pair(
          lower: words[words.startIndex],
          upper: upper,
          width: UInt8(truncatingIfNeeded: width))
      } else {
        let buffer = WordBuffer(count: count, width: width)
        buffer.storage.withUnsafeMutablePointerToElements({ elements in
          for (i, word) in words.prefix(count).enumerated() {
            elements.advanced(by: i).initialize(to: word)
          }
          normalize(upper: &elements[count - 1])
        })
        representation = .list(buffer)
      }
    }
  }

  /// Creates a bit pattern from a string describing a positive decimal number, with the minimal
  /// width required to represent the number described by`string`.
  ///
  /// - Returns: A bit pattern representing the number described by `string`, unless it contains
  ///   non-decimal characters. In that case, returns `nil`.
  public init?(fromDecimal string: String) {
    // Transform the string into an array of digits, ignoring leading zeroes.
    var input: [UInt8] = []
    input.reserveCapacity(input.count)

    var isLeading = true
    for character in string {
      guard let ascii = character.asciiValue,
            ascii >= 0x30 && ascii <= 0x39
      else { return nil }

      if !isLeading || ascii > 0x30 {
        isLeading = false
        input.append(ascii - 0x30)
      }
    }

    // Convert the digits to binary.
    var words: [UInt] = [0]
    var width = 0

    while let last = input.last {
      // Determine the value of the next bit.
      width += 1
      if width / UInt.bitWidth > words.count {
        words.append(0)
      }

      if (last % 2) != 0 {
        let i = (width - 1) / UInt.bitWidth
        words[i] = words[i] | (1 << ((width - 1) - i * UInt.bitWidth))
      }

      // Divide the input by 2.
      var newInput: [UInt8] = []
      var additive: UInt8 = 0
      for digit in input {
        newInput.append(digit / 2 + additive)
        additive = (digit % 2) != 0 ? 5 : 0
      }

      input = newInput.first == 0
        ? Array(newInput[1...])
        : newInput
    }

    self = BitPattern(words: words, width: width)
  }

  /// Returns a bit pattern resized to `width`, truncating the most significant bits or adding
  /// leading zeroes as necessary.
  public func resized(to width: Int) -> BitPattern {
    if width == self.width {
      return self
    } else {
      let count = (width + UInt.bitWidth - 1) / UInt.bitWidth
      let words = self.words
      return BitPattern(
        words: words + Array(repeating: 0, count: count - words.count),
        width: width)
    }
  }

  public var words: [UInt] {
    switch representation {
    case .pair(let lower, let upper, let width):
      return width <= UInt.bitWidth
        ? [lower]
        : [lower, upper]

    case .list(let buffer):
      return buffer.storage.withUnsafeMutablePointers({ (header, elements) in
        Array(UnsafeMutableBufferPointer(start: elements, count: header.pointee.count))
      })
    }
  }

  public var width: Int {
    switch representation {
    case .pair(_, _, let width):
      return Int(width)
    case .list(let words):
      return words.storage.header.width
    }
  }

}

extension BitPattern: MutableCollection, RandomAccessCollection {

  public typealias Index = Int

  public typealias Element = Bool

  public var startIndex: Int { 0 }

  public var endIndex: Int { width }

  public func index(after position: Int) -> Int {
    position + 1
  }

  public subscript(position: Int) -> Bool {
    get {
      switch representation {
      case .pair(let lower, let upper, let width):
        precondition(position >= 0 && position < Int(width), "index is out of bounds")
        return position < UInt.bitWidth
          ? lower & (1 << position) != 0
          : upper & (1 << (position - UInt.bitWidth)) != 0

      case .list(let words):
        precondition(
          position >= 0 && position < words.storage.header.width, "index is out of bounds")
        return words.storage.withUnsafeMutablePointerToElements({ elements in
          let i = position / UInt.bitWidth
          return elements[i] & (1 << (position - i * UInt.bitWidth)) != 0
        })
      }
    }

    set {
      switch representation {
      case .pair(var lower, var upper, let width):
        precondition(position >= 0 && position < Int(width), "index is out of bounds")
        if position < UInt.bitWidth {
          lower = newValue
            ? lower | (1 << position)
            : lower & ~(1 << position)
        } else {
          upper = newValue
            ? lower | (1 << (position - UInt.bitWidth))
            : lower & ~(1 << (position - UInt.bitWidth))
        }
        representation = .pair(lower: lower, upper: upper, width: width)

      case .list(let words):
        precondition(
          position >= 0 && position < words.storage.header.width, "index is out of bounds")
        words.storage.withUnsafeMutablePointerToElements({ elements in
          let i = position / UInt.bitWidth
          elements[i] = newValue
            ? elements[i] | (1 << (position - i * UInt.bitWidth))
            : elements[i] & ~(1 << (position - i * UInt.bitWidth))
        })
      }
    }
  }

}

extension BitPattern: CustomStringConvertible {

  public var description: String {
    switch representation {
    case .pair(let lower, let upper, _):
      let lsw = String(lower, radix: 2)
      if width <= UInt.bitWidth {
        let padding = String(repeating: "0", count: width - lsw.count)
        return padding + lsw
      } else {
        let padding = String(repeating: "0", count: UInt.bitWidth - lsw.count)
        return String(upper, radix: 2) + padding + lsw
      }

    case .list(let words):
      return words.storage.withUnsafeMutablePointers({ (header, elements) in
        return (0 ..< (header.pointee.count - 1)).reversed().reduce(
          into: String(elements[count - 1], radix: 2),
          { (result, i) in
            let s = String(elements[i], radix: 2)
            result += String(repeating: "0", count: UInt.bitWidth - s.count)
            result += s
          })
      })
    }
  }

}

extension BitPattern: CustomReflectable {

  public var customMirror: Mirror {
    Mirror(self, unlabeledChildren: self, displayStyle: .collection)
  }

}

extension BitPattern.WordBuffer: Equatable {

  static func == (l: Self, r: Self) -> Bool {
    l.storage.withUnsafeMutablePointers({ (leftHeader, leftElements) in
      r.storage.withUnsafeMutablePointers({ (rightHeader, rightElements) in
        if leftHeader.pointee.width != rightHeader.pointee.width { return false }
        for i in 0 ..< leftHeader.pointee.count {
          if leftElements[i] != rightElements[i] { return false }
        }
        return true
      })
    })
  }

}

extension BitPattern.WordBuffer: Hashable {

  func hash(into hasher: inout Hasher) {
    storage.withUnsafeMutablePointers({ (header, elements) in
      hasher.combine(header.pointee.count)
      hasher.combine(header.pointee.width)
      for i in 0 ..< header.pointee.count {
        hasher.combine(elements[i])
      }
    })
  }

}
