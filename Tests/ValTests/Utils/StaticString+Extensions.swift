extension String {

  /// Creates an instance having the value of `s`.
  init(_ s: StaticString) {
    self = s.withUTF8Buffer({ .init(decoding: $0, as: UTF8.self) })
  }

}
