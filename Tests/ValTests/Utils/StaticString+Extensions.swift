extension String {

  /// Creates an instance copying `s`.
  init(_ s: StaticString) {
    self = s.withUTF8Buffer({ .init(decoding: $0, as: UTF8.self) })
  }

}
