extension String.StringInterpolation {

  public mutating func appendInterpolation<L: Sequence>(list: L) {
    var first = true
    for x in list {
      if first { first = false } else { appendLiteral(", ") }
      appendLiteral(String(describing: x))
    }
  }

}
