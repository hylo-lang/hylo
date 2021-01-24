public typealias SourceRange = Range<String.Index>

extension SourceRange {

  public static var invalid: SourceRange {
    let i = "".startIndex
    return i ..< i
  }

}
