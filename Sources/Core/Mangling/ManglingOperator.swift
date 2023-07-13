/// A one or two letter identifier specifying how to interpret mangled data.
enum ManglingOperator: String {

  case productTypeDecl = "A"

  case productType = "B"

  case functionDecl = "F"

  case anonymousFunctionDecl = "aF"

  case staticFunctionDecl = "sF"

  case lookup = "K"

  case lambdaType = "L"

  case thinLambdaType = "tL"

  case moduleDecl = "M"

  case namespaceDecl = "N"

  case parameterDecl = "P"

  case parameterType = "pT"

  case remoteType = "rT"

  case translatonUnit = "U"

  case endOfSequence = "Z"

}

extension ManglingOperator: TextOutputStreamable {

  public func write<T: TextOutputStream>(to output: inout T) {
    output.write(rawValue)
  }

}
