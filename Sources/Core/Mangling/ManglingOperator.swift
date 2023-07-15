/// A one or two letter identifier specifying how to interpret mangled data.
public enum ManglingOperator: String {

  case productTypeDecl = "A"

  case traitDecl = "C"

  case importDecl = "iD"

  case associatedTypeDecl = "taD"

  case associatedValueDecl = "vaD"

  case functionDecl = "F"

  case staticFunctionDecl = "sF"

  case genericParameterDecl = "G"

  case propertyDecl = "pH"

  case subscriptDecl = "H"

  case subscriptImpl = "iH"

  case memberwiseInitializerDecl = "I"

  case lookup = "K"

  case moduleDecl = "M"

  case namespaceDecl = "N"

  case parameterDecl = "P"

  case reserved = "R"

  case typealiasDecl = "S"

  case productType = "aT"

  case boundGenericType = "bT"

  case lambdaType = "lT"

  case thinLambdaType = "mT"

  case parameterType = "pT"

  case remoteType = "rT"

  case subscriptType = "sT"

  case subscriptImplType = "siT"

  case tupleType = "tT"

  case sumType = "uT"

  case translatonUnit = "U"

  case varDecl = "V"

  case anonymousScope = "Y"

  case endOfSequence = "Z"

}

extension ManglingOperator: TextOutputStreamable {

  public func write<T: TextOutputStream>(to output: inout T) {
    output.write(rawValue)
  }

}
