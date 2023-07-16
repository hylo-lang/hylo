/// A one or two letter identifier specifying how to interpret mangled data.
public enum ManglingOperator: String {

  case productTypeDecl = "A"

  case traitDecl = "C"

  case conformanceDecl = "cD"

  case extensionDecl = "eD"

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

  case directDeclReference = "dQ"

  case reserved = "R"

  case typealiasDecl = "S"

  case productType = "aT"

  case boundGenericType = "bT"

  case traitType = "cT"

  case existentialTraitType = "eT"

  case existentialGenericType = "egT"

  case existentialMetatype = "emT"

  case genericTypeParameterType = "gT"

  case lambdaType = "lT"

  case metatypeType = "mT"

  case parameterType = "pT"

  case remoteType = "rT"

  case subscriptType = "sT"

  case subscriptImplType = "siT"

  case tupleType = "tT"

  case sumType = "uT"

  case translatonUnit = "U"

  case varDecl = "V"

  case whereClause = "W"

  case conformanceConstraint = "cW"

  case equalityConstraint = "eW"

  case valueConstraint = "vW"

  case anonymousScope = "Y"

  case endOfSequence = "Z"

}

extension ManglingOperator: TextOutputStreamable {

  public func write<T: TextOutputStream>(to output: inout T) {
    output.write(rawValue)
  }

}
