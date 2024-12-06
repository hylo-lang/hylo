import FrontEnd
import Utils

/// A one or two letter identifier specifying how to interpret mangled data.
public enum ManglingOperator: String {

  case productTypeDecl = "A"

  case bindingDecl = "bD"

  case traitDecl = "C"

  case conformanceDecl = "cD"

  case extensionDecl = "eD"

  case importDecl = "iD"

  case associatedTypeDecl = "taD"

  case associatedValueDecl = "vaD"

  case functionDecl = "F"

  case existentializedFunctionDecl = "eF"

  case methodDecl = "hF"

  case methodImpl = "iF"

  case monomorphizedFunctionDecl = "mF"

  case staticFunctionDecl = "sF"

  case synthesizedFunctionDecl = "xF"

  case genericParameterDecl = "G"

  case propertyDecl = "pH"

  case subscriptDecl = "H"

  case subscriptImpl = "iH"

  case memberwiseInitializerDecl = "I"

  case lookup = "K"

  case lookupRelative = "rK"

  case moduleDecl = "M"

  case namespaceDecl = "N"

  case parameterDecl = "P"

  case directDeclReference = "dQ"

  case reserved = "R"

  case typealiasDecl = "S"

  case productType = "aT"

  case associatedType = "aaT"

  case boundGenericType = "bT"

  case builtinIntegerType = "biT"

  case builtinFloatType = "bfT"

  case builtinPointerType = "bpT"

  case builtinModuleType = "bmT"

  case builtinWordType = "bwT"

  case traitType = "cT"

  case bufferType = "dT"

  case existentialTraitType = "eT"

  case existentialGenericType = "egT"

  case existentialMetatype = "emT"

  case genericTypeParameterType = "gT"

  case methodType = "hT"

  case arrowType = "lT"

  case metatypeType = "mT"

  case parameterType = "pT"

  case remoteType = "rT"

  case subscriptType = "sT"

  case subscriptImplType = "siT"

  case tupleType = "tT"

  case unionType = "uT"

  case translatonUnit = "U"

  case varDecl = "V"

  case whereClause = "W"

  case conformanceConstraint = "cW"

  case equalityConstraint = "eW"

  case valueConstraint = "vW"

  case witnessTable = "wW"

  case anonymousScope = "Y"

  case endOfSequence = "Z"

  /// Creates the operator corresponding to `d`.
  init<T: SingleEntityDecl>(for d: T.Type) {
    switch NodeKind(d) {
    case AssociatedTypeDecl.self:
      self = .associatedTypeDecl
    case AssociatedValueDecl.self:
      self = .associatedValueDecl
    case GenericParameterDecl.self:
      self = .genericParameterDecl
    case ImportDecl.self:
      self = .importDecl
    case ModuleDecl.self:
      self = .moduleDecl
    case NamespaceDecl.self:
      self = .namespaceDecl
    case ParameterDecl.self:
      self = .parameterDecl
    case ProductTypeDecl.self:
      self = .productTypeDecl
    case TraitDecl.self:
      self = .traitDecl
    case TypeAliasDecl.self:
      self = .typealiasDecl
    case VarDecl.self:
      self = .varDecl
    default:
      unreachable()
    }
  }

}

extension ManglingOperator: TextOutputStreamable {

  public func write<T: TextOutputStream>(to output: inout T) {
    output.write(rawValue)
  }

}
