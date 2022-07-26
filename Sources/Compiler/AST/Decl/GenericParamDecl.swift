/// The ID of a generic parameter declaration.
public enum GenericParamDeclID: Codable {

  case type(NodeID<GenericTypeParamDecl>)

  case value(NodeID<GenericValueParamDecl>)

}
