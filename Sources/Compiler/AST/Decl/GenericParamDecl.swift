/// The ID of a generic parameter declaration.
public enum GenericParamDeclID: Hashable {

  case type(NodeID<GenericTypeParamDecl>)

  case value(NodeID<GenericValueParamDecl>)

}
