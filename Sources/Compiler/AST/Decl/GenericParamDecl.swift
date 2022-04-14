/// The ID of a generic parameter declaration.
public enum GenericParamDeclID: Hashable {

  case size(NodeID<GenericSizeParamDecl>)

  case type(NodeID<GenericTypeParamDecl>)

}
