/// The index of a generic parameter declaration.
public enum GenericParamDeclIndex: Hashable {

  case type(NodeIndex<GenericTypeParamDecl>)

  case size(NodeIndex<GenericSizeParamDecl>)

}
