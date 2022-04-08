/// The index of a generic parameter declaration.
public enum GenericParamDeclIndex: Hashable {

  case type(DeclIndex<GenericTypeParamDecl>)

  case size(DeclIndex<GenericSizeParamDecl>)

}
