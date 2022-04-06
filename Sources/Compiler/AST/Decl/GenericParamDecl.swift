/// The index of a generic parameter declaration.
public enum GenericParamDeclIndex {

  case type(DeclIndex<GenericTypeParamDecl>)

  case size(DeclIndex<GenericSizeParamDecl>)

}
