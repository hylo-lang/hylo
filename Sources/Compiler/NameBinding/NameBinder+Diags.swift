extension Diag {

  static func bindingUsedBeforeDeclaration(_ ident: LabeledIdent, range: SourceRange?) -> Diag {
    Diag("binding named '\(ident)' used before declaration", anchor: range)
  }

  static func noBinding(named ident: LabeledIdent, range: SourceRange?) -> Diag {
    Diag("no binding named '\(ident)' in scope", anchor: range)
  }

  static func noBinding(
    named ident: LabeledIdent,
    in space: TypeDecl,
    range: SourceRange?
  ) -> Diag {
    Diag("type '\(space.ident)' has no member named '\(ident)'", anchor: range)
  }

  static func noType(named ident: String, range: SourceRange?) -> Diag {
    Diag("no type named '\(ident)' in scope", anchor: range)
  }

  static func noType(named ident: String, in space: TypeDecl, range: SourceRange?) -> Diag {
    Diag("type '\(space.ident)' has no member type named '\(ident)'", anchor: range)
  }

  static func noType(named ident: String, in type: ValType, range: SourceRange?) -> Diag {
    Diag("type '\(type)' has no member type named '\(ident)'", anchor: range)
  }

}
