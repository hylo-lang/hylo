extension Diag {

  static func noType(
    named ident: String, range: SourceRange?
  ) -> Diag {
    return Diag("no type named '\(ident)' in scope", anchor: range)
  }

  static func noType(
    named ident: String, in space: TypeDecl, range: SourceRange?
  ) -> Diag {
    return Diag("type '\(space.ident)' has no member type named '\(ident)'", anchor: range)
  }

  static func cannotIntrospect(space: TypeDecl, range: SourceRange?) -> Diag {
    return Diag("cannot introspect '\(space.ident)'", anchor: range)
  }

}
