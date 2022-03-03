extension Diag {

  static func noType(
    named name: String, range: SourceRange?
  ) -> Diag {
    return Diag("no type named '\(name)' in scope", anchor: range)
  }

  static func noType(
    named name: String, in space: TypeDecl, range: SourceRange?
  ) -> Diag {
    return Diag("type '\(space.name)' has no member type named '\(name)'", anchor: range)
  }

  static func cannotIntrospect(space: TypeDecl, range: SourceRange?) -> Diag {
    return Diag("cannot introspect '\(space.name)'", anchor: range)
  }

}
