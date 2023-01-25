/// An abstract node representing a built-in declaration.
public struct BuiltinDecl: Decl {

  public let site: SourceRange

  public init() {
    let f = SourceFile(synthesizedText: "/* built-in declaration */")
    self.site = f.range(f.text.startIndex ..< f.text.endIndex)
  }

}
