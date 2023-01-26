/// An abstract node representing a built-in declaration.
public struct BuiltinDecl: Decl {

  public let site: SourceRange

  public init() {
    self.site = Self.source.range(Self.source.text.startIndex ..< Self.source.text.endIndex)
  }

  /// A synthesized source file to define the site of this type's instances.
  private static let source = SourceFile(synthesizedText: "/* built-in declaration */")

}
