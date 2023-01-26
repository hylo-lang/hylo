/// An abstract node representing a built-in declaration.
public struct BuiltinDecl: Decl {

  public init() {}

  public var site: SourceRange { Self.site }

  /// A synthesized site for all instances.
  private static let site = SourceFile(synthesizedText: "/* built-in declaration */").wholeRange

}
