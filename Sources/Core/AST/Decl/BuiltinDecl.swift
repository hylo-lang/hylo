/// An abstract node representing a built-in declaration.
public struct BuiltinDecl: Decl {

  public init() {}

  public var site: SourceRange { Self.siteConstant }

  /// A synthesized site for all instances.
  private static let siteConstant = SourceFile(synthesizedText: "/* built-in */").wholeRange

}
