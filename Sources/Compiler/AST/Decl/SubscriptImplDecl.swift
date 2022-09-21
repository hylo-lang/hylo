/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl, LexicalScope {

  public static let kind = NodeKind.subscriptImplDecl

  public enum Introducer: Codable {

    case `let`

    case `inout`

    case set

    case sink

    /// The parameter passing convention corresponding to this introducer.
    public var convention: PassingConvention {
      switch self {
      case .let   : return .let
      case .inout : return .inout
      case .set   : return .set
      case .sink  : return .sink
      }
    }

  }

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the subscript, if any.
  public var body: Body?

  /// The declaration of the implicit receiver parameter, if any.
  ///
  /// This property is set during type checking.
  public var receiver: NodeID<ParameterDecl>?

  public init(introducer: SourceRepresentable<Introducer>, body: Body? = nil) {
    self.introducer = introducer
    self.body = body
  }

}
