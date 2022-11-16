import Foundation

protocol CodableMetatypeWrapperProtocol: AnyObject {
  static var wrappedType: Codable.Type { get }
}

private class CodableMetatypeWrapper<T: Codable>: CodableMetatypeWrapperProtocol {
  static var wrappedType: Codable.Type { T.self }
}
extension Decodable where Self: Encodable {
  static var typeKey: String { NSStringFromClass(CodableMetatypeWrapper<Self>.self) }
}

public struct NodeKind: Codable, Equatable, Hashable {
  let value: Node.Type

  init(_ value: Node.Type) {
    self.value = value
  }
  
  public func encode(to encoder: Encoder) throws {
    var c = encoder.singleValueContainer()
    try c.encode(value.typeKey)
  }
  
  public init(from decoder: Decoder) throws {
    let typeKey = try decoder.singleValueContainer().decode(String.self)
    
    guard let wrapperClass = NSClassFromString(typeKey) as? CodableMetatypeWrapperProtocol.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "Result of NSClassFromString was nil or was not a CodableMetatypeWrapperProtocol.",
          underlyingError: nil))
    }
    
    guard let nodeType = wrapperClass.wrappedType as? Node.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: decoder.codingPath,
          debugDescription: "\(wrapperClass.wrappedType) is not a Node type.",
          underlyingError: nil))
      
    }
    self.value = nodeType
  }

  public static func == (l: Self, r: Self) -> Bool {
    return l.value == r.value
  }

  public func hash(into h: inout Hasher) {
    ObjectIdentifier(value).hash(into: &h)
  }
  
  static func == (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }
  
  static func != (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }

  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r.value
  }
  
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r.value
  }

  // MARK: Declarations

  public static let associatedTypeDecl = NodeKind(AssociatedTypeDecl.self)
  public static let associatedValueDecl = NodeKind(AssociatedValueDecl.self)
  public static let bindingDecl = NodeKind(BindingDecl.self)
  public static let builtinDecl = NodeKind(BuiltinDecl.self)
  public static let conformanceDecl = NodeKind(ConformanceDecl.self)
  public static let extensionDecl = NodeKind(ExtensionDecl.self)
  public static let functionDecl = NodeKind(FunctionDecl.self)
  public static let genericTypeParamDecl = NodeKind(GenericTypeParamDecl.self)
  public static let genericValueParamDecl = NodeKind(GenericValueParamDecl.self)
  public static let importDecl = NodeKind(ImportDecl.self)
  public static let initializerDecl = NodeKind(InitializerDecl.self)
  public static let methodDecl = NodeKind(MethodDecl.self)
  public static let methodImplDecl = NodeKind(MethodImplDecl.self)
  public static let moduleDecl = NodeKind(ModuleDecl.self)
  public static let namespaceDecl = NodeKind(NamespaceDecl.self)
  public static let operatorDecl = NodeKind(OperatorDecl.self)
  public static let parameterDecl = NodeKind(ParameterDecl.self)
  public static let productTypeDecl = NodeKind(ProductTypeDecl.self)
  public static let subscriptDecl = NodeKind(SubscriptDecl.self)
  public static let subscriptImplDecl = NodeKind(SubscriptImplDecl.self)
  public static let traitDecl = NodeKind(TraitDecl.self)
  public static let typeAliasDecl = NodeKind(TypeAliasDecl.self)
  public static let varDecl = NodeKind(VarDecl.self)

  // MARK: Value expressions

  public static let assignExpr = NodeKind(AssignExpr.self)
  public static let asyncExpr = NodeKind(AsyncExpr.self)
  public static let awaitExpr = NodeKind(AwaitExpr.self)
  public static let booleanLiteralExpr = NodeKind(BooleanLiteralExpr.self)
  public static let bufferLiteralExpr = NodeKind(BufferLiteralExpr.self)
  public static let castExpr = NodeKind(CastExpr.self)
  public static let condExpr = NodeKind(CondExpr.self)
  public static let errorExpr = NodeKind(ErrorExpr.self)
  public static let floatLiteralExpr = NodeKind(FloatLiteralExpr.self)
  public static let funCallExpr = NodeKind(FunCallExpr.self)
  public static let inoutExpr = NodeKind(InoutExpr.self)
  public static let integerLiteralExpr = NodeKind(IntegerLiteralExpr.self)
  public static let lambdaExpr = NodeKind(LambdaExpr.self)
  public static let mapLiteralExpr = NodeKind(MapLiteralExpr.self)
  public static let matchExpr = NodeKind(MatchExpr.self)
  public static let nameExpr = NodeKind(NameExpr.self)
  public static let nilExpr = NodeKind(NilExpr.self)
  public static let sequenceExpr = NodeKind(SequenceExpr.self)
  public static let storedProjectionExpr = NodeKind(StoredProjectionExpr.self)
  public static let stringLiteralExpr = NodeKind(StringLiteralExpr.self)
  public static let subscriptCallExpr = NodeKind(SubscriptCallExpr.self)
  public static let tupleExpr = NodeKind(TupleExpr.self)
  public static let tupleMemberExpr = NodeKind(TupleMemberExpr.self)
  public static let unicodeScalarLiteralExpr = NodeKind(UnicodeScalarLiteralExpr.self)

  // MARK: Patterns

  public static let bindingPattern = NodeKind(BindingPattern.self)
  public static let exprPattern = NodeKind(ExprPattern.self)
  public static let namePattern = NodeKind(NamePattern.self)
  public static let tuplePattern = NodeKind(TuplePattern.self)
  public static let wildcardPattern = NodeKind(WildcardPattern.self)

  // MARK: Statements

  public static let braceStmt = NodeKind(BraceStmt.self)
  public static let breakStmt = NodeKind(BreakStmt.self)
  public static let condBindingStmt = NodeKind(CondBindingStmt.self)
  public static let continueStmt = NodeKind(ContinueStmt.self)
  public static let declStmt = NodeKind(DeclStmt.self)
  public static let discardStmt = NodeKind(DiscardStmt.self)
  public static let doWhileStmt = NodeKind(DoWhileStmt.self)
  public static let exprStmt = NodeKind(ExprStmt.self)
  public static let forStmt = NodeKind(ForStmt.self)
  public static let returnStmt = NodeKind(ReturnStmt.self)
  public static let whileStmt = NodeKind(WhileStmt.self)
  public static let yieldStmt = NodeKind(YieldStmt.self)

  // MARK: Type expressions

  /// The kind of type expression nodes.

  public static let asyncTypeExpr = NodeKind(AsyncTypeExpr.self)
  public static let conformanceLensTypeExpr = NodeKind(ConformanceLensTypeExpr.self)
  public static let existentialTypeExpr = NodeKind(ExistentialTypeExpr.self)
  public static let indirectTypeExpr = NodeKind(IndirectTypeExpr.self)
  public static let lambdaTypeExpr = NodeKind(LambdaTypeExpr.self)
  public static let nameTypeExpr = NodeKind(NameTypeExpr.self)
  public static let parameterTypeExpr = NodeKind(ParameterTypeExpr.self)
  public static let storedProjectionTypeExpr = NodeKind(StoredProjectionTypeExpr.self)
  public static let tupleTypeExpr = NodeKind(TupleTypeExpr.self)
  public static let unionTypeExpr = NodeKind(UnionTypeExpr.self)
  public static let wildcardTypeExpr = NodeKind(WildcardTypeExpr.self)

  // MARK: Others

  public static let matchCase = NodeKind(MatchCase.self)
  public static let topLevelDeclSet = NodeKind(TopLevelDeclSet.self)
}

extension NodeKind: CustomStringConvertible {

  public var description: String { String(describing: value) }

}
