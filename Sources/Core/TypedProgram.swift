/// A data structure representing a typed Val program ready to be lowered.
public struct TypedProgram: Program {

  public let ast: AST

  public let scopeToParent: ASTProperty<AnyScopeID>

  public let scopeToDecls: ASTProperty<[AnyDeclID]>

  public let declToScope: DeclProperty<AnyScopeID>

  public let exprToScope: [NameExpr.ID: AnyScopeID]

  public let varToBinding: [VarDecl.ID: BindingDecl.ID]

  /// A map from translation unit to its imports.
  public let imports: [TranslationUnit.ID: Set<ModuleDecl.ID>]

  /// The overarching type of each declaration.
  public let declTypes: DeclProperty<AnyType>

  /// The type of each expression.
  public let exprTypes: ExprProperty<AnyType>

  /// A map from module to its synthesized declarations.
  public let synthesizedDecls: [ModuleDecl.ID: [SynthesizedDecl]]

  /// A map from function and subscript declarations to their implicit captures.
  public let implicitCaptures: DeclProperty<[ImplicitCapture]>

  /// A map from name expression to its referred declaration.
  public let referredDecls: [NameExpr.ID: DeclRef]

  /// A map from sequence expressions to their evaluation order.
  public let foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr]

  /// The type relations of the program.
  public let relations: TypeRelations

  /// Val's core library.
  public var coreLibrary: ModuleDecl.Typed? {
    ast.coreLibrary.map({ self[$0] })
  }

  /// Creates a typed program from a scoped program and property maps describing type annotations.
  ///
  /// - Requires: All modules in `program` have been sucessfully typed checked.
  public init(
    annotating program: ScopedProgram,
    imports: [TranslationUnit.ID: Set<ModuleDecl.ID>],
    declTypes: DeclProperty<AnyType>,
    exprTypes: ExprProperty<AnyType>,
    implicitCaptures: DeclProperty<[ImplicitCapture]>,
    synthesizedDecls: [ModuleDecl.ID: [SynthesizedDecl]],
    referredDecls: [NameExpr.ID: DeclRef],
    foldedSequenceExprs: [SequenceExpr.ID: FoldedSequenceExpr],
    relations: TypeRelations
  ) {
    precondition(program.ast.modules.allSatisfy({ declTypes[$0]?.base is ModuleType }))

    self.ast = program.ast
    self.scopeToParent = program.scopeToParent
    self.scopeToDecls = program.scopeToDecls
    self.declToScope = program.declToScope
    self.exprToScope = program.exprToScope
    self.varToBinding = program.varToBinding
    self.imports = imports
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.implicitCaptures = implicitCaptures
    self.synthesizedDecls = synthesizedDecls
    self.referredDecls = referredDecls
    self.foldedSequenceExprs = foldedSequenceExprs
    self.relations = relations
  }

  /// Returns `d`, which declares an explicit capture, paired with its type.
  private func pairedWithType(
    capture d: BindingDecl.ID
  ) -> CallableParameterDecl {
    switch ast[ast[d].pattern].introducer.value {
    case .let:
      return .init(decl: AnyDeclID(d), type: ParameterType(.let, declTypes[d]!))
    case .inout:
      return .init(decl: AnyDeclID(d), type: ParameterType(.inout, declTypes[d]!))
    case .sinklet, .var:
      return .init(decl: AnyDeclID(d), type: ParameterType(.sink, declTypes[d]!))
    }
  }

  /// Returns `d`, which declares a parameter, paired with its type.
  private func pairedWithType(
    parameter d: ParameterDecl.ID
  ) -> CallableParameterDecl {
    .init(decl: AnyDeclID(d), type: ParameterType(declTypes[d]!)!)
  }

  /// Returns the declarations and types of the parameters of `d`'s lifted form.
  ///
  /// If `d` is a member function, its receiver comes first followed by formal parameters, from
  /// left to right. Otherwise, its explicit captures come first, in the order they appear in its
  /// capture list, from left to right. Implicit captures come next, in the order they were found
  /// during type checking. Formal parameters come last, from left to right.
  public func liftedParameters(of d: FunctionDecl.ID) -> [CallableParameterDecl] {
    var result: [CallableParameterDecl] = []
    if let r = ast[d].receiver {
      result.append(pairedWithType(parameter: r))
    } else {
      result.append(contentsOf: ast[d].explicitCaptures.map(pairedWithType(capture:)))
      result.append(contentsOf: implicitCaptures[d]!.map { (c) in
        .init(decl: c.decl, type: ParameterType(c.type))
      })
    }
    result.append(contentsOf: ast[d].parameters.map(pairedWithType(parameter:)))
    return result
  }

  /// Returns the declarations of the parameters of `d`'s lifted form.
  ///
  /// `d`'s receiver comes first and is followed by `d`'s formal parameters, from left to right.
  public func liftedParameters(of d: InitializerDecl.ID) -> [CallableParameterDecl] {
    var result: [CallableParameterDecl] = []
    result.append(pairedWithType(parameter: ast[d].receiver))
    result.append(contentsOf: ast[d].parameters.map(pairedWithType(parameter:)))
    return result
  }

  /// Returns the declarations of the parameters of `d`'s lifted form.
  ///
  /// If `d` is a member subscript, its receiver comes first followed by formal parameters, from
  /// left to right. Otherwise, its explicit captures come first, in the order they appear in its
  /// capture list, from left to right. Implicit captures come next, in the order they were found
  /// during type checking. Formal parameters come last, from left to right.
  public func liftedParameters(of d: SubscriptImpl.ID) -> [CallableParameterDecl] {
    let bundle = SubscriptDecl.ID(declToScope[d]!)!
    var result: [CallableParameterDecl] = []
    if let r = ast[d].receiver {
      result.append(pairedWithType(parameter: r))
    } else {
      result.append(contentsOf: ast[bundle].explicitCaptures.map(pairedWithType(capture:)))
      result.append(contentsOf: implicitCaptures[bundle]!.map { (c) in
        .init(decl: c.decl, type: ParameterType(c.type))
      })
    }
    if let p = ast[bundle].parameters {
      result.append(contentsOf: p.map(pairedWithType(parameter:)))
    }
    return result
  }

}
