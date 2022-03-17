/// Val's type checker.
struct TypeChecker_ {

  /// The state of a type checker.
  struct State {

    /// The modules visible from the type checker.
    var modules: [String: ModuleDecl]

    /// The generic environments of the generic declarations.
    var environments: [Decl.ID: GenericEnvironment] = [:]

  }

  /// The status of a type checking request.
  private enum RequestStatus {

    /// Type checking is in progress.
    case inProgress

    /// Type checking succeeded.
    case success

    /// Type checking failed.
    case failure

  }

  /// The diagnostics of the type errors.
  var diags: [Diag] = []

  /// The name binder.
  var binder: NameBinder

  /// The type realizer.
  var realizer: TypeRealizer_

  /// A cache for type checking requests on declarations.
  private var declRequests: [Decl.ID: RequestStatus] = [:]

  /// Creates a new type checker.
  ///
  /// - Parameters:
  ///   - modules: The modules that are visible during name binding.
  ///   - stdlib: The module representing the standard library, if it is loaded.
  init(modules: [String: ModuleDecl], stdlib: ModuleDecl?) {
    self.binder = NameBinder(modules: modules, stdlib: stdlib)
    self.realizer = TypeRealizer_()
  }

  /// Returns whether type checking `decl` succeeded.
  mutating func check(decl: Decl) -> Bool {
    // Check if we already processed the request.
    switch declRequests[decl.id] {
    case nil:
      declRequests[decl.id] = .inProgress
    case .inProgress:
      fatalError("circular dependency")
    case .success:
      return true
    case .failure:
      return false
    }

    // Dispatch.
    let success: Bool
    switch decl {
    case let decl as ModuleDecl:
      success = check(iterable: decl)
    case let decl as NamespaceDecl:
      success = check(iterable: decl)
    case let decl as ExtensionDecl:
      success = check(iterable: decl)
    case let decl as ProductTypeDecl:
      success = check(productType: decl)
    case let decl as PatternBindingDecl:
      success = check(binding: decl)
    default:
      fatalError("unexpected declaration \(type(of: decl))")
    }

    declRequests[decl.id] = success ? .success : .failure
    return success
  }

  private mutating func check<D>(iterable decl: D) -> Bool where D: IterableDeclSpace {
    decl.decls.reduce(into: true, { (result, member) in
      result = check(decl: member) && result
    })
  }

  private mutating func check(productType decl: ProductTypeDecl) -> Bool {
    var success = true

    // Type check the type's direct members.
    for member in decl.directMembers {
      success = check(decl: member) && success
    }

    // Type check extensions.
    for ext in binder.extensions(of: decl) {
      success = check(iterable: ext) && success
    }

    // TODO: Check the type's conformance

    return success
  }

  private mutating func check(binding decl: PatternBindingDecl) -> Bool {
    var success = true
    var system = ConstraintSystem()

    // If there's a signature, use it as the authoritative type information. Otherwise, infer it
    // from the pattern initializer.
    if let sign = decl.sign {
      // Realizes the type of the signature.
      let type = realizer.realize(sign, useSite: decl.parentDeclSpace!, binder: &binder)

      // Contextualize the type signature.
      print(type)
    }

    return success
  }

  /// Type checks `expr`.
  mutating func check(
    expr: Expr,
    expectedType: ValType,
    useSite: DeclSpace,
    freeTypeVarSubstPolicy: FreeTypeVarSubstPolicy = .bindToError
  ) {
    // Temporarily projects `self` into a pre-checker.
    withBorrowedSelf({ this -> (Self, Expr) in
      var pass = Desugarer(checker: this)
      let (_, expr) = pass.walk(expr: expr)
      return (pass.transformer.checker.release(), expr)
    })
  }

  /// Contextualizes `type` in the context of `useSite`.
  func contextualize(
    type type: ValType,
    from useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> ValType {
    // Nothing to do if the type isn't parameterized.
    guard type[.hasTypeParams] else { return type }


    // Check if we have to synthetize additional generic arguments, in case the signature refers
    // to an "underspecialized" generic nominal type.
    fatalError()
  }

  private mutating func withBorrowedSelf<T>(_ action: (Self) -> (Self, T)) -> T {
    withUnsafeMutablePointer(to: &self, { this in
      let (s, result) = action(this.move())
      this.initialize(to: s)
      return result
    })
  }

}

struct Contextualizer_: TypeWalker {

  var parent: ValType?

  @available(*, deprecated)
  let compiler: Compiler

  /// The substitution table keeping track of the type variables that were used to open each
  /// specific generic type parameter.
  var substitutions: [GenericParamType: TypeVar] = [:]

  /// A closure that accepts a type parameter and returns whether it should be opened (true) or
  /// skolemized (false).
  let shouldOpen: (GenericParamType) -> Bool

  mutating func willVisit(_ type: ValType) -> TypeWalkerAction {
    guard let type = type as? GenericParamType else {
      return type[.hasTypeParams] ? .stepInto(type) : .stepOver(type)
    }

    if let variable = substitutions[type] {
      // We already opened the parameter.
      return .stepOver(variable)
    } else if shouldOpen(type) {
      // Open the parameter as a fresh variable.
      let variable = TypeVar()
      substitutions[type] = variable
      return .stepOver(variable)
    } else {
      // Skolemize the parameter.
      fatalError()
    }
  }

}

/// The first pass of type checking for value expressions.
struct Desugarer: NodeWalker {

  typealias Result = Bool

  var transformer: Transformer

  init(checker: TypeChecker_) {
    self.transformer = Transformer(checker: checker)
  }

  var parent: Node? {
    get { transformer.parent }
    set { transformer.parent = newValue }
  }

  var innermostSpace: DeclSpace? {
    get { transformer.innermostSpace }
    set { transformer.innermostSpace = newValue }
  }

  struct Transformer: ExprVisitor {

    typealias ExprResult = Expr

    var checker: TypeChecker_!

    var parent: Node?

    var innermostSpace: DeclSpace?

    mutating func visit(_ node: BoolLiteralExpr) -> Expr { node }

    mutating func visit(_ node: IntLiteralExpr) -> Expr { node }

    mutating func visit(_ node: FloatLiteralExpr) -> Expr { node }

    mutating func visit(_ node: StringLiteralExpr) -> Expr { node }

    mutating func visit(_ node: AssignExpr) -> Expr { node }

    mutating func visit(_ node: BaseCastExpr) -> Expr { node }

    mutating func visit(_ node: StaticCastExpr) -> Expr { node }

    mutating func visit(_ node: RuntimeCastExpr) -> Expr { node }

    mutating func visit(_ node: PointerCastExpr) -> Expr { node }

    mutating func visit(_ node: TupleExpr) -> Expr { node }

    mutating func visit(_ node: CallExpr) -> Expr { node }

    mutating func visit(_ node: UnresolvedDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: UnresolvedMemberExpr) -> Expr { node }

    mutating func visit(_ node: UnresolvedQualDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: OverloadedDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: DeclRefExpr) -> Expr { node }

    mutating func visit(_ node: TypeDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: KindRefExpr) -> Expr { node }

    mutating func visit(_ node: MemberDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: TupleMemberExpr) -> Expr { node }

    mutating func visit(_ node: SpecializedDeclRefExpr) -> Expr { node }

    mutating func visit(_ node: LambdaExpr) -> Expr { node }

    mutating func visit(_ node: AsyncExpr) -> Expr { node }

    mutating func visit(_ node: AwaitExpr) -> Expr { node }

    mutating func visit(_ node: AddrOfExpr) -> Expr { node }

    mutating func visit(_ node: MatchExpr) -> Expr { node }

    mutating func visit(_ node: WildcardExpr) -> Expr { node }

    mutating func visit(_ node: ErrorExpr) -> Expr { node }

  }

}
