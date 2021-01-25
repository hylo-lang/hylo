import AST
import Basic

/// The name binder pass.
///
/// The name binder is the earliest pass of the semantic analysis. It establishes the link between
/// identifiers and the declaration(s) to which they refer.
///
/// The pass has side effects on the AST. In particular, it will modify or substitute unresolved
/// declaration references and update lookup tables.
public final class NameBinder: NodeWalker, AST.Pass {

  public static let name = "Name binder"

  public init(context: AST.Context) {
    self.context = context
  }

  /// The context in which the pass runs.
  public unowned let context: AST.Context

  /// The inner-most declaration space in which the next expression will be visited.
  private var currentSpace: DeclSpace?

  private var visited: Set<ObjectIdentifier> = []

  public func run(on module: Module) {
    currentSpace = module
    _ = visit(module)
  }

  public override func willVisit(_ decl: Decl) -> (shouldWalk: Bool, nodeBefore: Decl) {
    // Update the current decl scope if we're about to enter a new one.
    if let ds = decl as? DeclSpace {
      ds.parentDeclSpace = currentSpace
      currentSpace = ds
    }

    // Bind extensions to the type they extend.
    if let typeExtDecl = decl as? TypeExtDecl {
      // Resolve the identifier of the extended type.
      let (_, typeRepr) = willVisit(typeExtDecl.extendedIdent)

      // The declaration is ill-formed if the identifier does not resolve to a nominal type. In
      // this case, the body of the extension is skipped and should not be visited further by any
      // pass of the semantic analysis.
      guard let ident = typeRepr as? IdentTypeRepr else {
        context.report(.cannotExtendNonNominalType(typeRepr.type, range: typeExtDecl.range))
        typeExtDecl.state = .unbindable
        return (false, decl)
      }

      visited.insert(ObjectIdentifier(ident))
      typeExtDecl.extendedIdent = ident

      switch ident.type {
      case let type as NominalType:
        typeExtDecl.state = .bound(type.decl)
        return (true, typeExtDecl)
      case is UnresolvedType:
        typeExtDecl.state = .unbindable
        return (false, typeExtDecl)
      case let type:
        context.report(.cannotExtendNonNominalType(type, range: ident.range))
        typeExtDecl.state = .unbindable
        return (false, typeExtDecl)
      }
    }

    return (true, decl)
  }

  public override func didVisit(_ decl: Decl) -> (shouldContinue: Bool, nodeAfter: Decl) {
    // Restore the current decl space if we're about to exit one.
    if let ds = decl as? DeclSpace {
      currentSpace = ds.parentDeclSpace
    }
    return (true, decl)
  }

  public override func willVisit(_ stmt: Stmt) -> (shouldWalk: Bool, nodeBefore: Stmt) {
    // Update the current decl space if we're about to enter a new one.
    if let ds = stmt as? DeclSpace {
      ds.parentDeclSpace = currentSpace
      currentSpace = ds
    }
    return (true, stmt)
  }

  public override func didVisit(_ stmt: Stmt) -> (shouldContinue: Bool, nodeAfter: Stmt) {
    // Restore the current decl space if we're about to exit one.
    if let ds = stmt as? DeclSpace {
      currentSpace = ds.parentDeclSpace
    }
    return (true, stmt)
  }

  public override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeBefore: Expr) {
    switch expr {
    case let ref as UnresolvedDeclRefExpr:
      let matches = resolveUnqualTypeOrValueDecl(named: ref.name, at: ref.range)
      return (true, substitute(declRef: ref, using: matches))

    case let ref as QualDeclRefExpr:
      // Handle built-ins.
      if ref.namespace.type === context.builtin.instanceType {
        if let decl = context.getBuiltinDecl(for: ref.name) {
          return (true, DeclRefExpr(decl: decl, range: ref.range))
        } else {
          // FIXME: Handle type expressions.
          context.report(.cannotFind(bultinName: ref.name, range: ref.range))
          return (true, expr)
        }
      }

      // Run a qualified lookup if the namespace resolved to a nominal type.
      if let baseTypeDecl = (ref.namespace.type as? NominalType)?.decl {
        let matches = resolveTypeOrValueDecl(
          named: ref.name, qualifiedIn: baseTypeDecl, at: ref.range)
        return (true, substitute(declRef: ref, using: matches))
      }

      // Ignore the node if its namespace hasn't been resolved, otherwise complain that non-nominal
      // types can't have properties.
      if ref.namespace.type != context.unresolvedType {
        context.report(.cannotFind(name: ref.name, in: ref.namespace.type, range: ref.range))
      }
      return (true, ref)

    case let callExpr as CallExpr where callExpr.fun is TypeDeclRefExpr:
      let funExpr = callExpr.fun as! TypeDeclRefExpr
      guard let typeDecl = funExpr.decl as? AbstractNominalTypeDecl else {
        context.report(.cannotCallNonFunctionType(type: funExpr.decl.type, range: funExpr.range))
        return (true, expr)
      }

      // Substitute the callee for an overload set constructor references.
      let valueDecls = typeDecl.lookup(qualified: "new").values
      precondition(!valueDecls.isEmpty, "nominal type without any constructor")
      let newFunExpr = substitute(declRef: funExpr, using: valueDecls)
      let newCallExpr = CallExpr(
        callee: newFunExpr, args: callExpr.args, type: callExpr.type, range: callExpr.range)
      return (true, newCallExpr)

    default:
      return (true, expr)
    }
  }

  public override func willVisit(
    _ typeRepr: TypeRepr
  ) -> (shouldWalk: Bool, nodeBefore: TypeRepr) {
    // Make sure we didn't already visited this node. This will happen when the type representation
    // is used to identify the extended type of a declaration.
    guard !visited.contains(ObjectIdentifier(typeRepr)) else { return (false, typeRepr) }

    // Note that type representations are resolved in `willVisit` instead of `didVisit`. This is
    // because we need to resolve each element of a `CompoundTypeRepr` to resolve the next one.
    switch typeRepr {
    case let ref as UnqualTypeRepr:
      let matches = resolveUnqualTypeDecl(named: ref.name, at: ref.range)

      // FIXME: Handle overloaded type decls.
      if let typeDecl = matches.first {
        ref.type = typeDecl.instanceType
      }
      return (false, ref)

    case let ref as CompoundTypeRepr:
      let base = ref.components[0]
      var matches = resolveUnqualTypeDecl(named: base.name, at: base.range)

      // FIXME: Handle overloaded type decls.
      guard var typeDecl = matches.first else { return (false, ref) }
      base.type = typeDecl.instanceType

      // Handle built-ins.
      if typeDecl === context.builtin {
        guard let builtinType = context.getBuiltinType(named: ref.components[1].name) else {
          context.report(
            .cannotFind(bultinName: ref.components[1].name, range: ref.components[1].range))
          return (false, ref)
        }
        if ref.components.count > 2 {
          context.report(.builtinTypesAreNotNamespaces(range: ref.components[1].range))
        }
        return (false, BuiltinTypeRepr(type: builtinType, range: ref.range))
      }

      // Resolve each sub-sequent component with qualified lookups.
      for comp in ref.components[1...] {
        matches = resolveTypeDecl(named: comp.name, qualifiedIn: typeDecl, at: comp.range)
        guard let nextTypeDecl = matches.first else { break }
        comp.type = nextTypeDecl.instanceType
        typeDecl = nextTypeDecl
      }

      // Children have been visited explicity, there's no need to walk them.
      return (false, ref)

    default:
      return (true, typeRepr)
    }
  }

  private func resolveUnqualTypeOrValueDecl(
    named name: String, at range: SourceRange
  ) -> LookupResult {
    let matches = currentSpace!.lookup(unqualified: name, in: context)
    if matches.isEmpty {
      context.report(.cannotFind(name: name, range: range))
    }
    return matches
  }

  private func resolveUnqualTypeDecl(
    named name: String, at range: SourceRange
  ) -> [TypeDecl] {
    let typeDecls = currentSpace!.lookup(unqualified: name, in: context).types
    if typeDecls.isEmpty {
      context.report(.cannotFind(typeName: name, range: range))
    }
    return typeDecls
  }

  private func resolveTypeOrValueDecl(
    named name: String, qualifiedIn typeDecl: TypeDecl, at range: SourceRange
  ) -> LookupResult {
    let matches = typeDecl.lookup(qualified: name)
    if matches.isEmpty {
      context.report(.cannotFind(name: name, in: typeDecl.instanceType, range: range))
    }
    return matches
  }

  private func resolveTypeDecl(
    named name: String, qualifiedIn typeDecl: TypeDecl, at range: SourceRange
  ) -> [TypeDecl] {
    let typeDecls = typeDecl.lookup(qualified: name).types
    if typeDecls.isEmpty {
      context.report(.cannotFind(typeName: name, in: typeDecl.instanceType, range: range))
    }
    return typeDecls
  }

  private func resolveValueDecl(
    named name: String, qualifiedIn typeDecl: TypeDecl, at range: SourceRange
  ) -> [ValueDecl] {
    let valueDecls = typeDecl.lookup(qualified: name).values
    if valueDecls.isEmpty {
      context.report(.cannotFind(name: name, in: typeDecl.instanceType, range: range))
    }
    return valueDecls
  }

  private func substitute(declRef expr: Expr, using matches: LookupResult) -> Expr {
    let valueDecls = matches.values
    if !valueDecls.isEmpty {
      return substitute(declRef: expr, using: valueDecls)
    }

    // FIXME: Handle overloaded type decls.
    guard let typeDecl = matches.types.first else { return expr }
    return TypeDeclRefExpr(decl: typeDecl, range: expr.range)
  }

  private func substitute(declRef expr: Expr, using valueDecls: [ValueDecl]) -> Expr {
    if valueDecls.count == 1 {
      // Resolve the reference immediately.
      return DeclRefExpr(decl: valueDecls[0], range: expr.range)
    }

    if valueDecls.count > 1 {
      // Substitute the reference with an overload set.
      let newExpr = OverloadedDeclRefExpr(
        declSet: valueDecls, type: context.unresolvedType, range: expr.range)
      newExpr.type = TypeVar(context: context, node: newExpr)
      return newExpr
    }

    return expr
  }

}
