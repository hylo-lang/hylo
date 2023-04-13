import Utils

extension AST {

  /// Returns `.success` if `decl` is a well-formed top-level or namespace member declaration.
  /// Otherwise, returns `.failure` with the diagnostics of the broken invariants.
  func validateGlobalScopeMember(
    _ decl: AnyDeclID,
    into diagnostics: inout DiagnosticSet,
    atTopLevel: Bool
  ) {
    switch decl.kind {
    case AssociatedTypeDecl.self:
      diagnostics.insert(
        .error(unexpectedAssociatedTypeDecl: self[NodeID(decl)!]))

    case AssociatedValueDecl.self:
      diagnostics.insert(
        .error(unexpectedAssociatedValueDecl: self[NodeID(decl)!]))

    case BindingDecl.self:
      let d = self[BindingDecl.ID(decl)!]
      if let m = d.memberModifier {
        diagnostics.insert(.error(unexpectedMemberModifier: m))
      }
      if self[d.pattern].introducer.value != .let {
        diagnostics.insert(.error(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        diagnostics.insert(.error(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
      let d = self[FunctionDecl.ID(decl)!]
      if let m = d.memberModifier {
        diagnostics.insert(.error(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        diagnostics.insert(.error(missingFunctionIdentifier: d))
      }

    case GenericParameterDecl.self:
      diagnostics.insert(
        .error(unexpectedGenericParameterDecl: self[NodeID(decl)!]))

    case ImportDecl.self:
      if !atTopLevel {
        diagnostics.insert(.error(unexpectedImportDecl: self[NodeID(decl)!]))
      }

    case InitializerDecl.self:
      diagnostics.insert(
        .error(unexpectedInitializerDecl: self[NodeID(decl)!]))

    case MethodDecl.self:
      diagnostics.insert(.error(unexpectedMethodDecl: self[NodeID(decl)!]))

    case MethodImpl.self:
      diagnostics.insert(.error(unexpectedMethodImpl: self[NodeID(decl)!]))

    case NamespaceDecl.self:
      break

    case OperatorDecl.self:
      if !atTopLevel {
        diagnostics.insert(.error(unexpectedOperatorDecl: self[NodeID(decl)!]))
      }

    case ParameterDecl.self:
      diagnostics.insert(.error(unexpectedParameterDecl: self[NodeID(decl)!]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
      let d = self[SubscriptDecl.ID(decl)!]
      if d.introducer.value != .subscript {
        diagnostics.insert(.error(unexpectedPropertyDecl: d))
      }
      if let m = d.memberModifier {
        diagnostics.insert(.error(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        diagnostics.insert(.error(missingSubscriptIdentifier: d))
      }

    case SubscriptImpl.self:
      diagnostics.insert(
        .error(unexpectedSubscriptImpl: self[NodeID(decl)!]))

    case TraitDecl.self:
      break

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
      diagnostics.insert(.error(unexpectedVarDecl: self[NodeID(decl)!]))

    default:
      unexpected(decl, in: self)
    }

  }

  /// Reports any well-formedness problems with `decl` into `diagnostics`.
  func validateTypeMember(_ decl: AnyDeclID, into diagnostics: inout DiagnosticSet) {
    switch decl.kind {
    case AssociatedTypeDecl.self:
      diagnostics.insert(
        .error(unexpectedAssociatedTypeDecl: self[NodeID(decl)!]))

    case AssociatedValueDecl.self:
      diagnostics.insert(
        .error(unexpectedAssociatedValueDecl: self[NodeID(decl)!]))

    case BindingDecl.self:
      let d = self[BindingDecl.ID(decl)!]
      let introducer = self[d.pattern].introducer
      if introducer.value != .let {
        if introducer.value != .var {
          diagnostics.insert(.error(illegalMemberBindingIntroducer: self[d.pattern].introducer))
        }
        if d.memberModifier?.value == .static {
          diagnostics.insert(.error(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
        }
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        diagnostics.insert(.error(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
      let d = self[FunctionDecl.ID(decl)!]
      if d.identifier == nil {
        diagnostics.insert(.error(missingFunctionIdentifier: d))
      }
      if let c = d.explicitCaptures.first {
        diagnostics.insert(.error(unexpectedCapture: self[self[c].pattern]))
      }

    case GenericParameterDecl.self:
      diagnostics.insert(
        .error(unexpectedGenericParameterDecl: self[NodeID(decl)!]))

    case ImportDecl.self:
      diagnostics.insert(.error(unexpectedImportDecl: self[NodeID(decl)!]))

    case InitializerDecl.self:
      break

    case MethodDecl.self:
      break

    case MethodImpl.self:
      diagnostics.insert(.error(unexpectedMethodImpl: self[NodeID(decl)!]))

    case NamespaceDecl.self:
      diagnostics.insert(.error(unexpectedNamespaceDecl: self[NodeID(decl)!]))

    case OperatorDecl.self:
      diagnostics.insert(.error(unexpectedOperatorDecl: self[NodeID(decl)!]))

    case ParameterDecl.self:
      diagnostics.insert(.error(unexpectedParameterDecl: self[NodeID(decl)!]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
      break

    case SubscriptImpl.self:
      diagnostics.insert(
        .error(unexpectedSubscriptImpl: self[NodeID(decl)!]))

    case TraitDecl.self:
      diagnostics.insert(.error(unexpectedTraitDecl: self[NodeID(decl)!]))

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
      diagnostics.insert(.error(unexpectedVarDecl: self[NodeID(decl)!]))

    default:
      unexpected(decl, in: self)
    }
  }

}
