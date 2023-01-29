import Utils

extension AST {

  /// Returns `.success` if `decl` is a well-formed top-level or namespace member declaration.
  /// Otherwise, returns `.failure` with the diagnostics of the broken invariants.
  func validateGlobalScopeMember(
    _ decl: AnyDeclID,
    into diagnostics: inout Diagnostics,
    atTopLevel: Bool
  ) {
    switch decl.kind {
    case AssociatedTypeDecl.self:
      diagnostics.report(
        .error(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case AssociatedValueDecl.self:
      diagnostics.report(
        .error(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case BindingDecl.self:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        diagnostics.report(.error(unexpectedMemberModifier: m))
      }
      if self[d.pattern].introducer.value != .let {
        diagnostics.report(.error(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        diagnostics.report(.error(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        diagnostics.report(.error(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        diagnostics.report(.error(missingFunctionIdentifier: d))
      }

    case GenericParameterDecl.self:
      diagnostics.report(
        .error(unexpectedGenericParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ImportDecl.self:
      if !atTopLevel {
        diagnostics.report(.error(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case InitializerDecl.self:
      diagnostics.report(
        .error(unexpectedInitializerDecl: self[NodeID(rawValue: decl.rawValue)]))

    case MethodDecl.self:
      diagnostics.report(.error(unexpectedMethodDecl: self[NodeID(rawValue: decl.rawValue)]))

    case MethodImpl.self:
      diagnostics.report(.error(unexpectedMethodImpl: self[NodeID(rawValue: decl.rawValue)]))

    case NamespaceDecl.self:
      break

    case OperatorDecl.self:
      if !atTopLevel {
        diagnostics.report(.error(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case ParameterDecl.self:
      diagnostics.report(.error(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
      let d = self[NodeID<SubscriptDecl>(rawValue: decl.rawValue)]
      if d.introducer.value != .subscript {
        diagnostics.report(.error(unexpectedPropertyDecl: d))
      }
      if let m = d.memberModifier {
        diagnostics.report(.error(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        diagnostics.report(.error(missingSubscriptIdentifier: d))
      }

    case SubscriptImpl.self:
      diagnostics.report(
        .error(unexpectedSubscriptImpl: self[NodeID(rawValue: decl.rawValue)]))

    case TraitDecl.self:
      break

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
      diagnostics.report(.error(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

  }

  /// Reports any well-formedness problems with `decl` into `diagnostics`.
  func validateTypeMember(_ decl: AnyDeclID, into diagnostics: inout Diagnostics) {
    switch decl.kind {
    case AssociatedTypeDecl.self:
      diagnostics.report(
        .error(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case AssociatedValueDecl.self:
      diagnostics.report(
        .error(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case BindingDecl.self:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      let introducer = self[d.pattern].introducer
      if introducer.value != .let {
        if introducer.value != .var {
          diagnostics.report(.error(illegalMemberBindingIntroducer: self[d.pattern].introducer))
        }
        if d.memberModifier?.value == .static {
          diagnostics.report(.error(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
        }
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        diagnostics.report(.error(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        diagnostics.report(.error(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        diagnostics.report(.error(missingFunctionIdentifier: d))
      }
      if let c = d.explicitCaptures.first {
        diagnostics.report(.error(unexpectedCapture: self[self[c].pattern]))
      }

    case GenericParameterDecl.self:
      diagnostics.report(
        .error(unexpectedGenericParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ImportDecl.self:
      diagnostics.report(.error(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))

    case InitializerDecl.self:
      break

    case MethodDecl.self:
      break

    case MethodImpl.self:
      diagnostics.report(.error(unexpectedMethodImpl: self[NodeID(rawValue: decl.rawValue)]))

    case NamespaceDecl.self:
      diagnostics.report(.error(unexpectedNamespaceDecl: self[NodeID(rawValue: decl.rawValue)]))

    case OperatorDecl.self:
      diagnostics.report(.error(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ParameterDecl.self:
      diagnostics.report(.error(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
      break

    case SubscriptImpl.self:
      diagnostics.report(
        .error(unexpectedSubscriptImpl: self[NodeID(rawValue: decl.rawValue)]))

    case TraitDecl.self:
      diagnostics.report(.error(unexpectedTraitDecl: self[NodeID(rawValue: decl.rawValue)]))

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
      diagnostics.report(.error(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }
  }

}
