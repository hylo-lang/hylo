import Utils

extension AST {

  /// Returns `.success` if `decl` is a well-formed top-level or namespace member declaration.
  /// Otherwise, returns `.failure` with the diagnostics of the broken invariants.
  func isValidGlobalScopeMember(
    _ decl: AnyDeclID,
    atTopLevel: Bool
  ) -> SuccessOrDiagnostics {
    var ds: [Diagnostic] = []

    switch decl.kind {
    case .associatedTypeDecl:
      ds.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .associatedValueDecl:
      ds.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .bindingDecl:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        ds.append(.diagnose(unexpectedMemberModifier: m))
      }
      if self[d.pattern].introducer.value != .let {
        ds.append(.diagnose(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        ds.append(.diagnose(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case .conformanceDecl:
      break

    case .extensionDecl:
      break

    case .functionDecl:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        ds.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        ds.append(.diagnose(missingFunctionIdentifier: d))
      }

    case .genericTypeParamDecl:
      ds.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .genericValueParamDecl:
      ds.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .importDecl:
      if !atTopLevel {
        ds.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case .initializerDecl:
      ds.append(.diagnose(unexpectedInitializerDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .methodDecl:
      ds.append(.diagnose(unexpectedMethodDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .methodImplDecl:
      ds.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .namespaceDecl:
      break

    case .operatorDecl:
      if !atTopLevel {
        ds.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case .parameterDecl:
      ds.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .productTypeDecl:
      break

    case .subscriptDecl:
      let d = self[NodeID<SubscriptDecl>(rawValue: decl.rawValue)]
      if d.introducer.value != .subscript {
        ds.append(.diagnose(unexpectedPropertyDecl: d))
      }
      if let m = d.memberModifier {
        ds.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        ds.append(.diagnose(missingSubscriptIdentifier: d))
      }

    case .subscriptImplDecl:
      ds.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .traitDecl:
      break

    case .typeAliasDecl:
      break

    case .varDecl:
      ds.append(.diagnose(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

    return ds.isEmpty ? .success : .failure(ds)
  }

  /// Returns `.success` if `decl` is a well-formed type member declaration. Otherwise, returns
  /// `.failure` with the diagnostics of the broken invariants.
  func isValidTypeMember(_ decl: AnyDeclID) -> SuccessOrDiagnostics {
    var ds: [Diagnostic] = []

    switch decl.kind {
    case .associatedTypeDecl:
      ds.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .associatedValueDecl:
      ds.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .bindingDecl:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      let introducer = self[d.pattern].introducer
      if introducer.value != .let {
        if introducer.value != .var {
          ds.append(.diagnose(illegalMemberBindingIntroducer: self[d.pattern].introducer))
        }
        if d.memberModifier?.value == .static {
          ds.append(.diagnose(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
        }
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        ds.append(.diagnose(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case .conformanceDecl:
      break

    case .extensionDecl:
      break

    case .functionDecl:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        ds.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        ds.append(.diagnose(missingFunctionIdentifier: d))
      }
      if let c = d.explicitCaptures.first {
        ds.append(.diagnose(unexpectedCapture: self[self[c].pattern]))
      }

    case .genericTypeParamDecl:
      ds.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .genericValueParamDecl:
      ds.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .importDecl:
      ds.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .initializerDecl:
      break

    case .methodDecl:
      break

    case .methodImplDecl:
      ds.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .namespaceDecl:
      ds.append(.diagnose(unexpectedNamespaceDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .operatorDecl:
      ds.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .parameterDecl:
      ds.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .productTypeDecl:
      break

    case .subscriptDecl:
      break

    case .subscriptImplDecl:
      ds.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .traitDecl:
      ds.append(.diagnose(unexpectedTraitDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .typeAliasDecl:
      break

    case .varDecl:
      ds.append(.diagnose(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

    return ds.isEmpty ? .success : .failure(ds)
  }

}
