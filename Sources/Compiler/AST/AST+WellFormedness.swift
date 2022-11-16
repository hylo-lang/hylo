import Utils

extension AST {

  /// Returns `.success` if `decl` is a well-formed top-level or namespace member declaration.
  /// Otherwise, returns `.failure` with the diagnostics of the broken invariants.
  func validateGlobalScopeMember(
    _ decl: AnyDeclID,
    atTopLevel: Bool
  ) -> SuccessOrDiagnostics {
    var report: [Diagnostic] = []

    switch decl.kind {
    case .associatedTypeDecl:
      report.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .associatedValueDecl:
      report.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .bindingDecl:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        report.append(.diagnose(unexpectedMemberModifier: m))
      }
      if self[d.pattern].introducer.value != .let {
        report.append(.diagnose(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        report.append(.diagnose(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case .conformanceDecl:
      break

    case .extensionDecl:
      break

    case .functionDecl:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        report.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        report.append(.diagnose(missingFunctionIdentifier: d))
      }

    case .genericTypeParamDecl:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .genericValueParamDecl:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .importDecl:
      if !atTopLevel {
        report.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case .initializerDecl:
      report.append(.diagnose(unexpectedInitializerDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .methodDecl:
      report.append(.diagnose(unexpectedMethodDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .methodImplDecl:
      report.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .namespaceDecl:
      break

    case .operatorDecl:
      if !atTopLevel {
        report.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case .parameterDecl:
      report.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .productTypeDecl:
      break

    case .subscriptDecl:
      let d = self[NodeID<SubscriptDecl>(rawValue: decl.rawValue)]
      if d.introducer.value != .subscript {
        report.append(.diagnose(unexpectedPropertyDecl: d))
      }
      if let m = d.memberModifier {
        report.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        report.append(.diagnose(missingSubscriptIdentifier: d))
      }

    case .subscriptImplDecl:
      report.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .traitDecl:
      break

    case .typeAliasDecl:
      break

    case .varDecl:
      report.append(.diagnose(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

    return report.isEmpty ? .success : .failure(report)
  }

  /// Returns `.success` if `decl` is a well-formed type member declaration. Otherwise, returns
  /// `.failure` with the diagnostics of the broken invariants.
  func validateTypeMember(_ decl: AnyDeclID) -> SuccessOrDiagnostics {
    var report: [Diagnostic] = []

    switch decl.kind {
    case .associatedTypeDecl:
      report.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .associatedValueDecl:
      report.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .bindingDecl:
      let d = self[NodeID<BindingDecl>(rawValue: decl.rawValue)]
      let introducer = self[d.pattern].introducer
      if introducer.value != .let {
        if introducer.value != .var {
          report.append(.diagnose(illegalMemberBindingIntroducer: self[d.pattern].introducer))
        }
        if d.memberModifier?.value == .static {
          report.append(.diagnose(illegalGlobalBindingIntroducer: self[d.pattern].introducer))
        }
      }
      if (d.initializer == nil) && (self[d.pattern].annotation == nil) {
        report.append(.diagnose(missingTypeAnnotation: self[d.pattern], in: self))
      }

    case .conformanceDecl:
      break

    case .extensionDecl:
      break

    case .functionDecl:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        report.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        report.append(.diagnose(missingFunctionIdentifier: d))
      }
      if let c = d.explicitCaptures.first {
        report.append(.diagnose(unexpectedCapture: self[self[c].pattern]))
      }

    case .genericTypeParamDecl:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .genericValueParamDecl:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .importDecl:
      report.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .initializerDecl:
      break

    case .methodDecl:
      break

    case .methodImplDecl:
      report.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .namespaceDecl:
      report.append(.diagnose(unexpectedNamespaceDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .operatorDecl:
      report.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .parameterDecl:
      report.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .productTypeDecl:
      break

    case .subscriptDecl:
      break

    case .subscriptImplDecl:
      report.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .traitDecl:
      report.append(.diagnose(unexpectedTraitDecl: self[NodeID(rawValue: decl.rawValue)]))

    case .typeAliasDecl:
      break

    case .varDecl:
      report.append(.diagnose(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

    return report.isEmpty ? .success : .failure(report)
  }

}
