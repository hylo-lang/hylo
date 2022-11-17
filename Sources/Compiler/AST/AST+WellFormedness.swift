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
    case AssociatedTypeDecl.self:
      report.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case AssociatedValueDecl.self:
      report.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case BindingDecl.self:
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

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
      let d = self[NodeID<FunctionDecl>(rawValue: decl.rawValue)]
      if let m = d.memberModifier {
        report.append(.diagnose(unexpectedMemberModifier: m))
      }
      if d.identifier == nil {
        report.append(.diagnose(missingFunctionIdentifier: d))
      }

    case GenericTypeParamDecl.self:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case GenericValueParamDecl.self:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ImportDecl.self:
      if !atTopLevel {
        report.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case InitializerDecl.self:
      report.append(.diagnose(unexpectedInitializerDecl: self[NodeID(rawValue: decl.rawValue)]))

    case MethodDecl.self:
      report.append(.diagnose(unexpectedMethodDecl: self[NodeID(rawValue: decl.rawValue)]))

    case MethodImplDecl.self:
      report.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case NamespaceDecl.self:
      break

    case OperatorDecl.self:
      if !atTopLevel {
        report.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))
      }

    case ParameterDecl.self:
      report.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
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

    case SubscriptImplDecl.self:
      report.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case TraitDecl.self:
      break

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
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
    case AssociatedTypeDecl.self:
      report.append(.diagnose(unexpectedAssociatedTypeDecl: self[NodeID(rawValue: decl.rawValue)]))

    case AssociatedValueDecl.self:
      report.append(.diagnose(unexpectedAssociatedValueDecl: self[NodeID(rawValue: decl.rawValue)]))

    case BindingDecl.self:
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

    case ConformanceDecl.self:
      break

    case ExtensionDecl.self:
      break

    case FunctionDecl.self:
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

    case GenericTypeParamDecl.self:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case GenericValueParamDecl.self:
      report.append(.diagnose(unexpectedGenericTypeParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ImportDecl.self:
      report.append(.diagnose(unexpectedImportDecl: self[NodeID(rawValue: decl.rawValue)]))

    case InitializerDecl.self:
      break

    case MethodDecl.self:
      break

    case MethodImplDecl.self:
      report.append(.diagnose(unexpectedMethodImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case NamespaceDecl.self:
      report.append(.diagnose(unexpectedNamespaceDecl: self[NodeID(rawValue: decl.rawValue)]))

    case OperatorDecl.self:
      report.append(.diagnose(unexpectedOperatorDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ParameterDecl.self:
      report.append(.diagnose(unexpectedParameterDecl: self[NodeID(rawValue: decl.rawValue)]))

    case ProductTypeDecl.self:
      break

    case SubscriptDecl.self:
      break

    case SubscriptImplDecl.self:
      report.append(.diagnose(unexpectedSubscriptImplDecl: self[NodeID(rawValue: decl.rawValue)]))

    case TraitDecl.self:
      report.append(.diagnose(unexpectedTraitDecl: self[NodeID(rawValue: decl.rawValue)]))

    case TypeAliasDecl.self:
      break

    case VarDecl.self:
      report.append(.diagnose(unexpectedVarDecl: self[NodeID(rawValue: decl.rawValue)]))

    default:
      unreachable("unexpected declaration")
    }

    return report.isEmpty ? .success : .failure(report)
  }

}
