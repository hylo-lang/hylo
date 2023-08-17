import Utils

extension AST {

  /// Reports any well-formedness problems with `d` occurring at global scope into `log`.
  ///
  /// `atTopLevel` is `true` iff `d` isn't nested in any declaration.
  func validateGlobalScopeMember(
    _ d: AnyDeclID, atTopLevel: Bool, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    switch d.kind {
    case AssociatedTypeDecl.self:
      log.insert(.error(unexpectedAssociatedTypeDecl: self[NodeID(d)!]))
    case AssociatedValueDecl.self:
      log.insert(.error(unexpectedAssociatedValueDecl: self[NodeID(d)!]))
    case BindingDecl.self:
      validateGlobalScopeMember(BindingDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case ConformanceDecl.self:
      break
    case ExtensionDecl.self:
      break
    case FunctionDecl.self:
      validateGlobalScopeMember(FunctionDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case GenericParameterDecl.self:
      log.insert(.error(unexpectedGenericParameterDecl: self[NodeID(d)!]))
    case ImportDecl.self:
      if !atTopLevel { log.insert(.error(unexpectedImportDecl: self[NodeID(d)!])) }
    case InitializerDecl.self:
      log.insert(.error(unexpectedInitializerDecl: self[NodeID(d)!]))
    case MethodDecl.self:
      log.insert(.error(unexpectedMethodDecl: self[NodeID(d)!]))
    case MethodImpl.self:
      log.insert(.error(unexpectedMethodImpl: self[NodeID(d)!]))
    case NamespaceDecl.self:
      break
    case OperatorDecl.self:
      if !atTopLevel { log.insert(.error(unexpectedOperatorDecl: self[NodeID(d)!])) }
    case ParameterDecl.self:
      log.insert(.error(unexpectedParameterDecl: self[NodeID(d)!]))
    case ProductTypeDecl.self:
      break
    case SubscriptDecl.self:
      validateGlobalScopeMember(SubscriptDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case SubscriptImpl.self:
      log.insert(.error(unexpectedSubscriptImpl: self[NodeID(d)!]))
    case TraitDecl.self:
      break
    case TypeAliasDecl.self:
      break
    case VarDecl.self:
      log.insert(.error(unexpectedVarDecl: self[NodeID(d)!]))
    default:
      unexpected(d, in: self)
    }
  }

  /// Returns `.success` if `d` is a well-formed declaration ar global scope. Otherwise, returns
  /// `.failure`, reporting the diagnostics of the broken invariants in `log`.
  func validateGlobalScopeMember(
    _ d: BindingDecl.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if let m = self[d].memberModifier {
      log.insert(.error(unexpectedMemberModifier: m))
    }

    let p = self[d].pattern
    if self[p].introducer.value != .let {
      log.insert(.error(illegalGlobalBindingIntroducer: self[p].introducer))
    }
    if (self[d].initializer == nil) && (self[p].annotation == nil) {
      log.insert(.error(missingTypeAnnotation: self[p], in: self))
    }
  }

  /// Returns `.success` if `d` is a well-formed declaration ar global scope. Otherwise, returns
  /// `.failure`, reporting the diagnostics of the broken invariants in `log`.
  func validateGlobalScopeMember(
    _ d: FunctionDecl.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if let m = self[d].memberModifier {
      log.insert(.error(unexpectedMemberModifier: m))
    }
    if self[d].identifier == nil {
      log.insert(.error(missingFunctionIdentifier: self[d]))
    }
  }

  /// Returns `.success` if `d` is a well-formed declaration ar global scope. Otherwise, returns
  /// `.failure`, reporting the diagnostics of the broken invariants in `log`.
  func validateGlobalScopeMember(
    _ d: SubscriptDecl.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if self[d].isProperty {
      log.insert(.error(unexpectedPropertyDecl: self[d]))
    }
    if let m = self[d].memberModifier {
      log.insert(.error(unexpectedMemberModifier: m))
    }
    if self[d].identifier == nil {
      log.insert(.error(missingSubscriptIdentifier: self[d]))
    }
  }

  /// Reports any well-formedness problems with `d` occurring at type scope into `log`.
  func validateTypeMember(_ d: AnyDeclID, into log: inout DiagnosticSet) {
    switch d.kind {
    case AssociatedTypeDecl.self:
      log.insert(.error(unexpectedAssociatedTypeDecl: self[NodeID(d)!]))
    case AssociatedValueDecl.self:
      log.insert(.error(unexpectedAssociatedValueDecl: self[NodeID(d)!]))
    case BindingDecl.self:
      validateTypeMember(BindingDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case ConformanceDecl.self:
      break
    case ExtensionDecl.self:
      break
    case FunctionDecl.self:
      validateTypeMember(FunctionDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case GenericParameterDecl.self:
      log.insert(.error(unexpectedGenericParameterDecl: self[NodeID(d)!]))
    case ImportDecl.self:
      log.insert(.error(unexpectedImportDecl: self[NodeID(d)!]))
    case InitializerDecl.self:
      break
    case MethodDecl.self:
      break
    case MethodImpl.self:
      log.insert(.error(unexpectedMethodImpl: self[NodeID(d)!]))
    case NamespaceDecl.self:
      log.insert(.error(unexpectedNamespaceDecl: self[NodeID(d)!]))
    case OperatorDecl.self:
      log.insert(.error(unexpectedOperatorDecl: self[NodeID(d)!]))
    case ParameterDecl.self:
      log.insert(.error(unexpectedParameterDecl: self[NodeID(d)!]))
    case ProductTypeDecl.self:
      break
    case SubscriptDecl.self:
      break
    case SubscriptImpl.self:
      log.insert(.error(unexpectedSubscriptImpl: self[NodeID(d)!]))
    case TraitDecl.self:
      log.insert(.error(unexpectedTraitDecl: self[NodeID(d)!]))
    case TypeAliasDecl.self:
      break
    case VarDecl.self:
      log.insert(.error(unexpectedVarDecl: self[NodeID(d)!]))
    default:
      unexpected(d, in: self)
    }
  }

  /// Reports any well-formedness problems with `d` occurring at type scope into `log`.
  func validateTypeMember(_ d: BindingDecl.ID, reportingDiagnosticsTo log: inout DiagnosticSet) {
    let p = self[d].pattern
    let introducer = self[p].introducer
    if introducer.value != .let {
      if introducer.value != .var {
        log.insert(.error(illegalMemberBindingIntroducer: self[p].introducer))
      }
      if self[d].memberModifier?.value == .static {
        log.insert(.error(illegalGlobalBindingIntroducer: self[p].introducer))
      }
    }
    if (self[d].initializer == nil) && (self[p].annotation == nil) {
      log.insert(.error(missingTypeAnnotation: self[p], in: self))
    }
  }

  /// Reports any well-formedness problems with `d` occurring at type scope into `log`.
  func validateTypeMember(_ d: FunctionDecl.ID, reportingDiagnosticsTo log: inout DiagnosticSet) {
    if self[d].identifier == nil {
      log.insert(.error(missingFunctionIdentifier: self[d]))
    }
    if let c = self[d].explicitCaptures.first {
      log.insert(.error(unexpectedCapture: self[self[c].pattern]))
    }
  }

}
