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
      log.insert(.error(unexpected: AssociatedTypeDecl.ID(d)!, in: self))
    case AssociatedValueDecl.self:
      log.insert(.error(unexpected: AssociatedValueDecl.ID(d)!, in: self))
    case BindingDecl.self:
      validateGlobalScopeMember(BindingDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case ConformanceDecl.self:
      break
    case ExtensionDecl.self:
      break
    case FunctionDecl.self:
      validateGlobalScopeMember(FunctionDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case GenericParameterDecl.self:
      log.insert(.error(unexpected: GenericParameterDecl.ID(d)!, in: self))
    case ImportDecl.self:
      if !atTopLevel { log.insert(.error(unexpected: ImportDecl.ID(d)!, in: self)) }
    case InitializerDecl.self:
      log.insert(.error(unexpected: InitializerDecl.ID(d)!, in: self))
    case MethodDecl.self:
      log.insert(.error(unexpected: MethodDecl.ID(d)!, in: self))
    case MethodImpl.self:
      log.insert(.error(unexpected: MethodImpl.ID(d)!, in: self))
    case NamespaceDecl.self:
      break
    case OperatorDecl.self:
      if !atTopLevel { log.insert(.error(unexpected: OperatorDecl.ID(d)!, in: self)) }
    case ParameterDecl.self:
      log.insert(.error(unexpected: ParameterDecl.ID(d)!, in: self))
    case ProductTypeDecl.self:
      break
    case SubscriptDecl.self:
      validateGlobalScopeMember(SubscriptDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case SubscriptImpl.self:
      log.insert(.error(unexpected: SubscriptImpl.ID(d)!, in: self))
    case TraitDecl.self:
      break
    case TypeAliasDecl.self:
      break
    case VarDecl.self:
      log.insert(.error(unexpected: VarDecl.ID(d)!, in: self))
    default:
      unexpected(d, in: self)
    }
  }

  /// Reports any well-formedness problems with `d` occurring at global scope into `log`.
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

  /// Reports any well-formedness problems with `d` occurring at global scope into `log`.
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

  /// Reports any well-formedness problems with `d` occurring at global scope into `log`.
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
      log.insert(.error(unexpected: AssociatedTypeDecl.ID(d)!, in: self))
    case AssociatedValueDecl.self:
      log.insert(.error(unexpected: AssociatedValueDecl.ID(d)!, in: self))
    case BindingDecl.self:
      validateTypeMember(BindingDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case ConformanceDecl.self:
      break
    case ExtensionDecl.self:
      break
    case FunctionDecl.self:
      validateTypeMember(FunctionDecl.ID(d)!, reportingDiagnosticsTo: &log)
    case GenericParameterDecl.self:
      log.insert(.error(unexpected: GenericParameterDecl.ID(d)!, in: self))
    case ImportDecl.self:
      log.insert(.error(unexpected: ImportDecl.ID(d)!, in: self))
    case InitializerDecl.self:
      break
    case MethodDecl.self:
      break
    case MethodImpl.self:
      log.insert(.error(unexpected: MethodImpl.ID(d)!, in: self))
    case NamespaceDecl.self:
      log.insert(.error(unexpected: NamespaceDecl.ID(d)!, in: self))
    case OperatorDecl.self:
      log.insert(.error(unexpected: OperatorDecl.ID(d)!, in: self))
    case ParameterDecl.self:
      log.insert(.error(unexpected: ParameterDecl.ID(d)!, in: self))
    case ProductTypeDecl.self:
      break
    case SubscriptDecl.self:
      break
    case SubscriptImpl.self:
      log.insert(.error(unexpected: SubscriptImpl.ID(d)!, in: self))
    case TraitDecl.self:
      log.insert(.error(unexpected: TraitDecl.ID(d)!, in: self))
    case TypeAliasDecl.self:
      break
    case VarDecl.self:
      log.insert(.error(unexpected: VarDecl.ID(d)!, in: self))
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
