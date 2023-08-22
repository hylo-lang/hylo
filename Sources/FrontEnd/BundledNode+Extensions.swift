import Core

extension BundledNode where T: DeclID, P == TypedProgram {

  /// The type of the declared entity.
  public var type: AnyType {
    container.declType[id]!
  }

}

extension BundledNode where T: ConcreteNodeID, T.Subject: CapturingDecl, P == TypedProgram {

  /// The implicit captures for the declared entity.
  public var implicitCaptures: [ImplicitCapture] {
    container.implicitCaptures[AnyDeclID(id)!, default: []]
  }

}

extension BundledNode where T: ExprID, P == TypedProgram {

  /// The type of this expression
  public var type: AnyType {
    container.exprType[id]!
  }

}

extension BundledNode where T == NameExpr.ID, P == TypedProgram {

  /// The declaration referenced by this expression.
  public var referredDecl: DeclReference {
    container.referredDecl[id]!
  }

}

extension BundledNode where T == SequenceExpr.ID, P == TypedProgram {

  /// A representation of `self` that encodes its evaluation order.
  public var folded: FoldedSequenceExpr {
    container.foldedForm[id]!
  }

}
