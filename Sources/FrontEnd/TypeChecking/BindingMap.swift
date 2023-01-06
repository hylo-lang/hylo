import Core

/// A map from name expression to its referred declaration.
public typealias BindingMap = [NodeID<NameExpr>: DeclRef]
