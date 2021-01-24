import Foundation

extension Context {

  func loadBuiltinDecls() {
    guard
      let url = Bundle.module.url(forResource: "Builtin", withExtension: "json"),
      let config = try? JSONDecoder().decode(BuiltinConfig.self, from: Data(contentsOf: url))
    else {
      preconditionFailure("I coudn't load builtin definitions")
    }

    for function in config.functions {
      let decl = createBuiltinFunDecl(
        name  : function[0],
        params: function[1 ..< (function.count - 1)],
        ret   : function[function.count - 1])
      decl.parentDeclScope = builtin
      builtin.statements.append(decl)
      builtinDecls[decl.name] = decl
    }
  }

  private func createBuiltinFunDecl(
    name: String,
    params: ArraySlice<String>,
    ret: String
  ) -> FunDecl {
    var paramTypes: [TupleType.Elem] = []
    var paramDecls: [FunParamDecl] = []
    for (i, param) in params.enumerated() {
      let type = getBuiltinType(named: param)!
      paramTypes.append(TupleType.Elem(type: type))
      let decl = FunParamDecl(
        name: "_\(i)",
        typeSign: BuiltinTypeRepr(type: type, range: .invalid),
        type: type,
        range: .invalid)
      paramDecls.append(decl)
    }

    let retTypeSign: TypeRepr? = ret.isEmpty
      ? nil
      : BuiltinTypeRepr(type: getBuiltinType(named: ret)!, range: .invalid)

    let builtinFunType = funType(
      paramType: tupleType(paramTypes),
      retType  : retTypeSign?.type ?? unitType)

    return FunDecl(
      name: name,
      params: paramDecls,
      retTypeSign: retTypeSign,
      type: builtinFunType,
      range: .invalid)
  }

}

fileprivate struct BuiltinConfig: Codable {

  /// The signatures of all built-in functions.
  ///
  /// Signatures are represented as sequence of strings composed of the function's name (in first
  /// position), the type of its parameters and its return type (in last position). If the function
  /// doesn't return any value (e.g., `i64_cpy`), then the last element is an empty string.
  let functions: [[String]]

}
