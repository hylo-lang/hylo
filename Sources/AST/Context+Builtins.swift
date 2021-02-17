import Foundation

extension Context {

  func loadBuiltinDecls() {
    guard
      let url = Bundle.module.url(forResource: "Builtins", withExtension: "json"),
      let config = try? JSONDecoder().decode(BuiltinConfig.self, from: Data(contentsOf: url))
    else {
      preconditionFailure("I coudn't load builtin definitions")
    }

    for function in config.functions {
      let decl = createBuiltinFunDecl(
        name  : function[0],
        params: function[1 ..< (function.count - 1)],
        ret   : function[function.count - 1])
      decl.parentDeclSpace = builtin
      builtin.decls.append(decl)
      builtinDecls[decl.name] = decl
    }
  }

  private func createBuiltinFunDecl(
    name: String,
    params: ArraySlice<String>,
    ret: String
  ) -> FunDecl {
    // Create the declaration of the function.
    let funDecl = FunDecl(name: name, type: unresolvedType, range: .invalid)

    // Create the declaration(s) of the function's parameter.
    var paramTypes: [TupleType.Elem] = []
    for (i, param) in params.enumerated() {
      // Create the parameter's type.
      let type = parse(typeNamed: param)
      paramTypes.append(TupleType.Elem(type: type))

      // Create the declaration of the parameter.
      let decl = FunParamDecl(
        name: "_\(i)",
        typeSign: UnqualTypeRepr(name: param, type: type, range: .invalid),
        type: type,
        range: .invalid)
      decl.parentDeclSpace = funDecl
      decl.setState(.typeChecked)

      funDecl.params.append(decl)
    }

    // Setup the function's signature.
    funDecl.retSign = ret.isEmpty
      ? nil
      : UnqualTypeRepr(name: ret, type: parse(typeNamed: ret), range: .invalid)
    funDecl.type = funType(
      paramType: tupleType(paramTypes),
      retType  : funDecl.retSign?.type ?? unitType)

    funDecl.setState(.typeChecked)
    funDecl.props.insert(.isBuiltin)
    return funDecl
  }

  func parse(typeNamed name: String) -> ValType {
    return name == "Unit"
      ? unitType
      : getBuiltinType(named: name)!
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
