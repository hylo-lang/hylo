import Foundation

/// The synthesized declaration of the built-in module.
extension ModuleDecl {

  private struct Config: Codable {

    /// The signatures of all built-in functions.
    ///
    /// Signatures are represented as sequence of strings composed of the function's name (in first
    /// position), the type of its parameters and its return type (in last position). If the function
    /// doesn't return any value (e.g., `i64_cpy`), then the last element is an empty string.
    let functions: [[String]]

  }

  /// Synthesizes all built-in declarations.
  private func synthesizeBuiltinDecls() {
    guard let url = Bundle.module.url(forResource: "Builtins", withExtension: "json"),
          let config = try? JSONDecoder().decode(Config.self, from: Data(contentsOf: url))
    else { preconditionFailure("unable load builtin definitions") }

    let unit = FileUnit(source: nil)
    units.append(unit)

    // Synthesize functions.
    for spec in config.functions {
      let decl = createBuiltinFunDecl(
        ident: spec[0], params: spec[1 ..< (spec.count - 1)], ret: spec[spec.count - 1])
      decl.parentDeclSpace = unit
      unit.decls.append(decl)
    }

    // Synthesize type aliases.
    var decl = AliasTypeDecl(
      ident: "Any", aliasedSign: ViewCompSign(views: [], type: .any), type: .unresolved)
    decl.state = .typeChecked
    decl.type = AliasType(decl: decl).kind
    decl.parentDeclSpace = unit
    unit.decls.append(decl)

    decl = AliasTypeDecl(
      ident: "Unit", aliasedSign: TupleSign(elems: [], type: .unit), type: .unresolved)
    decl.state = .typeChecked
    decl.type = AliasType(decl: decl).kind
    decl.parentDeclSpace = unit
    unit.decls.append(decl)

    decl = AliasTypeDecl(
      ident: "Nothing", aliasedSign: TupleSign(elems: [], type: .unit), type: .unresolved)
    decl.state = .typeChecked
    decl.type = AliasType(decl: decl).kind
    decl.parentDeclSpace = unit
    unit.decls.append(decl)
  }

  /// Synthesizes a built-in function declaration.
  private func createBuiltinFunDecl(
    ident: String,
    params: ArraySlice<String>,
    ret: String
  ) -> FunDecl {
    // Create the declaration of the function.
    let funDecl = FunDecl(ident: ident, type: .unresolved)
    funDecl.props.insert(.isBuiltin)

    // Create the declaration(s) of the function's parameter.
    var funTypeParams: [FunType.Param] = []
    for (i, param) in params.enumerated() {
      // Create the parameter's type.
      funTypeParams.append(FunType.Param(type: parse(paramTypeNamed: param)))

      // Create the declaration of the parameter.
      let decl = FunParamDecl(ident: "_\(i)", policy: .consuming, type: funTypeParams.last!.type)
      decl.parentDeclSpace = funDecl
      decl.state = .typeChecked

      funDecl.params.append(decl)
    }

    // Create the function's type.
    funDecl.type = FunType(
      params: funTypeParams,
      retType: ret.isEmpty ? .unit : parse(typeNamed: ret[ret.startIndex...]))
    funDecl.state = .typeChecked

    return funDecl
  }

  private func parse<S>(typeNamed name: S) -> ValType where S: StringProtocol {
    return name == "Unit" ? .unit : BuiltinType.get(name: String(name))!
  }

  private func parse(paramTypeNamed name: String) -> ValType {
    switch name.last {
    case "&":
      return FunParamType(policy: .local, rawType: parse(typeNamed: name.dropLast()))
    case "*":
      return FunParamType(policy: .inout, rawType: parse(typeNamed: name.dropLast()))
    default:
      return FunParamType(policy: .consuming, rawType: parse(typeNamed: name))
    }
  }

  /// The singleton instance of the built-in module declaration.
  public static let builtin: ModuleDecl = {
    let decl = ModuleDecl(ident: "_Builtin", generation: 0)
    decl.synthesizeBuiltinDecls()
    return decl
  }()

}
