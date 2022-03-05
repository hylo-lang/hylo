extension DeclSpace {

  public func lookup(unqualified name: String, in context: Compiler) -> LookupResult {
    var space: DeclSpace = self
    var result = LookupResult()

    // Only function declarations are overloadable. Hence, as soon as we find a non-overloadable
    // symbol, we should skip all non-overloadable declaration.
    var hasNonOverloadableDecl = false

    while true {
      // Enumerate the symbols declared directly within the current space.
      var locals = space.lookup(qualified: name)
      if hasNonOverloadableDecl {
        locals = locals.filter({ $0.isOverloadable })
      } else {
        hasNonOverloadableDecl = locals.contains(where: { !$0.isOverloadable })
      }

      result.append(contentsOf: locals)
      if let parent = space.parentDeclSpace {
        space = parent
      } else if let stdlib = context.stdlib, space !== stdlib {
        space = stdlib
      } else {
        break
      }
    }

    // Handle the implicit import of the built-in module in the standard library.
    if context.isCompilingStdlib && (name == "Builtin") {
      result.types.append(context.builtin)
    }

    return result
  }

}

extension IterableDeclSpace {

  public func lookup(qualified name: String) -> LookupResult {
    var types : [TypeDecl]  = []
    var values: [ValueDecl] = []

    for node in decls where node.state != .invalid {
      switch node {
      case let typeDecl as TypeDecl where typeDecl.ident == name:
        types.append(typeDecl)

      case let valueDecl as ValueDecl where valueDecl.ident == name:
        values.append(valueDecl)

      case let pbDecl as PatternBindingDecl:
        for pattern in pbDecl.pattern.namedPatterns where pattern.decl.ident == name {
          values.append(pattern.decl)
        }

      default:
        continue
      }
    }

    return LookupResult(types: types, values: values)
  }

}
