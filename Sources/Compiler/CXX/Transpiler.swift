import Utils

/// A Val to C++ transpiler.
public struct Transpiler {

  /// The program being transpiled.
  public let program: TypedProgram

  /// The type definitions explosed in the C++ API.
  private var publicTypeDefinitions: [NodeID<ProductTypeDecl>: CXXTypeDecl] = [:]

  /// Creates a C++ transpiler with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  /// Compiles the specified lowered module into C++.
  public func compile(module: Module) -> String {
    // Create a new compilation unit.
    var unit = CXXUnit(name: CXXIdentifier(module.name))
    unit.source.write("#include <memory>\n")
    unit.source.write("#include <new>\n")
    unit.source.write("namespace \(unit.name) {\n")

    // Transpile a functions.
    for functionID in 0 ..< module.functions.count {
      compile(function: functionID, in: module, into: &unit)
    }

    unit.source.write("} // \(unit.name)")
    return unit.source
  }

  func compile(function functionID: Function.ID, in module: Module, into unit: inout CXXUnit) {
    let function = module.functions[functionID]

    // Generate unique names for the basic blocks and operands in the function.
    var blockNames  : [Block.ID: String] = [:]
    var operandNames: [Operand: String] = [:]
    for blockIndex in function.blocks.indices {
      let blockID = Block.ID(function: functionID, index: blockIndex)
      blockNames[blockID] = "bb\(blockNames.count)"

      for i in 0 ..< function.blocks[blockIndex].inputs.count {
        operandNames[.parameter(block: blockID, index: i)] = "l\(operandNames.count)"
      }
      for instIndex in function.blocks[blockIndex].instructions.indices {
        let instID = InstID(function: functionID, block: blockIndex, index: instIndex)
        operandNames[.inst(instID)] = "l\(operandNames.count)"
      }
    }

    // Build the C++ signature of the function.
    let returnType = compile(loweredType: function.output, into: &unit)
    var signature = returnType != .never
      ? unit.describe(cxxType: returnType)
      : "[[noreturn]] void"

    signature.write(" " + function.name + "(")
    for i in 0 ..< function.inputs.count {
      if i > 0 { signature.write(", ") }
      let cxxType = compile(loweredType: function.inputs[i], into: &unit)
      signature.write(unit.describe(cxxType: cxxType))
      signature.write(" l\(i)")
    }
    signature.write(")")

    unit.source.write(signature)

    // Compile the instructions.
    unit.source.write("{\n")
    var instructions = ""

    for blockIndex in function.blocks.indices {
      let blockID = Block.ID(function: functionID, index: blockIndex)
      instructions.write(blockNames[blockID]!)
      instructions.write(":\n")

      let block = function.blocks[blockIndex]
      for instIndex in block.instructions.indices {
        let instID = InstID(function: functionID, block: blockIndex, index: instIndex)

        switch block.instructions[instIndex] {
        case let inst as AllocInst:
          let instName = operandNames[.inst(instID)]!
          let instType = compile(type: inst.objectType, into: &unit)
          let instTypeName = unit.describe(cxxType: instType)
          unit.source.write("\(instTypeName) _\(instName); ")
          unit.source.write("\(instTypeName)* \(instName) = &_\(instName);\n")

        case let inst as BranchInst:
          let targetLabel = blockNames[inst.target]!
          instructions.write("goto \(targetLabel);\n")

        case let inst as CallInst:
          let instName = operandNames[.inst(instID)]!
          let instType = inst.type.astType

          // Assign the return value of the function if there's one.
          if (instType != .never) && ((instType != .unit) || !inst.isBuiltinCall) {
            let instType = compile(loweredType: inst.type, into: &unit)
            let instTypeName = unit.describe(cxxType: instType)
            unit.source.write("\(instTypeName) \(instName);\n")
            instructions.write("\(instName) = ")
          }

          instructions.write(emit(operand: inst.callee, names: operandNames))
          instructions.write("(")
          for i in 0 ..< inst.operands.count {
            if i > 0 { instructions.write(", ") }
            instructions.write(emit(operand: inst.operands[i], names: operandNames))
          }
          instructions.write(");\n")

        case let inst as CondBranchInst:
          let t = blockNames[inst.targetIfTrue]!
          let f = blockNames[inst.targetIfFalse]!
          instructions.write("if (")
          instructions.write(emit(operand: inst.condition, names: operandNames))
          instructions.write(") { goto \(t); } else { goto \(f); }\n")

        case let inst as LoadInst:
          let instName = operandNames[.inst(instID)]!
          let instType = compile(loweredType: inst.type, into: &unit)
          let instTypeName = unit.describe(cxxType: instType)
          unit.source.write("\(instTypeName) \(instName);\n")

          instructions.write("\(instName) = *")
          instructions.write(emit(operand: inst.source, names: operandNames))
          instructions.write(";\n")

        case let inst as MemberAddrInst:
          let instName = operandNames[.inst(instID)]!
          let instType = compile(loweredType: inst.type, into: &unit)
          let instTypeName = unit.describe(cxxType: instType)
          unit.source.write("\(instTypeName) \(instName);\n")

          instructions.write("\(instName) = &")
          instructions.write(emit(operand: inst.value, names: operandNames))
          for offset in inst.path {
            instructions.write("->m\(offset)")
          }
          instructions.write(";\n")

        case let inst as ReturnInst:
          if let value = inst.value,
             module.type(of: value).astType != .unit
          {
            instructions.write("return ")
            instructions.write(emit(operand: value, names: operandNames))
            instructions.write(";\n")
          } else {
            instructions.write("return;\n")
          }

        case let inst as StoreInst:
          instructions.write("*")
          instructions.write(emit(operand: inst.target, names: operandNames))
          instructions.write(" = ")
          instructions.write(emit(operand: inst.object, names: operandNames))
          instructions.write(";\n")

        case let inst as RecordInst:
          let instName = operandNames[.inst(instID)]!
          let instType = compile(loweredType: inst.type, into: &unit)
          let instTypeName = unit.describe(cxxType: instType)
          unit.source.write("\(instTypeName) \(instName);\n")

          instructions.write("new(&\(instName)) \(instTypeName){")
          for i in 0 ..< inst.operands.count {
            if i > 0 { instructions.write(", ") }
            instructions.write(emit(operand: inst.operands[i], names: operandNames))
          }
          instructions.write("};\n")

        default:
          unreachable("unexpected instruction")
        }
      }
    }

    unit.source.write(instructions)
    unit.source.write("}\n")
  }

  func emit(operand: Operand, names operandNames: [Operand: String]) -> String {
    guard case .constant(let constant) = operand else {
      return operandNames[operand]!
    }

    switch constant {
    case .builtin(let builtin):
      return "Val::Builtin::" + builtin.name

    case .function(let function):
      return function.name

    case .integer(let integer):
      return "0x" + integer.bitPattern.hexadecimalString()

    default:
      unreachable("unexpected constant")
    }
  }

  /// Compiles `loweredType` into `unit` and returns its representation.
  ///
  /// - Requires: `loweredType` must have a C++ representation.
  func compile(loweredType: LoweredType, into unit: inout CXXUnit) -> CXXType {
    let objectType = compile(type: loweredType.astType, into: &unit)
    if loweredType.isAddress {
      return .pointer(objectType)
    } else {
      return objectType
    }
  }

  /// Compiles `type` into `unit` and returns its representation.
  ///
  /// - Requires: `type` must have a C++ representation.
  func compile(type: Type, into unit: inout CXXUnit) -> CXXType {
    switch type {
    case .builtin(let type):
      switch type {
      case .module:
        preconditionFailure("no C++ type representation")

      case .i(let bitWidth):
        switch bitWidth {
        case 1, 2, 3, 8, 16, 32, 64:
          return .fixedWidthInteger(bitWidth)
        default:
          preconditionFailure("no C++ type representation")
        }
      }

    case .product(let type):
      return compile(productType: type.decl, into: &unit)

    case .tuple(let type):
      return compile(tuple: type, into: &unit)

    case .union(let type):
      return compile(union: type, into: &unit)

    default:
      unreachable("unexpected type")
    }
  }

  private func compile(tuple type: TupleType, into unit: inout CXXUnit) -> CXXType {
    // Check if we already compiled that type.
    if let structID = unit.tupleReprs[type] {
      return .structure(structID)
    }

    switch type.elements.count {
    case 0:
      let newStructID = unit.insert(struct: CXXStruct(name: CXXIdentifier("Unit")))
      unit.tupleReprs[type] = newStructID
      return .structure(newStructID)

    case 1:
      return compile(type: type.elements[0].type, into: &unit)

    default:
      var fields: [CXXType] = []
      for element in type.elements {
        fields.append(compile(type: element.type, into: &unit))
      }

      let newStructID = unit.insert(
        struct: CXXStruct(name: unit.makeStructIdentifier(), fields: fields))
      unit.tupleReprs[type] = newStructID
      return .structure(newStructID)
    }
  }

  private func compile(union type: UnionType, into unit: inout CXXUnit) -> CXXType {
    if type.elements.count == 0 {
      return .never
    } else {
      fatalError("not implemented")
    }
  }

  private func compile(
    productType declID: NodeID<ProductTypeDecl>,
    into unit: inout CXXUnit
  ) -> CXXType {
    // Check if we already compiled that product type.
    if let structID = unit.productTypeReprs[declID] {
      return .structure(structID)
    }

    // Create a new struct.
    let locator = program.locator(identifying: declID)
    let newStructID = unit.insert(struct: CXXStruct(name: CXXIdentifier(locator.mangled)))
    unit.productTypeReprs[declID] = newStructID

    // Gather the fields of the struct.
    for memberID in program.ast[declID].members {
      guard let bindingDeclID = NodeID<BindingDecl>(converting: memberID) else { continue }
      for (_, nameID) in program.ast.names(in: program.ast[bindingDeclID].pattern) {
        let declType = program.declTypes[program.ast[nameID].decl]!
        unit.structures[newStructID].fields.append(compile(type: declType, into: &unit))
      }
    }

    // Return the identifier of the struct in `unit`.
    return .structure(newStructID)
  }

  // MARK: API

  /// Emits a C++ header file of the specified module.
  ///
  /// - Parameters:
  ///   - moduleID: The ID of the module declaration whose header is generated.
  public mutating func emitHeader(of moduleID: NodeID<ModuleDecl>) -> String {
    let moduleDecl = program.ast[moduleID]

    // Reinitialize the state of the transpiler.
    publicTypeDefinitions.removeAll()

    // Emit the header guard.
    var header = ""
    header.write("#ifndef VAL_\(moduleDecl.name.uppercased())\n")
    header.write("#define VAL_\(moduleDecl.name.uppercased())\n")

    // Emit include clauses.
    header.write("#include <utility>\n")
    header.write("#include <functional>\n")

    // Create a namespace for the entire module.
    header.write("namespace \(moduleDecl.name) {\n")

    // Collect public declarations from Val.
    for memberID in moduleDecl.members {
      switch memberID.kind {
      case .productTypeDecl:
        let typeDeclID = NodeID<ProductTypeDecl>(converting: memberID)!
        if program.ast[typeDeclID].isPublic && (publicTypeDefinitions[typeDeclID] == nil) {
          publicTypeDefinitions[typeDeclID] = CXXTypeDecl(name: program.ast[typeDeclID].name)
        }

      case .conformanceDecl, .extensionDecl, .funDecl, .namespaceDecl, .subscriptDecl:
        fatalError("not implemented")

      default:
        continue
      }
    }

    // Emit type declarations.
    for typeDeclID in publicTypeDefinitions.keys {
      header.write("class \(program.ast[typeDeclID].name);\n")
    }

    // Emit type definitions.
    for typeDeclID in publicTypeDefinitions.keys {
      header += emitTypeDeclAPI(of: typeDeclID)
    }

    header.write("}\n") // module namespace
    header.write("#endif\n") // header guard
    return header
  }

  /// Emits the definition of the specified type declaration.
  private mutating func emitTypeDeclAPI(of typeDeclID: NodeID<ProductTypeDecl>) -> String {
    var cxxDecl = CXXTypeDecl(name: program.ast[typeDeclID].name)

    for memberID in program.ast[typeDeclID].members {
      switch memberID.kind {
      case .bindingDecl:
        let bindingDeclID = NodeID<BindingDecl>(converting: memberID)!
        let bindingDecl = program.ast[bindingDeclID]

        // Stored properties are always emitted.
        let access: CXXTypeDecl.SectionAccess = bindingDecl.isPublic
          ? .public
          : .private
        cxxDecl.fields.append(contentsOf: program.ast.names(in: bindingDecl.pattern)
          .map({ (_, name) -> (String, CXXTypeDecl.SectionAccess) in
            var definition = program.ast[bindingDeclID].isStatic
              ? "static "
              : ""

            let varDeclID = program.ast[name].decl
            definition += emitTypeExpr(program.declTypes[varDeclID]!)
            definition += " "
            definition += program.ast[varDeclID].name
            definition += ";"
            return (definition, access)
          }))

      case .funDecl:
        let methodDeclID = NodeID<FunDecl>(converting: memberID)!
        let methodDecl = program.ast[methodDeclID]

        // Memberwise constructors are always emitted.
        if methodDecl.introducer.value == .memberwiseInit {
          let definition = emitMemberwiseInit(typeDeclID: typeDeclID, initDeclID: methodDeclID)
          if methodDecl.isPublic {
            cxxDecl.publicCtors.append(definition)
          } else {
            cxxDecl.privateCtors.append(definition)
          }
          continue
        }

        // Other constructors and methods are not emitted unless they are public.
        if !methodDecl.isPublic { continue }
        assert(!methodDecl.isStatic, "not implemented")

        switch methodDecl.introducer.value {
        case .memberwiseInit:
          unreachable()
        case .fun:
          cxxDecl.methods.append(contentsOf: emitPublicMethodDecl(methodDeclID))
        default:
          fatalError("not implemented")
        }

      default:
        continue
      }
    }

    var output = ""
    cxxDecl.write(into: &output)
    return output
  }

  /// Emits the definition of the specified memberwise initializer.
  private func emitMemberwiseInit(
    typeDeclID: NodeID<ProductTypeDecl>,
    initDeclID: NodeID<FunDecl>
  ) -> String {
    // Note: the first parameter of the signature is the receiver.
    guard case .lambda(let initType) = program.declTypes[initDeclID] else { unreachable() }
    var definition = "explicit \(program.ast[typeDeclID].name)"

    // Emit the parameters.
    definition += "("
    definition += initType.inputs
      .dropFirst()
      .map({ (p) -> String in "\(emitTypeExpr(p.type)) \(p.label!)" })
      .joined(separator: ", ")
    definition += ")"

    // Emit the memberwise initialization.
    if !initType.inputs.isEmpty {
      definition += ": "
      definition += initType.inputs
        .dropFirst()
        .map({ (p) -> String in "\(p.label!)(std::move(\(p.label!)))" })
        .joined(separator: ", ")
    }

    definition += " {}"
    return definition
  }

  /// Emits the declarations(s) of the specified method declaration.
  ///
  /// - Note: The method returns multiple definitions if `methodDeclID` denotes a method bundle.
  private func emitPublicMethodDecl(_ methodDeclID: NodeID<FunDecl>) -> [String] {
    let methodName = program.ast[methodDeclID].identifier!.value

    switch program.declTypes[methodDeclID] {
    case .lambda(let type):
      var signature = emitTypeExpr(type.output)
      signature += " \(methodName)("
      signature += type.inputs
        .map({ p in emitTypeExpr(p.type) })
        .joined(separator: ", ")
      signature += ")"

      if program.ast[methodDeclID].isSink {
        signature += " &&"
      } else if !program.ast[methodDeclID].isSink {
        signature += " const"
      }

      signature += ";"
      return [signature]

    case .method(let type):
      // The input/output is the same for all variants.
      let output = emitTypeExpr(type.output)
      let inputs = type.inputs
        .map({ p in emitTypeExpr(p.type) })
        .joined(separator: ", ")

      return type.capabilities.map({ (capability) -> String in
        switch capability {
        case .let:
          return "\(output) \(methodName)(\(inputs)) const;"
        case .inout:
          return "void inplace_\(methodName)(\(inputs));"
        case .sink:
          return "\(output) sink_\(methodName)(\(inputs)) &&;"
        }
      })

    default:
      unreachable()
    }
  }

  /// Emits the C++ expression of the given type.
  private func emitTypeExpr(_ type: Type) -> String {
    switch type {
    case .parameter(let type):
      switch type.convention {
      case .let:
        return emitTypeExpr(type.bareType) + " const&"
      case .inout:
        return emitTypeExpr(type.bareType) + "&"
      case .sink:
        return emitTypeExpr(type.bareType) + "&&"
      default:
        unreachable("unexpected parameter passing convention")
      }

    case .product(let type):
      var nameComponents: [String] = []
      for scopeID in program.scopeHierarchy.scopesToRoot(from: type.decl) {
        switch scopeID.kind {
        case .moduleDecl:
          nameComponents.append(program.ast[NodeID<ModuleDecl>(unsafeRawValue: scopeID.rawValue)].name)

        default:
          if let decl = program.ast[scopeID] as? SingleEntityDecl {
            nameComponents.append(decl.name)
          }
        }

      }
      return nameComponents.reversed().joined(separator: "::")

    default:
      unreachable("unexpected type")
    }
  }

}
