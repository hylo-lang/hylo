import Utils

/// A module lowered to Val IR.
public struct Module {

  /// The module's declaration.
  public let decl: NodeID<ModuleDecl>

  /// The module's name.
  public let name: String

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Operand: [Use]] = [:]

  /// The functions in the module.
  public private(set) var functions: [Function] = []

  /// A table mapping function declarations to their ID in the module.
  private var loweredFunctions: [NodeID<FunDecl>: Function.ID] = [:]

  public init(decl: NodeID<ModuleDecl>, id: String) {
    self.decl = decl
    self.name = id
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> LoweredType {
    switch operand {
    case .result(let inst, let index):
      return functions[inst.function][inst.block][inst.address].types[index]

    case .parameter(let block, let index):
      return functions[block.function][block.address].inputs[index]

    case .constant(let constant):
      return constant.type
    }
  }

  /// Returns whether the module is well-formed.
  public func check() -> Bool {
    for i in 0 ..< functions.count {
      if !check(function: i) { return false }
    }
    return true
  }

  /// Returns whether the specified function is well-formed.
  public func check(function functionID: Function.ID) -> Bool {
    for block in functions[functionID].blocks {
      for inst in block.instructions {
        if !inst.check(in: self) { return false }
      }
    }
    return true
  }

  /// Returns the identifier of the Val IR function corresponding to `declID`.
  mutating func getOrCreateFunction(
    from declID: NodeID<FunDecl>,
    ast: AST,
    withScopeHierarchy scopeHierarchy: ScopeHierarchy,
    withDeclTypes declTypes: DeclMap<Type>
  ) -> Function.ID {
    if let id = loweredFunctions[declID] { return id }

    // Determine the type of the function.
    var inputs: [Function.Input] = []
    let output: LoweredType

    switch declTypes[declID] {
    case .lambda(let declType):
      output = LoweredType(lowering: declType.output)
      inputs.reserveCapacity(declType.captures.count + declType.inputs.count)

      // Define inputs for the captures.
      for capture in declType.captures {
        switch capture.type {
        case .projection(let type):
          precondition(type.capability != .yielded, "cannot lower yielded parameter")
          inputs.append((
            convention: PassingConvention(matching: type.capability),
            type: .address(type.base)))

        case let type:
          switch declType.operatorProperty {
          case nil:
            inputs.append((convention: .let, type: .address(type)))
          case .inout:
            inputs.append((convention: .inout, type: .address(type)))
          case .sink:
            inputs.append((convention: .sink, type: .object(type)))
          }
        }
      }

      // Define inputs for the parameters.
      for parameter in declType.inputs {
        switch parameter.type {
        case .parameter(let type):
          precondition(type.convention != .yielded, "cannot lower yielded parameter")
          inputs.append((convention: type.convention, type: .address(type.bareType)))

        default:
          unreachable()
        }
      }

    default:
      unreachable()
    }

    // Declare a new function in the module.
    let id = functions.count
    let locator = DeclLocator(
      identifying: declID,
      in: ast,
      withScopeHierarchy: scopeHierarchy,
      withDeclTypes: declTypes)
    let function = Function(
      name: locator.mangled,
      debugName: locator.description,
      linkage: ast[declID].isPublic ? .external : .module,
      inputs: inputs,
      output: output,
      blocks: [])
    functions.append(function)

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[declID] = id
    return id
  }

  /// Creates a basic block at the end of the specified function and returns its identifier.
  @discardableResult
  mutating func createBasicBlock(
    accepting inputs: [LoweredType] = [],
    atEndOf function: Function.ID
  ) -> Block.ID {
    let address = functions[function].blocks.append(Block(inputs: inputs))
    return Block.ID(function: function, address: address)
  }

  /// Inserts `inst` at the specified insertion point.
  @discardableResult
  mutating func insert<I: Inst>(_ inst: I, at ip: InsertionPoint) -> [Operand] {
    // Inserts the instruction.
    let address: Block.InstAddress
    switch ip.position {
    case .end:
      address = functions[ip.block.function][ip.block.address].instructions.append(inst)
    case .after(let i):
      address = functions[ip.block.function][ip.block.address].instructions.insert(inst, after: i)
    case .before(let i):
      address = functions[ip.block.function][ip.block.address].instructions.insert(inst, before: i)
    }

    // Generate an instruction identifier.
    let userID = InstID(function: ip.block.function, block: ip.block.address, address: address)

    // Update the use lists of the instruction's operands.
    for i in 0 ..< inst.operands.count {
      uses[inst.operands[i], default: []].append(Use(user: userID, index: i))
    }

    return (0 ..< inst.types.count).map({ k in .result(inst: userID, index: k) })
  }

}

extension Module {

  public typealias FunctionIndex = Int

  public subscript(_ position: FunctionIndex) -> Function {
    _read   { yield functions[position] }
    _modify { yield &functions[position] }
  }

}
