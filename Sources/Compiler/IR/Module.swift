import Utils

/// A module lowered to Val IR.
///
/// An IR module is notionally composed of a collection of functions, one of which may be
/// designated as its entry point (i.e., the `main` function of a Val program).
public struct Module {

/// The form in which a `Module` exposes all its lowered functions.
  public typealias Functions = [Function]

  /// The program defining the functions in `self`.
  public let program: TypedProgram

  /// The module's declaration.
  public let decl: ModuleDecl.Typed

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Operand: [Use]] = [:]

  /// The functions in the module.
  public private(set) var functions: Functions = []

  /// The ID of the module's entry function, if any.
  public private(set) var entryFunctionID: Function.ID?

  /// A map from function declaration its ID in the module.
  private var loweredFunctions: [FunctionDecl.Typed: Function.ID] = [:]

  /// Creates an IR module lowering `decl` from `program`.
  ///
  /// - Requires: `decl` must be a valid module declaration in `program`.
  public init(_ decl: NodeID<ModuleDecl>, in program: TypedProgram) {
    self.program = program
    self.decl = program[decl]

    var emitter = Emitter(program: program)
    for member in program.ast.topLevelDecls(decl) {
      emitter.emit(topLevel: program[member], into: &self)
    }
  }

  /// The module's name.
  public var name: String { decl.name }

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

  /// Returns whether the IR in `self` is well-formed.
  ///
  /// Use this method as a sanity check to verify the module's invariants.
  public func isWellFormed() -> Bool {
    for i in 0 ..< functions.count {
      if !isWellFormed(function: i) { return false }
    }
    return true
  }

  /// Returns whether `f` is well-formed.
  ///
  /// Use this method as a sanity check to verify the function's invariants.
  public func isWellFormed(function f: Function.ID) -> Bool {
    for block in functions[f].blocks {
      for inst in block.instructions {
        if !inst.isWellFormed(in: self) { return false }
      }
    }
    return true
  }

  /// Returns the identifier of the Val IR function corresponding to `decl`.
  mutating func getOrCreateFunction(
    correspondingTo decl: FunctionDecl.Typed,
    program: TypedProgram
  ) -> Function.ID {
    if let id = loweredFunctions[decl] { return id }

    // Determine the type of the function.
    var inputs: [Function.Input] = []
    let output: LoweredType

    switch decl.type.base {
    case let declType as LambdaType:
      output = LoweredType(lowering: declType.output)
      inputs.reserveCapacity(declType.captures.count + declType.inputs.count)

      // Define inputs for the captures.
      for capture in declType.captures {
        switch capture.type.base {
        case let type as RemoteType:
          precondition(type.capability != .yielded, "cannot lower yielded parameter")
          inputs.append((convention: type.capability, type: .address(type.base)))

        case let type:
          precondition(declType.receiverEffect != .yielded, "cannot lower yielded parameter")
          inputs.append((convention: declType.receiverEffect ?? .let, type: .address(type)))
        }
      }

      // Define inputs for the parameters.
      for parameter in declType.inputs {
        let parameterType = parameter.type.base as! ParameterType
        inputs.append(parameterType.asIRFunctionInput())
      }

    case is MethodType:
      fatalError("not implemented")

    default:
      unreachable()
    }

    // Declare a new function in the module.
    let loweredID = functions.count
    let locator = DeclLocator(identifying: decl.id, in: program)
    let function = Function(
      name: locator.mangled,
      debugName: locator.description,
      linkage: decl.isPublic ? .external : .module,
      inputs: inputs,
      output: output,
      blocks: [])
    functions.append(function)

    // Determine if the new function is the module's entry.
    if decl.scope.kind == TopLevelDeclSet.self,
       decl.isPublic,
       decl.identifier?.value == "main"
    {
      assert(entryFunctionID == nil)
      entryFunctionID = loweredID
    }

    // Update the cache and return the ID of the newly created function.
    loweredFunctions[decl] = loweredID
    return loweredID
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

  public subscript(_ position: Functions.Index) -> Function {
    _read   { yield functions[position] }
    _modify { yield &functions[position] }
  }

}
