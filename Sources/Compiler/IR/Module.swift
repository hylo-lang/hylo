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

  /// Accesses the function at given index.
  public subscript(function index: Functions.Index) -> Function {
    _read { yield functions[index] }
    _modify { yield &functions[index] }
  }

  /// Accesses the basic block with the given identity.
  public subscript(block id: Block.ID) -> Block {
    _read { yield functions[id.function].blocks[id.address] }
    _modify { yield &functions[id.function].blocks[id.address] }
  }

  /// Accesses the instruction with the given identity.
  public subscript(instruction id: InstructionID) -> Instruction {
    _read { yield functions[id.function].blocks[id.block].instructions[id.address] }
    _modify { yield &functions[id.function].blocks[id.block].instructions[id.address] }
  }

  /// Returns the type of `operand`.
  public func type(of operand: Operand) -> LoweredType {
    switch operand {
    case .result(let instruction, let index):
      return functions[instruction.function][instruction.block][instruction.address].types[index]

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
    for i in 0..<functions.count {
      if !isWellFormed(function: i) { return false }
    }
    return true
  }

  /// Returns whether `f` is well-formed.
  ///
  /// Use this method as a sanity check to verify the function's invariants.
  public func isWellFormed(function f: Function.ID) -> Bool {
    for block in functions[f].blocks {
      for instruction in block.instructions {
        if !instruction.isWellFormed(in: self) { return false }
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

  /// Returns the global "past the end" position of `block`.
  func globalEndIndex(of block: Block.ID) -> InstructionIndex {
    InstructionIndex(
      block: block,
      index: functions[block.function].blocks[block.address].instructions.endIndex)
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    if let a = functions[block.function].blocks[block.address].instructions.lastAddress {
      return InstructionID(block: block, address: a)
    } else {
      return nil
    }
  }

  /// Adds `newInstruction` at the end of `block` and returns the identities of its return values.
  @discardableResult
  mutating func append<I: Instruction>(_ newInstruction: I, to block: Block.ID) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        InstructionID(block: block, address: m[block: block].instructions.append(newInstruction))
      })
  }

  /// Inserts `newInstruction` at `position` and returns the identities of its return values.
  ///
  /// The instruction is inserted before the instruction currently at `position`. You can pass a
  /// "past the end" position to append at the end of a block.
  @discardableResult
  mutating func insert<I: Instruction>(_ newInstruction: I, at position: InstructionIndex)
    -> [Operand]
  {
    insert(
      newInstruction,
      with: { (m, i) in
        let address = m.functions[position.function].blocks[position.block].instructions
          .insert(newInstruction, at: position.index)
        return InstructionID(function: position.function, block: position.block, address: address)
      })
  }

  /// Inserts `newInstruction` before the instruction identified by `id` and returns the identities
  /// of its results.
  @discardableResult
  mutating func insert<I: Instruction>(_ newInstruction: I, before id: InstructionID) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        let address = m.functions[id.function].blocks[id.block].instructions
          .insert(newInstruction, before: id.address)
        return InstructionID(function: id.function, block: id.block, address: address)
      })
  }

  /// Inserts `newInstruction` after the instruction identified by `id` and returns the identities
  /// of its results.
  @discardableResult
  mutating func insert<I: Instruction>(_ newInstruction: I, after id: InstructionID) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        let address = m.functions[id.function].blocks[id.block].instructions
          .insert(newInstruction, after: id.address)
        return InstructionID(function: id.function, block: id.block, address: address)
      })
  }

  /// Inserts `newInstruction` with `impl` and returns the identities of its return values.
  private mutating func insert<I: Instruction>(
    _ newInstruction: I,
    with impl: (inout Self, I) -> InstructionID
  ) -> [Operand] {
    // Insert the instruction.
    let user = impl(&self, newInstruction)

    // Update the def-use chains.
    for i in 0..<newInstruction.operands.count {
      uses[newInstruction.operands[i], default: []].append(Use(user: user, index: i))
    }

    // Return the identities of the instruction's results.
    return (0..<newInstruction.types.count).map({ (k) -> Operand in
      .result(instruction: user, index: k)
    })
  }

}
