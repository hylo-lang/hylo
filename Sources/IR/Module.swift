import Core
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

  /// The module's syntax.
  public let syntax: ModuleDecl.Typed

  /// The def-use chains of the values in this module.
  public private(set) var uses: [Operand: [Use]] = [:]

  /// The functions in the module.
  public private(set) var functions: Functions = []

  /// The ID of the module's entry function, if any.
  public private(set) var entryFunctionID: Function.ID?

  /// A map from function declaration its ID in the module.
  private var loweredFunctions: [FunctionDecl.Typed: Function.ID] = [:]

  /// Creates an instance lowering `syntax` in `program`, reporting errors and warnings to
  /// `diagnostics`.
  ///
  /// - Requires: `syntax` is a valid ID `program`.
  /// - Throws: `Diagnostics` if lowering fails.
  public init(
    lowering syntax: NodeID<ModuleDecl>,
    in program: TypedProgram,
    diagnostics: inout DiagnosticSet
  ) throws {
    self.program = program
    self.syntax = program[syntax]

    var emitter = Emitter(program: program)
    for d in program.ast.topLevelDecls(syntax) {
      emitter.emit(topLevel: program[d], into: &self, diagnostics: &diagnostics)
    }
    try diagnostics.throwOnError()
  }

  /// The module's name.
  public var name: String { syntax.baseName }

  /// Accesses the given function.
  public subscript(f: Functions.Index) -> Function {
    _read { yield functions[f] }
    _modify { yield &functions[f] }
  }

  /// Accesses the given block.
  public subscript(b: Block.ID) -> Block {
    _read { yield functions[b.function].blocks[b.address] }
    _modify { yield &functions[b.function].blocks[b.address] }
  }

  /// Accesses the given instruction.
  public subscript(i: InstructionID) -> Instruction {
    _read { yield functions[i.function].blocks[i.block].instructions[i.address] }
    _modify { yield &functions[i.function].blocks[i.block].instructions[i.address] }
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
      anchor: decl.introducerSite.first(),
      linkage: decl.isPublic ? .external : .module,
      inputs: inputs,
      output: output,
      blocks: [])
    functions.append(function)

    // Determine if the new function is the module's entry.
    if decl.scope.kind == TranslationUnit.self,
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
      block, functions[block.function].blocks[block.address].instructions.endIndex)
  }

  /// Returns the global identity of `block`'s terminator, if it exists.
  func terminator(of block: Block.ID) -> InstructionID? {
    if let a = functions[block.function].blocks[block.address].instructions.lastAddress {
      return InstructionID(block, a)
    } else {
      return nil
    }
  }

  /// Adds `newInstruction` at the end of `block` and returns the identities of its return values.
  @discardableResult
  mutating func append<I: Instruction>(
    _ newInstruction: I,
    to block: Block.ID
  ) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        InstructionID(block, m[block].instructions.append(newInstruction))
      })
  }

  /// Inserts `newInstruction` at `position` and returns the identities of its return values.
  ///
  /// The instruction is inserted before the instruction currently at `insertionPoint`. You can
  /// pass a "past the end" position to append at the end of a block.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    at insertionPoint: InstructionIndex
  ) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        let address =
          m
          .functions[insertionPoint.function].blocks[insertionPoint.block].instructions
          .insert(newInstruction, at: insertionPoint.index)
        return InstructionID(insertionPoint.function, insertionPoint.block, address)
      })
  }

  /// Inserts `newInstruction` before the instruction identified by `successor` and returns the
  /// identities of its results.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    before successor: InstructionID
  ) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        let address = m.functions[successor.function].blocks[successor.block].instructions
          .insert(newInstruction, before: successor.address)
        return InstructionID(successor.function, successor.block, address)
      })
  }

  /// Inserts `newInstruction` after the instruction identified by `predecessor` and returns the
  /// identities of its results.
  @discardableResult
  mutating func insert<I: Instruction>(
    _ newInstruction: I,
    after predecessor: InstructionID
  ) -> [Operand] {
    insert(
      newInstruction,
      with: { (m, i) in
        let address = m.functions[predecessor.function].blocks[predecessor.block].instructions
          .insert(newInstruction, after: predecessor.address)
        return InstructionID(predecessor.function, predecessor.block, address)
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
    for i in 0 ..< newInstruction.operands.count {
      uses[newInstruction.operands[i], default: []].append(Use(user: user, index: i))
    }

    // Return the identities of the instruction's results.
    return (0 ..< newInstruction.types.count).map({ (k) -> Operand in
      .result(instruction: user, index: k)
    })
  }

}
