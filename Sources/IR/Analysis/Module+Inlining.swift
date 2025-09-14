import FrontEnd
import Utils

extension IR.Program {

  /// Inlines calls in `self` satisfying `shouldInline`.
  public mutating func inlineCalls(where shouldInline: InliningPredicate) {
    for m in modules.keys {
      inlineCalls(in: m, where: shouldInline)
    }
  }

  /// Inlines calls in `m` satisfying `shouldInline`.
  public mutating func inlineCalls(in m: Module.ID, where shouldInline: InliningPredicate) {
    for k in modules[m]!.functions.keys {
      modules[m]![k].inlineCalls(program: self, where: shouldInline)
    }
  }

}

extension Function {

  /// Inlines calls in `f` satisfying `shouldInline`, reading the definition of `f` from `m`.
  fileprivate mutating func inlineCalls(program: Program, where shouldInline: InliningPredicate) {
    var work: [InstructionID] = []
    for i in instructionIDs {
      if self[i] is Call {
        work.append(i)
      }
    }

    while let i = work.popLast() {
      _ = inline(functionCall: i, program: program, if: shouldInline)
    }
  }

  /// Inlines the contents of function called by `i` if it satisfies `shouldInline`, reading the
  /// contents of the instruction from `m`.
  ///
  /// This methods returns `true` if `shouldInline` is satisfied and the callee is a reference to
  /// a non-generic function. Otherwise, it returns `false` without modifying anything.
  ///
  /// The contents of the callee is inserted in the function containing `i`, applying one "level"
  /// of inlining. For example, if `i` is a call to F, the functions called by F are not inlined.
  /// Likewise, if F is recursive, only one level of recursion is inlined.
  private mutating func inline(
    functionCall i: InstructionID, program: Program, if shouldInline: InliningPredicate
  ) -> Bool {
    let s = self[i] as! Call

    // Can't inline the call if the callee isn't a function reference.
    guard let callee = s.callee.constant as? FunctionReference else {
      return false
    }

    // Can't inline the call if the callee is generic.
    if !callee.specialization.isEmpty {
      return false
    }

    // Can't inline if `shouldInline` doesn't hold.
    let source = program.module(defining: callee.function)
    if !shouldInline(callee.function, definedIn: program.modules[source]!) {
      return false
    }

    return inline(functionCall: i, calling: program.modules[source]![callee.function])
  }

  private mutating func inline(functionCall i: InstructionID, calling f: Function) -> Bool {
    let s = self[i] as! Call

    // Can't inline if the function has no implementation.
    if f.entry == nil {
      return false
    }

    var translation = InliningTranslation(rewrittenBlock: [:])

    // Simplest case: the inlined function has no control flow.
    if f.blocks.count == 1 {
      let e = f.entry!

      translation.rewrittenOperand[.parameter(e, s.arguments.count)] = s.output
      for (n, o) in s.arguments.enumerated() {
        translation.rewrittenOperand[.parameter(e, n)] = o
      }

      for j in f.instructions(in: e) {
        if f[j] is Terminator { break }
        let k = rewrite(j, in: f, transformedBy: &translation, at: .before(i))
        translation.rewrittenOperand[.register(j)] = .register(k)
      }

      removeInstruction(i)
      return true
    }

    return false
  }

}

/// How to translate the contents of a function being inlined.
private struct InliningTranslation: InstructionTransformer {

  /// A map from basic block to its rewritten form in the inlined function.
  let rewrittenBlock: [Block.ID: Block.ID]

  /// A map from operand to its rewritten form in the inlined function.
  var rewrittenOperand: [Operand: Operand] = [:]

  /// Returns `t`.
  func transform(_ t: AnyType) -> AnyType {
    t
  }

  /// Returns a transformed copy of `o` for use in `ir`.
  func transform(_ o: Operand) -> Operand {
    o.isConstant ? o : rewrittenOperand[o]!
  }

  /// Returns a transformed copy of `b` for use in `ir`.
  func transform(_ b: Block.ID) -> Block.ID {
    rewrittenBlock[b]!
  }

}
