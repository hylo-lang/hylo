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
      inlineCalls(in: k, definedIn: m, where: shouldInline)
    }
  }

  /// Inlines calls in `f` satisfying `shouldInline`, reading the definition of `f` from `m`.
  public mutating func inlineCalls(
    in f: Function.ID, definedIn m: Module.ID, where shouldInline: InliningPredicate
  ) {
    var work: [InstructionID] = []
    for b in modules[m]!.blocks(in: f) {
      for i in modules[m]!.instructions(in: b).map(InstructionID.init) {
        if modules[m]![i, in: f] is Call {
          work.append(i)
        }
      }
    }

    while let i = work.popLast() {
      _ = inline(functionCall: i, from: f, definedIn: m, if: shouldInline)
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
    functionCall i: InstructionID, from f: Function.ID, definedIn m: Module.ID, if shouldInline: InliningPredicate
  ) -> Bool {
    let s = modules[m]![i, in: f] as! Call

    // Can't inline the call if the callee isn't a function reference.
    guard let callee = s.callee.constant as? FunctionReference else {
      return false
    }

    // Can't inline the call if the callee is generic.
    if !callee.specialization.isEmpty {
      return false
    }

    // Can't inline if the function has no implementation.
    let source = module(defining: callee.function)
    if modules[source]![callee.function].entry == nil {
      return false
    }

    // Can't inline if `shouldInline` doesn't hold.
    if !shouldInline(callee.function, definedIn: modules[source]!) {
      return false
    }

    var translation = InliningTranslation(rewrittenBlock: [:])

    // Simplest case: the inlined function has no control flow.
    if modules[source]![callee.function].blocks.count == 1 {
      let e = Block.AbsoluteID(callee.function, modules[source]![callee.function].entry!)

      translation.rewrittenOperand[.parameter(e, s.arguments.count)] = s.output
      for (n, o) in s.arguments.enumerated() {
        translation.rewrittenOperand[.parameter(e, n)] = o
      }

      for j in modules[source]!.instructions(in: e) {
        if modules[source]![j] is Terminator { break }
        let k = self.rewrite(j, from: source, transformedBy: &translation, at: .before(AbsoluteInstructionID(f, i)), in: m)
        translation.rewrittenOperand[.register(j)] = .register(k)
      }

      modules[m]!.removeInstruction(i, in: f)
      return true
    }

    return false
  }

}

/// How to translate the contents of a function being inlined.
private struct InliningTranslation: InstructionTransformer {

  /// A map from basic block to its rewritten form in the inlined function.
  let rewrittenBlock: [Block.AbsoluteID: Block.AbsoluteID]

  /// A map from operand to its rewritten form in the inlined function.
  var rewrittenOperand: [Operand: Operand] = [:]

  /// Returns `t`.
  func transform(_ t: AnyType, in ir: inout Program) -> AnyType {
    t
  }

  /// Returns a transformed copy of `o` for use in `ir`.
  func transform(_ o: Operand, in ir: inout IR.Program) -> Operand {
    o.isConstant ? o : rewrittenOperand[o]!
  }

  /// Returns a transformed copy of `b` for use in `ir`.
  func transform(_ b: Block.AbsoluteID, in ir: inout IR.Program) -> Block.AbsoluteID {
    rewrittenBlock[b]!
  }

}
