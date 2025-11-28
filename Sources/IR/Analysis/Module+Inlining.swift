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
    // Keep inlining until no more calls can be inlined (fixed-point iteration)
    var madeProgress = true
    while madeProgress {
      madeProgress = false
      
      // Collect all call instructions in the function
      var work: [InstructionID] = []
      for b in modules[m]!.blocks(in: f) {
        for i in modules[m]!.instructions(in: b) {
          if modules[m]![i] is Call {
            work.append(i)
          }
        }
      }

      // Try to inline each call. We process the work list in reverse order
      // to inline inner calls before outer ones when possible.
      while let i = work.popLast() {
        if inline(functionCall: i, definedIn: m, if: shouldInline) {
          madeProgress = true
        }
      }
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
  ///
  /// This method supports inlining of monomorphized generic functions. When a generic function
  /// is monomorphized, it becomes a concrete function with no generic parameters, even though
  /// the FunctionReference may still carry specialization arguments. Such functions are safe to
  /// inline because they operate in a fully concrete context with no runtime witnesses.
  private mutating func inline(
    functionCall i: InstructionID, definedIn m: Module.ID, if shouldInline: InliningPredicate
  ) -> Bool {
    let s = modules[m]![i] as! Call

    // Can't inline the call if the callee is not a constant.
    guard s.callee.isConstant else {
      print("Failed to inline call because callee is not constant `\(s.callee)`")
      return false
    }

    // Can't inline the call if the callee isn't a function reference.
    guard let callee = s.callee.constant as? FunctionReference else {
      print("Failed to inline call because callee is not a function reference `\(s.callee)`")
      return false
    }

    // Extract function name for debugging
    let functionName: String
    if case .lowered(let declId) = callee.function.value,
        let functionDeclId = FunctionDecl.ID(declId),
        let identifier = modules[m]!.program.ast[functionDeclId].identifier?.value {
      functionName = identifier
    } else {
      functionName = "\(callee.function)"
    }

    // print("Attempting to inline call to `\(functionName)`")

    // Can't inline if the function has no implementation.
    let source = module(defining: callee.function)
    let sourceModule = modules[source]!
    guard let calleeFunction = sourceModule.functions[callee.function] else {
      print("Failed to inline call to `\(functionName)` because function not found in module")
      return false
    }
    if calleeFunction.entry == nil {
      print("Failed to inline call to `\(functionName)` because function has no implementation")
      return false
    }

    // Can't inline if `shouldInline` doesn't hold.
    if !shouldInline(callee.function, definedIn: sourceModule) {
      print("Failed to inline call to `\(functionName)` because predicate not satisfied")
      return false
    }

    var translation = InliningTranslation(rewrittenBlock: [:])

    // Simplest case: the inlined function has no control flow.
    if calleeFunction.blocks.count == 1 {
      let e = Block.ID(callee.function, calleeFunction.entry!)

      translation.rewrittenOperand[.parameter(e, s.arguments.count)] = s.output
      for (n, o) in s.arguments.enumerated() {
        translation.rewrittenOperand[.parameter(e, n)] = o
      }

      for j in sourceModule.instructions(in: e) {
        if sourceModule[j] is Terminator { break }
        let k = self.rewrite(j, from: source, transformedBy: &translation, at: .before(i), in: m)
        translation.rewrittenOperand[.register(j)] = .register(k)
      }

      modules[m]!.removeInstruction(i)

      // print("    -> Successfully inlined call to `\(functionName)`")
      return true
    }

    print("Failed to inline call to `\(functionName)` because it has \(calleeFunction.blocks.count) blocks.")

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
  func transform(_ t: AnyType, in ir: inout Program) -> AnyType {
    t
  }

  /// Returns a transformed copy of `o` for use in `ir`.
  func transform(_ o: Operand, in ir: inout IR.Program) -> Operand {
    o.isConstant ? o : rewrittenOperand[o]!
  }

  /// Returns a transformed copy of `b` for use in `ir`.
  func transform(_ b: Block.ID, in ir: inout IR.Program) -> Block.ID {
    rewrittenBlock[b]!
  }

}
