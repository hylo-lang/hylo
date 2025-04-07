/// Types that support a contextual mutation pattern.
///
/// A contextual mutation method encapsulates setup and teardown steps
/// around some arbitrary mutating operation, expressed as a block.
///
/// A contextually-executed block has the form:
///
///     for _z in inSomeContext(...) where _do(_z) {
///       ...
///     }
///
/// where: `Self: ContextuallyMutable` and `inSomeContext(...)`
/// prepares some context and returns a `ContextualMutation<Self>`
/// capturing the teardown of that context.
///
public protocol ContextuallyMutable {
  var contextualMutations: ContextualMutation<Self>.Factory { get set }
}

public extension ContextuallyMutable {

  /// In the second iteration of a contextual mutation loop, applies
  /// the completion action captured in `m` and returns `false`;
  /// returns `true` otherwise.
  mutating func _do(_ m: ContextualMutation<Self>) -> Bool {
    // print("do with phase", m.phase)
    precondition(m.phase < 2, "you didn't follow the contextual mutation pattern")
    if m.phase == 0 {
      precondition(
        m.enclosingMutation == contextualMutations.currentMutation,
        """
        Starting mutation \(m.id) with enclosing mutation \(m.enclosingMutation), \
        but mutation \(contextualMutations.currentMutation) is current.  Did you \
        call '_do(x)' on the wrong 'x'?
        """
      )
      contextualMutations.currentMutation = m.id
      return true
    }

    precondition(
      contextualMutations.currentMutation == m.id,
      """
      Finishing mutation \(m.id), \
      but mutation \(contextualMutations.currentMutation) is current.  Did you \
      break or throw out of a contextual mutation block?
      """)

    contextualMutations.currentMutation = m.enclosingMutation
    m.completionAction(&self)
    return false
  }

}

/// A notional mutation in progress with an action to be taken upon completion.
public struct ContextualMutation<Operand: ContextuallyMutable>: Sequence, IteratorProtocol {

  typealias ID = Int

  public struct Factory {
    var currentMutation: ID = -1
    var nextMutation: ID = 0

    mutating func makeMutation(
      finally completionAction: @escaping (inout Operand)->Void
    ) -> ContextualMutation<Operand> {
      defer { nextMutation += 1 }
      return .init(id: nextMutation, enclosingMutation: currentMutation, finally: completionAction)
    }
  }

  /// An instance with the given `completionAction`.
  public init(id: Int, enclosingMutation: Int, finally completionAction: @escaping (inout Operand)->Void)
  {
    self.id = id
    self.enclosingMutation = enclosingMutation
    self.completionAction = completionAction
  }

  fileprivate var id: Int
  fileprivate var enclosingMutation: Int

  /// 0 := entering the context, 1 := exiting the context, 2 := done
  fileprivate var phase: Int = 0

  /// The action to take upon completion
  fileprivate let completionAction: (inout Operand)->Void

  public mutating func next() -> Self? {
    if phase == 2 { return nil }
    defer { phase += 1 }
    return self
  }

}

// ======================= Usage ==========================

/// A type with example block execution contexts.
struct X: ContextuallyMutable {
  var contextualMutations = ContextualMutation<X>.Factory()

  /// Example state.
  var currentMessage = "hello"
  var counter = 0

  mutating func increment() { counter += 1 }
  mutating func decrement() { counter -= 1 }

  mutating func withIncrementedCount() -> ContextualMutation<Self> {
    increment()
    return contextualMutations.makeMutation(finally: { $0.decrement() })
  }

  /// Context function executing the following block with `currentMessage` set to `m()`
  mutating func withMessage(_ m: String) -> ContextualMutation<Self> {
    let savedMessage = currentMessage
    currentMessage = m
    return contextualMutations.makeMutation(finally: { $0.currentMessage = savedMessage })
  }

  mutating func doIt() {
    print(currentMessage)
    if let _z = withMessage("inside") {
      defer { _done(_z) }
      print(currentMessage)

      if let _z = withMessage("deeper") {
        defer { _done(_z) }
        print(currentMessage)
      }
      print(currentMessage)
    }
    print(currentMessage)
    if let _z = withMessage("around") {
      defer { _done(_z) }
      print(currentMessage)
    }
    print(currentMessage)

    print(counter)
    if let _z = withIncrementedCount() {
      defer { _done(_z) }
      print(counter)
    }
    print(counter)
  }
}

var a = X()
a.doIt()
