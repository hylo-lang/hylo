import Basic

/// A thread managed by the virtual machine (a.k.a. a green thread).
struct VirtualThread {

  typealias ID = Int

  /// The register stack of the thread.
  var registerStack: RegisterStack

  /// The call stack of the thread.
  var callStack: CallStack

  /// The current program counter of the thread.
  var programCounter = InstAddr.null

  /// The result of the thread, once it has returned from its entry point.
  var result: RuntimeValue?

  init(
    id: VirtualThread.ID,
    regiterStackCapacity: Int,
    callStackCapacity: Int
  ) {
    self.registerStack = RegisterStack(initialCapacity: regiterStackCapacity)
    self.callStack = CallStack(threadID: id, initialCapacity: callStackCapacity)
  }

}
