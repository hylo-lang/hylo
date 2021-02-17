import Eval
import VIL

/// A job that evaluates a VIL module.
///
/// Thisjob will read a VIL module from the top of the driver's stack and evaluate, assuming it is
/// the program's main entry point.
public struct EvalJob: Job {

  public init() {
  }

  public func run(with driver: inout Driver) throws {
    guard let module = driver.stack.last as? Module else { return }
    var interpreter = Interpreter()
    try interpreter.eval(module: module)
  }

}
