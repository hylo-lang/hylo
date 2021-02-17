import AST

/// A job that dumps the AST of the driver's context.
public struct DumpJob: Job {

  public init() {}

  public func run(with driver: inout Driver) {
    NodePrinter(context: driver.context).print()
  }

}
