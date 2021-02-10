import AST

/// An job that dumps the AST.
public struct DumpJob: Job {

  public init() {}

  public func run(in context: Context) {
    NodePrinter(context: context).print()
  }

}
