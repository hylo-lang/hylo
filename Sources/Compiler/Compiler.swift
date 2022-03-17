import Foundation
import Utils

/// A central repository for AST nodes and data shared across compilation phases.
public final class Compiler {

  /// Creates a new compiler instance.
  public init() {}

  // MARK: General properties

  /// A flag that indicates whether the compiler is processing the standard library.
  ///
  /// The standard library requires the compiler to behave slightly differently, so as to to handle
  /// built-in definitions. This flag enables this
  public var isCompilingStdlib = false

  /// The modules loaded in the context.
  public var modules: [String: ModuleDecl] = [:]

  /// The current generation number of the context, denoting the number of times new modules have
  /// been loaded.
  ///
  /// This number serves to determine whether name lookup caches are up to date.
  public var generation = 0

  // MARK: Standard library

  /// The standard library.
  public var stdlib: ModuleDecl?

  /// Returns the declaration of a type from the standard library.
  ///
  /// - Parameter id: The identifier of a type from the standard library.
  ///
  /// - Note: This method always returns `nil` until the standard library has been parsed and
  ///   loaded into the context.
  public func getTypeDecl(for id: KnownStdTypes) -> TypeDecl? {
    guard let stdlib = self.stdlib else { return nil }
    return stdlib.lookup(unqualified: id.name, in: self).types.first
  }

  public var nilType: ProductType {
    return getTypeDecl(for: .Nil)!.instanceType as! ProductType
  }

  public var copyableType: ViewType {
    return getTypeDecl(for: .Copyable)!.instanceType as! ViewType
  }

  // MARK: Debugging

  /// Dumps the contents of the context into the standard output.
  public func dump() {
    var stream = StandardOutput()
    print(to: &stream)
  }

  /// Dumps the AST into the given stream.
  public func dumpAST<S>(to stream: inout S) where S: TextOutputStream {
    var printer = NodePrinter()

    stream.write("[")
    var isFirst = true
    for module in modules.values {
      if isFirst {
        isFirst = false
      } else {
        stream.write(",")
      }
      stream.write(printer.visit(module))
    }
    stream.write("]")
  }

}

@available(*, deprecated)
let _ctx: Compiler! = nil
