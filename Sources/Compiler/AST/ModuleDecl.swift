/// A module declaration.
///
/// A module (a.k.a. compilation unit) is an abstract collection of types and function declared in
/// one or several file units. It is internally treated as a type, whose members are the top-level
/// declarations that are defined within its file units.
///
/// Module declarations conform the `Collection` protocol, allowing iteration over all top-level
/// entities.
public final class ModuleDecl {

  public private(set) var state = DeclState.realized

  public var name: String

  public private(set) var type: ValType

  /// The generation number of the module.
  public let generation: Int

  /// The dependencies of the module.
  public var dependencies: Set<ModuleDecl> = []

  /// The file units in the module.
  public var units: [FileUnit] = []

  public init(name: String, generation: Int, context: Compiler) {
    self.name = name
    self.generation = generation
    self.type = context.unresolvedType
    self.type = ModuleType(context: context, module: self).kind
  }

  public var parentDeclSpace: DeclSpace? {
    get { nil }
    set { precondition(newValue == nil) }
  }

  public func setState(_ newState: DeclState) {
    assert(newState.rawValue >= state.rawValue)
    state = newState
  }

  /// Returns the extensions of the given type declaration.
  public func extensions(of decl: GenericTypeDecl) -> [ExtensionDecl] {
    func delegate(_ d: GenericTypeDecl) -> GenericTypeDecl {
      if let alias = (d as? AliasTypeDecl)?.aliasedSign.type as? NominalType {
        return delegate(alias.decl)
      } else {
        return d
      }
    }

    // Follow chain of aliases to identify the name being extended.
    let decl = delegate(decl)

    // Loop through all extensions in the module, (partially) binding them if necessary.
    var matches: [ExtensionDecl] = []
    stmt:for case let ext as ExtensionDecl in decls where ext.state != .invalid {
      // Check if the extension is already bound to decl.
      if ext.state >= .realized {
        if let d = ext.extendedDecl, delegate(d) === decl {
          matches.append(ext)
        }
        continue stmt
      }

      // The extension isn't bound yet: we must resolve its identifier.
      if ext.extendedIdent is BareIdentSign {
        if let d = ext.extendedDecl, delegate(d) === decl {
          matches.append(ext)
        }
        continue stmt
      }

      // If the identifier is a `CompoundIdentSign`, we can't realize the full signature at once,
      // as we may risk to trigger infinite recursion of name lookups if the signature points
      // within `decl`. Instead, we must realize it lazily and abort if we detect that we're
      // about to start a lookup from `decl`.
      let compound = ext.extendedIdent as! CompoundIdentSign
      var compType = compound.components[0].realize(unqualifiedFrom: self)
      var compDecl = (compType as? NominalType)?.decl

      for i in 1 ..< compound.components.count {
        // We should give up if the signature doesn't denote a norminal type, or if it refers to a
        // type nested inside of `decl`.
        guard var d = compDecl else { continue stmt }
        d = delegate(d)
        guard d !== decl else { continue stmt }

        // Realize the next component.
        compType = compound.components[i].realize(in: d.instanceType, from: self)
        compDecl = (compType as? NominalType)?.decl
      }

      // Check if the declaration to which the signature resolves is `decl`.
      if compDecl === decl {
        matches.append(ext)
      }
    }

    return matches
  }

}

extension ModuleDecl: BidirectionalCollection, MutableCollection {

  public var startIndex: Index {
    let i = Index(unitIndex: 0, declIndex: 0)
    return !units.isEmpty && units[0].decls.isEmpty
      ? index(after: i)
      : i
  }

  public var endIndex: Index {
    return Index(unitIndex: units.count, declIndex: 0)
  }

  public func index(after i: Index) -> Index {
    var newIndex = i

    while true {
      newIndex.declIndex += 1
      if newIndex.declIndex >= units[newIndex.unitIndex].decls.count {
        newIndex.unitIndex += 1
        newIndex.declIndex = 0
        guard newIndex.unitIndex < units.count else { return newIndex }
      }

      if !units[newIndex.unitIndex].decls.isEmpty {
        return newIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    var newIndex = i

    while true {
      newIndex.declIndex -= 1
      if newIndex.declIndex < 0 {
        newIndex.unitIndex -= 1
        newIndex.declIndex = units[newIndex.unitIndex].decls.count - 1
      }

      if !units[newIndex.unitIndex].decls.isEmpty {
        return newIndex
      }
    }
  }

  public subscript(position: Index) -> Decl {
    get { units[position.unitIndex].decls[position.declIndex] }
    set { units[position.unitIndex].decls[position.declIndex] = newValue }
  }

  public struct Index: Comparable {

    fileprivate var unitIndex: Int

    fileprivate var declIndex: Int

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.unitIndex == rhs.unitIndex
        ? lhs.declIndex < rhs.declIndex
        : lhs.unitIndex < rhs.unitIndex
    }

  }

}

extension ModuleDecl: IterableDeclSpace {

  typealias DeclSequence = ModuleDecl

  var decls: ModuleDecl { self }

}

extension ModuleDecl: TypeDecl {

  public var fullyQualName: [String] { [name] }

  public var range: SourceRange? { nil }

  public var isOverloadable: Bool { false }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

extension ModuleDecl: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(name)
  }

  public static func == (lhs: ModuleDecl, rhs: ModuleDecl) -> Bool {
    return lhs === rhs
  }

}

extension ModuleDecl: CustomStringConvertible {

  public var description: String {
    return "Module(\(name))"
  }

}
