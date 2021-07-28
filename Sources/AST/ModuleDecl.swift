import Basic

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

  public init(name: String, generation: Int, context: Context) {
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
  public func extensions(of decl: GenericTypeDecl) -> [TypeExtDecl] {
    var dealiasedDecl = decl
    while let alias = (dealiasedDecl as? AliasTypeDecl)?.aliasedDecl {
      dealiasedDecl = alias
    }

    var matches: [TypeExtDecl] = []

    // Loop through all extensions in the module, (partially) binding them if necessary.
    stmt:for case let ext as TypeExtDecl in decls {
      // Skip invalid declarations.
      guard ext.state != .invalid else { continue }

      // The extension is already bound, so we can check if it's bound to `decl` directly.
      if ext.state >= .realized {
        if dealiasedDecl === ext.extendedDecl {
          matches.append(ext)
        }
        continue stmt
      }

      // The extension isn't bound yet, so we need to realize to resolve its identifier.
      if ext.extendedIdent is UnqualIdentSign {
        if ext.extendedDecl === dealiasedDecl {
          matches.append(ext)
        }
        continue stmt
      }

      // If the identifier is a `CompoundIdentSign`, we can't realize the full signature at once,
      // as we may risk to trigger infinite recursion of name lookups if the signature points
      // within `decl`. Instead, we must realize it lazily and abort if we detect that we're
      // about to start a lookup from `decl`.
      let compound = ext.extendedIdent as! CompoundIdentSign
      let baseType = compound.components[0].realize(unqualifiedFrom: self)
      guard var baseDecl = (baseType as? NominalType)?.decl else { continue stmt }

      for i in 1 ..< compound.components.count {
        // The signature points within `decl`; we have to give up.
        guard baseDecl !== dealiasedDecl else { continue stmt }

        // Realize the next component.
        let nextType = compound.components[i].realize(qualifiedIn: baseDecl, from: self)
        guard let nextDecl = (nextType as? NominalType)?.decl else { continue stmt }
        baseDecl = nextDecl
      }

      // Check if the declaration to which the signature resolves is `decl`.
      if baseDecl === dealiasedDecl {
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

  public var range: SourceRange { .invalid }

  public var isOverloadable: Bool { false }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
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
