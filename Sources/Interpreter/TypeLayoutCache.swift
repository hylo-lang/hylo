import IR
import FrontEnd
import Utils

/// A memoizing computer of type layouts.
public struct TypeLayoutCache {

  /// The program supplying the types.
  let p: TypedProgram

  /// The ABI for which the types will be laid out.
  let abi: any TargetABI

  /// The memo of layouts computed so far.
  private var storage: [AnyType: TypeLayout] = [:]

  /// An instance for laying out types in `p` according to `abi`.
  public init(typesIn p: TypedProgram, for abi: any TargetABI) {
    self.p = p
    self.abi = abi
  }

  /// The layout for `t`.
  public subscript(_ t: AnyType) -> TypeLayout {
    mutating get {
      if let r = storage[t] { return r }
      let r = computeLayout(t)
      storage[t] = r
      return r
    }
  }

  /// Returns the layout for `u`.
  private mutating func computeLayout(_ u: UnionType) -> TypeLayout {
    let basis = u.elements.map { self[$0] }

    let discriminator = abi.unionDiscriminator(count: basis.count)
    let discriminatorLayout = self[discriminator]

    let payloadBytes = TypeLayout.Bytes(
      alignment: basis.lazy.map(\.alignment).max()!,
      size: basis.lazy.map(\.size).max()!)

    let payloadFirst = payloadBytes.appending(discriminatorLayout.bytes)
    let discriminatorFirst = discriminatorLayout.bytes.appending(payloadBytes)

    let payloadOffset: Int
    let discriminatorOffset: Int
    let l: TypeLayout.Bytes

    if payloadFirst.size < discriminatorFirst.size {
      l = payloadFirst
      payloadOffset = 0
      discriminatorOffset = l.size - discriminatorLayout.size
    } else {
      l = discriminatorFirst
      payloadOffset = l.size - payloadBytes.size
      discriminatorOffset = 0
    }

    return TypeLayout.init(
      bytes: l,
      type: ^u,
      parts:
        basis.map { .init(name: String(describing: $0.type), type: $0.type, offset: payloadOffset) }
        + [.init(name: "discriminator", type: discriminator, offset: discriminatorOffset)],
      isUnionLayout: true)
  }

  /// Returns the concrete layout for `l`
  private mutating func concretized(_ l: AbstractTypeLayout) -> TypeLayout {
    if l.properties.isEmpty {
      return TypeLayout(
        bytes: .init(alignment: 1, size: 0), type: l.type,
        parts: [], isUnionLayout: false
      )
    }
    let f = l.properties.first!
    var b = self[f.type].bytes
    var parts: [TypeLayout.Part] = [.init(name: f.label ?? "0", type: f.type, offset: 0)]
    for (i, p) in l.properties.dropFirst().enumerated() {
      let c = self[p.type].bytes
      b = b.appending(c)
      parts.append(
        .init(name: p.label ?? String(describing: i + 1), type: p.type, offset: b.size - c.size))
    }
    return TypeLayout(bytes: b, type: l.type, parts: parts, isUnionLayout: false)
  }

  /// Returns the layout for `t`.
  private mutating func computeLayout(_ t: AnyType) -> TypeLayout {
    switch t.base {
    case let u as UnionType:
      computeLayout(u)
    case let u as BuiltinType:
      TypeLayout(bytes: abi.layout(u), type: t, parts: [], isUnionLayout: false)
    case _ where t.hasRecordLayout:
      concretized(AbstractTypeLayout(of: t, definedIn: p))
    default:
      fatalError()
    }
  }
}
