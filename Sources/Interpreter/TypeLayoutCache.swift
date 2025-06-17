import IR
import FrontEnd
import Utils

struct TypeLayoutCache {

  let p: TypedProgram
  let abi: any TargetABI

  private typealias Storage = [AnyType: TypeLayout]
  private var storage: Storage

  subscript(_ t: AnyType) -> TypeLayout {
    mutating get {
      if let r = storage[t] { return r }
      let r = computeLayout(t)
      storage[t] = r
      return r
    }
  }

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
      }
      else {
        l = discriminatorFirst
        payloadOffset = l.size - payloadBytes.size
        discriminatorOffset = 0
      }

      return TypeLayout.init(
        bytes: l,
        type: ^u,
        components:
          basis.map { (name: String(describing: $0.type), type: $0.type, offset: payloadOffset) }
          + [ (name: "discriminator",  type: discriminator, offset: discriminatorOffset) ],
        isUnionLayout: true)
  }

  private mutating func concretized(_ l: AbstractTypeLayout) -> TypeLayout {
    let f = l.properties.first!
    var b = self[f.type].bytes
    var components: [TypeLayout.Component] = [(name: f.label ?? "0", type: f.type, offset: 0)]
    for (i, p) in l.properties.enumerated() {
      let c = self[p.type].bytes
      b = b.appending(c)
      components.append((name: f.label ?? String(describing: i), type: p.type, offset: b.size - c.size))
    }
    return TypeLayout(bytes: b, type: l.type, components: components, isUnionLayout: false)
  }

  private mutating func computeLayout(_ t: AnyType) -> TypeLayout {
    switch t.base {
    case let u as UnionType:
      computeLayout(u)
    case let u as BuiltinType:
      TypeLayout(bytes: abi.layout(u), type: t, components: [], isUnionLayout: false)
    case _ where t.hasRecordLayout:
      concretized(AbstractTypeLayout(of: t, definedIn: p))
    default:
      fatalError()
    }
  }
}
