import AST
import Basic

/// The representation of runtime value.
public protocol Value {

  /// The type of the value.
  var type: VILType { get }

}

/// A pair representing the use of a value in an instruction.
public struct Use: Hashable {

  /// The index of the user that contains this use.
  public let user: InstIndex

  /// The index of this use in `user`'s operands.
  public let index: Int

}

// MARK: Constants

/// A constant value.
public protocol Constant: Value, CustomStringConvertible {

  func isEqual(to other: Constant) -> Bool

  func hash(into hasher: inout Hasher)

}

extension Constant where Self: Equatable {

  public func isEqual(to other: Constant) -> Bool {
    return self == (other as? Self)
  }

}

/// A poison value.
public struct PoisonValue: Constant, Hashable {

  public let type: VILType

  init(type: VILType) {
    self.type = type
  }

  public var description: String { "poison" }

}

/// A constant "unit" value.
public struct UnitValue: Constant, Hashable {

  public let type: VILType

  init(context: Context) {
    self.type = .lower(context.unitType)
  }

  public var description: String { "unit" }

}

/// A constant integer value.
public struct IntValue: Constant {

  public let type: VILType

  /// The constant's value.
  ///
  /// The value is encoded with the `bitWidth` least significant bits of that property.
  public let bitPattern: Int64

  /// The number of bits in the binary representation of values of this type.
  public let bitWidth: Int

  init(bitPattern: Int64, bitWidth: Int, context: Context) {
    self.bitPattern = bitPattern
    self.bitWidth = bitWidth
    self.type = .lower(context.getBuiltinType(named: "i\(bitWidth)")!)
  }

  public var description: String {
    return String(describing: bitPattern)
  }

  /// Creates a built-in `false` constant (i.e., `0` with the type `i1`).
  public static func makeFalse(context: Context) -> IntValue {
    return IntValue(bitPattern: 0, bitWidth: 1, context: context)
  }

  /// Creates a built-in `true` constant (i.e., `1` with the type `i1`).
  static func makeTrue(context: Context) -> IntValue {
    return IntValue(bitPattern: 1, bitWidth: 1, context: context)
  }

}

extension IntValue: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(bitPattern)
    hasher.combine(bitWidth)
  }

  public static func == (lhs: IntValue, rhs: IntValue) -> Bool {
    return (lhs.bitPattern == rhs.bitPattern) && (lhs.bitWidth == rhs.bitWidth)
  }

}

/// An integer literal.
public struct IntLiteralValue: Constant {

  public let type: VILType

  /// The literal's value.
  public let value: Int

  init(value: Int, context: Context) {
    self.value = value
    self.type = .lower(context.getBuiltinType(named: "IntLiteral")!)
  }

  public var description: String {
    return String(describing: value)
  }

}

extension IntLiteralValue: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  public static func == (lhs: IntLiteralValue, rhs: IntLiteralValue) -> Bool {
    return lhs.value == rhs.value
  }

}

/// A reference to a built-in function.
public struct BuiltinFunRef: Constant {

  public let type: VILType

  /// The built-in function declaration that is being referred.
  public let decl: FunDecl

  init(decl: FunDecl) {
    precondition(decl.isBuiltin)
    self.decl = decl
    self.type = .lower(decl.type)
  }

  public var description: String {
    return "b\"\(decl.name)\""
  }

}

extension BuiltinFunRef: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public static func == (lhs: BuiltinFunRef, rhs: BuiltinFunRef) -> Bool {
    return lhs.decl === rhs.decl
  }

}

/// A reference to a VIL function.
public struct FunRef: Constant {

  public let type: VILType

  /// The name of the function being referenced.
  public let name: String

  init(function: VILFun) {
    self.name = function.name
    self.type = function.type.address
  }

  init(funName: String, type: VILType) {
    self.name = funName
    self.type = type.address
  }

  public var description: String {
    return "@\(name)"
  }

}

extension FunRef: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(name)
  }

  public static func == (lhs: FunRef, rhs: FunRef) -> Bool {
    return lhs.name == rhs.name
  }

}

// MARK: Arguments

/// The formal argument (a.k.a. parameter) of a block or function.
public final class ArgValue: Value, Identifiable {

  public typealias ID = ObjectIdentifier

  public let type: VILType

  public init(type: VILType) {
    self.type = type
  }

  public var id: ObjectIdentifier { ObjectIdentifier(self) }

}

extension ArgValue: Equatable, Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

  public static func == (lhs: ArgValue, rhs: ArgValue) -> Bool {
    return lhs === rhs
  }

}
