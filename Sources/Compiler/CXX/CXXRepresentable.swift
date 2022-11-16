/// A C++ code fragment that can be translated in one place.
public protocol CXXRepresentable {
    func writeCode<Target: TextOutputStream>(into target: inout Target)
}
