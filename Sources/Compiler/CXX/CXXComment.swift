
/// A C++ comment
struct CXXComment : CXXRepresentable {

    public let comment: String

    func writeCode<Target: TextOutputStream>(into target: inout Target) {
        if comment.contains("\n") {
            target.write("/* \(comment) */")
        } else {
            target.write("// \(comment)\n")
        }
    }

}