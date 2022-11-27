
/// Cast the result of an expression to void
struct CXXVoidCast : CXXRepresentable {

    public let baseExpr: CXXRepresentable

    func writeCode<Target: TextOutputStream>(into target: inout Target) {
        target.write("(void) ")
        baseExpr.writeCode(into: &target)
    }

}