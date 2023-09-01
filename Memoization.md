# Memoization

We want to address a pattern that we see in the Val compiler's source, and we want to do it with
pure value semantics.

The job of a compiler is, effectively, to build successive representations of a program that starts
out in textual form by repeatedly transforming it; e.g. Text -> AST -> TypeChecked AST -> Val IR ->
LLVM IR -> machine code. Each transformation may be able to take advantage of any of the earlier
representations, so in our codebase, each new representation contains (parts of) its predecessor.
Each transformation can be thought of as a pure function that takes an input representation and
generates an output representation containing additional data structures representing the results of
some analysis. These functions are implemented by starting with empty additional data and repeatedly
mutating it until the new representation can be formed from its predecessor and the additional data.

Each transformation step may use temporary state that is discarded when the
transformation is complete.

Each step in the chain can be thought of as a
pure function of an input representation.


typically need to store relationships to (parts of) earlier representations.  To keep these
relationships valid, the In a value semantic context we usually keep these relationships

Each representation tends to reference earlier representations, not least so that errors detected
