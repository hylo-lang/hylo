#  C++ transpilation

## Implementation notes

A Val module is transpiled as a C++ compilation unit, composed of one header and one source file.
All transpiled symbols are defined in the module namespace.
The header of a transpiled unit exposes the public symbols of the module through a C++ API.

Stack-allocated objects are declared as arrays of chars, so that C++ doesn't run ctors/dtors.
For example, `%n = alloc [stack] T` is translated as follows:

```
alignas(T) char _ln[sizeof(T)];
T* ln = reinterpret_cast<T*>(_ln);
``` 

