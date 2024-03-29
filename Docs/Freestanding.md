# Free-standing mode

Most Hylo programs are expected to be executed in some hosted environment (i.e., with an operating system), that provides a handful of primitive operations to interact with that environment.
These operations roughly correspond to the [C standard library](https://en.cppreference.com/w/c/program).

Nonetheless, it is possible to compile programs for execution in a free-standing environment by passing `--freestanding` to the compiler:
Programs compiled that way must be linked with a library providing an implementation of essential operations necessary to implement the free-standing core of the standard library.
Only one function is currently required:

```hylo
/// Halts the execution of the program.
@external("halt")
fun halt() -> Never
```

A possible implementation of that function in C is:

```
void halt(void* r) {
  while (1) {}
}
```
