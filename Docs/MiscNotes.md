# Miscellaneous notes

## Constrained accessors

```
subscript identity<T>(x: T): T {
  let { yield x }
  sink where T:Movable { x } // <======= HERE
  inout { yield x }
  set { x = newValue }
}
```

## Optimizations

Transforming stack allocations of metatypes to global references.

```
%i0.0#0: &Metatype<Int> = alloc_stack Metatype<Int>
store Int, %i0.0#0
%i0.2#0: &Metatype<T> = borrow [let] %i0.0#0
```

Would become

```
%i0.0#0: &Metatype<Int> = global_addr Int.metatype
%i0.2#0: &Metatype<T> = borrow [let] %i0.0#0
```
