# Miscellaneous notes

## Constrained accessors

```
subscript identity<T>(x: T): T {
  let { yield x }
  sink where T:Sinkable { x } // <======= HERE
  inout { yield x }
  set { x = newValue }
}
```
