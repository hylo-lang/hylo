# Metatype lowering

This document describes how metatypes should be lowered to LLVM.

## Goals

Metatypes are used to query information about the type of a particular value at runtime.
For example, in the program below, a metatype is used to obtain the size of a type in memory:

```swift
public fun main() {
  print(Int.layout.size)
}
```

> Why not simply `Int.size`?
> Using `layout` as a prefix for all layout-related properties reduces the pollution of a type's static namespace.

Here is the information we should be able to get from a metatype:
- Size and alignment
- Textual representation (e.g., name)
- Kind (e.g., product type, lambda type, etc.)
- Generic arguments (if any)
- Name, types, and offsets of stored properties

### About reflective programming

Though metatypes will probably be essential to implement countless exciting reflective programming use cases (e.g., ORMs), I think we should not get too distracted at this point.
Clearly the most important information we need to access at this point is size and alignment, which is necessary to allocate storage beyond resilience boundaries.
Additional information can be used to support debuggability (e.g., print any value), which I think is also very valuable at this stage.

For other, more advanced use cases, my sense is that if we store the additional information mentioned above, we'll likely be able to build a robust reflective API in the future by exposing the runtime functions that will be part of Val's implementation.

## Detailed design

Concretely, a metatype is just a pointer to some static data generated during compilation.
This, the question is: what is the representation of this generated data?

An arbitrary metatype is represented by the following structure:
```c++
struct val_metatype {

  /// The contiguous memory footprint of the type, in bytes.
  int64_t size;

  /// The preferred memory alignment of the type, in bytes.
  int64_t alignment;

  /// The representation of the type.
  intptr_t representation;

};
```
`size` and `alignment` are computed statically when the metatype value is generated.

`representation` is used to encode other reflective information.
Because every different types can have widely different representations, this property denotes a variant encoded as a tagged pointer to the actual representation.

**Product types:**

```c++
struct val_stored_property {

  /// The name of the property, null-terminated.
  const char* name;

  /// The type of the property.
  val_metatype* type;

};

struct val_product_type_mirror {

  /// The mangled name of the type, null-terminated.
  const char* name;

  /// The number of stored properties in the type.
  int64_t property_count;

  /// The type's stored properties.
  val_stored_property* properties;

};
```

**Lambda types:**

```c++
struct val_lambda_type_mirror {

  /// The number of parameters in the type.
  int64_t parameter_count;

  /// The argument labels of the lamparameters.
  const char** labels;

  /// The types of the parameters, return value, and environment.
  val_metatype* types;

};
```

### Metatypes of unparametrized generic types

Unparameterized generic types are not first class values and thus do not require a lowered metatype representation.
However, a special "unparameterized" metatype is still needed to represent the base of a bound generic type.

Such a metatype is generated for all generic product type declarations, with unspecified size and alignment.

## Related work

- [The Future of Reflective Programming in Swift](https://forums.swift.org/t/the-future-of-reflective-programming-in-swift/54956)