numeric_types = [
    # type, buliltin, rmw_ops
    # ("Int", "word", ["add", "sub", "max", "min", "and", "nand", "or", "xor"]),
    ("Int8", "i8", ["add", "sub", "max", "min", "and", "nand", "or", "xor"]),
    ("Int32", "i32", ["add", "sub", "max", "min", "and", "nand", "or", "xor"]),
    ("Int64", "i64", ["add", "sub", "max", "min", "and", "nand", "or", "xor"]),
    # ("UInt", "word", ["add", "sub", "umax", "umin", "and", "nand", "or", "xor"]),
    ("UInt8", "i8", ["add", "sub", "umax", "umin", "and", "nand", "or", "xor"]),
    ("UInt32", "i32", ["add", "sub", "umax", "umin", "and", "nand", "or", "xor"]),
    ("UInt64", "i64", ["add", "sub", "umax", "umin", "and", "nand", "or", "xor"]),
]

ops_to_hylo = {
    "add": "fetch_add",
    "fadd": "fetch_add",
    "sub": "fetch_sub",
    "fsub": "fetch_sub",
    "max": "fetch_max",
    "umax": "fetch_max",
    "fmax": "fetch_max",
    "min": "fetch_min",
    "umin": "fetch_min",
    "fmin": "fetch_min",
    "and": "fetch_and",
    "nand": "fetch_nand",
    "or": "fetch_or",
    "xor": "fetch_xor",
}

load_orderings = [
    # hylo_ordering, native_ordering
    ("relaxed", "relaxed", 0),
    ("acquiring", "acquire", 1),
    ("sequentially_consistent", "seqcst", 4),
]

store_orderings = [
    # hylo_ordering, native_ordering
    ("relaxed", "relaxed", 0),
    ("releasing", "release", 2),
    ("sequentially_consistent", "seqcst", 4),
]

update_orderings = [
    # hylo_ordering, native_ordering, value
    ("relaxed", "relaxed", 0),
    ("acquiring", "acquire", 1),
    ("releasing", "release", 2),
    ("acquiring_and_releasing", "acqrel", 3),
    ("sequentially_consistent", "seqcst", 4),
]
