(comment) @comment

[
  "let"
  "in"
  "if"
  "then"
  "else"
  "fn"
  "forall"
  "where"
  "bound"
  "dim"
  "elem"
  "All"
  "Any"
  "clamp"
  "wrap"
  "mirror"
  "constant"
  "generate"
  "fill"
  "replicate"
  "slice"
  "reshape"
  "map"
  "zipwith"
  "reduce"
  "reduce_generate"
  "foldl"
  "foldl_while"
  "scan"
  "scan_inclusive"
  "scanr"
  "scanr_inclusive"
  "segmented_reduce"
  "sort_indices"
  "iota"
  "make_index"
  "coo_sum_duplicates"
  "csr_from_sorted_coo"
  "permute"
  "scatter"
  "scatter_guarded"
  "gather"
  "sqrt"
  "fst"
  "snd"
  "expf"
  "log"
  "sin"
  "cos"
  "abs_f"
  "floor_f"
  "ceil_f"
  "erf"
  "float_of"
  "int_of_float"
  "index"
  "shape_of"
  "check_index"
  "read_array"
  "read_array_float"
  "write_array"
  "write_array_float"
  "get_env_int"
  "get_env_string"
  "stencil"
] @keyword

[
  "int"
  "bool"
  "float"
] @type.builtin

[
  "true"
  "false"
] @boolean

[
  "+"
  "-"
  "*"
  "/"
  "%"
  "+."
  "-."
  "*."
  "/."
  "="
  "<>"
  "<"
  "<="
  ">"
  ">="
  "=."
  "<>."
  "<."
  "<=."
  ">."
  ">=."
  "=>"
  "->"
] @operator

(integer_literal) @number
(float_literal) @number.float
(string_literal) @string

(declaration
  name: (identifier) @function)

(record_field
  name: (identifier) @property)

(type_record_field
  name: (identifier) @property)

(postfix_expression
  field: (identifier) @property)

(variable_pattern
  name: (identifier) @variable.parameter)

(bound_pattern
  name: (identifier) @variable.parameter)

(polytype
  type_parameter: (identifier) @type.parameter)

