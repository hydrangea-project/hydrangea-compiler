const PREC = {
  lambda: -1,
  letIn: -1,
  conditional: -1,
  or: 1,
  and: 2,
  compare: 3,
  add: 4,
  multiply: 5,
  unary: 6,
  application: 7,
  postfix: 8,
  typeProduct: 1,
  typeArrow: 0,
};

module.exports = grammar({
  name: 'hydrangea',

  word: $ => $.identifier,

  externals: $ => [
    $.comment,
  ],

  extras: $ => [
    /\s/,
    $.comment,
  ],

  supertypes: $ => [
    $._expression,
    $._atom,
    $._pattern,
    $._type,
    $._refine_predicate,
    $._refine_term,
  ],

  rules: {
    source_file: $ => repeat($.declaration),

    identifier: _ => /[A-Za-z_][A-Za-z0-9_'?]*/,

    integer_literal: _ => /[0-9]+/,
    float_literal: _ => token(choice(
      /[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,
      /[0-9]+\.[0-9]+/
    )),
    string_literal: _ => token(seq('"', repeat(choice(/[^"\\]/, /\\./)), '"')),
    boolean_literal: _ => choice('true', 'false'),

    declaration: $ => seq(
      'let',
      field('name', $.identifier),
      repeat(field('parameter', $._pattern)),
      optional(field('precondition', $.where_clause)),
      optional(field('type', $.type_annotation)),
      '=',
      field('value', $._expression),
    ),

    where_clause: $ => seq('where', $._refine_predicate),

    type_annotation: $ => seq(':', $.polytype),

    polytype: $ => choice(
      seq('forall', repeat1(field('type_parameter', $.identifier)), '.', $._type),
      $._type,
    ),

    _type: $ => choice(
      $.primitive_type,
      $.type_identifier,
      $.unit_type,
      $.parenthesized_type,
      $.pair_type,
      $.record_type,
      $.product_type,
      $.function_type,
    ),

    primitive_type: _ => choice('int', 'bool', 'float'),
    type_identifier: $ => $.identifier,
    unit_type: _ => seq('(', ')'),
    parenthesized_type: $ => seq('(', $._type, ')'),
    pair_type: $ => seq('(', field('left', $._type), ',', field('right', $._type), ')'),
    record_type: $ => seq('{', commaSep1($.type_record_field), '}'),
    type_record_field: $ => seq(field('name', $.identifier), ':', field('type', $._type)),
    product_type: $ => prec.left(PREC.typeProduct, seq(field('left', $._type), '*', field('right', $._type))),
    function_type: $ => prec.right(PREC.typeArrow, seq(field('domain', $._type), '->', field('codomain', $._type))),

    _pattern: $ => choice(
      $.variable_pattern,
      $.vector_pattern,
      $.bound_pattern,
      $.pair_pattern,
    ),

    variable_pattern: $ => field('name', $.identifier),
    vector_pattern: $ => seq('[', commaSep1($._pattern), ']'),
    bound_pattern: $ => seq(field('name', $.identifier), 'bound', field('bound', $._atom)),
    pair_pattern: $ => seq('(', field('left', $._pattern), ',', field('right', $._pattern), ')'),

    _expression: $ => choice(
      $.lambda_expression,
      $.let_in_expression,
      $.bound_let_expression,
      $.conditional_expression,
      $.binary_expression,
      $.unary_expression,
      $.application_expression,
      $.special_expression,
      $._atom,
    ),

    lambda_expression: $ => prec.right(PREC.lambda, seq(
      'fn',
      repeat1(field('parameter', $._pattern)),
      '=>',
      field('body', $._expression),
    )),

    let_in_expression: $ => prec.right(PREC.letIn, seq(
      field('binding', $.declaration),
      'in',
      field('body', $._expression),
    )),

    bound_let_expression: $ => prec.right(PREC.letIn, seq(
      'let',
      field('name', $.identifier),
      'bound',
      field('bound', $._atom),
      '=',
      field('value', $._expression),
      'in',
      field('body', $._expression),
    )),

    conditional_expression: $ => prec.right(PREC.conditional, seq(
      'if',
      field('condition', $._expression),
      'then',
      field('consequence', $._expression),
      optional(seq(
        'else',
        field('alternative', $._expression),
      )),
    )),

    unary_expression: $ => prec.right(PREC.unary, seq(
      '-',
      field('argument', $._expression),
    )),

    application_expression: $ => prec.left(PREC.application, seq(
      field('function', $._atom),
      field('argument', $._atom),
      repeat(field('argument', $._atom)),
    )),

    binary_expression: $ => choice(
      prec.left(PREC.or, seq(field('left', $._expression), field('operator', '|'), field('right', $._expression))),
      prec.left(PREC.and, seq(field('left', $._expression), field('operator', '&'), field('right', $._expression))),
      prec.left(PREC.compare, seq(field('left', $._expression), field('operator', choice('=', '<>', '<', '<=', '>', '>=', '=.', '<>.', '<.', '<=.', '>.', '>=.')), field('right', $._expression))),
      prec.left(PREC.add, seq(field('left', $._expression), field('operator', choice('+', '-', '+.', '-.')), field('right', $._expression))),
      prec.left(PREC.multiply, seq(field('left', $._expression), field('operator', choice('*', '/', '%', '*.', '/.')), field('right', $._expression))),
    ),

    special_expression: $ => choice(
      $.generate_expression,
      $.fill_expression,
      $.replicate_expression,
      $.slice_expression,
      $.reshape_expression,
      $.map_expression,
      $.zipwith_expression,
      $.reduce_expression,
      $.reduce_generate_expression,
      $.foldl_expression,
      $.foldl_while_expression,
      $.scan_expression,
      $.scan_inclusive_expression,
      $.scanr_expression,
      $.scanr_inclusive_expression,
      $.segmented_reduce_expression,
      $.sort_indices_expression,
      $.iota_expression,
      $.make_index_expression,
      $.coo_sum_duplicates_expression,
      $.csr_from_sorted_coo_expression,
      $.permute_expression,
      $.scatter_expression,
      $.scatter_guarded_expression,
      $.gather_expression,
      $.unary_special_expression,
      $.index_expression,
      $.shape_of_expression,
      $.check_index_expression,
      $.read_array_expression,
      $.read_array_float_expression,
      $.write_array_expression,
      $.write_array_float_expression,
      $.get_env_int_expression,
      $.get_env_string_expression,
      $.stencil_expression,
    ),

    generate_expression: $ => seq('generate', field('shape', $._atom), field('generator', $._atom)),
    fill_expression: $ => seq('fill', field('shape', $._atom), field('value', $._atom)),
    replicate_expression: $ => seq('replicate', field('shape', $.shape_spec), field('source', $._atom)),
    slice_expression: $ => seq('slice', field('slice', $.slice_spec), field('source', $._atom)),
    reshape_expression: $ => seq('reshape', field('shape', $._atom), field('source', $._atom)),
    map_expression: $ => seq('map', field('function', $._atom), field('source', $._atom)),
    zipwith_expression: $ => seq('zipwith', field('function', $._atom), field('left', $._atom), field('right', $._atom)),
    reduce_expression: $ => seq('reduce', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    reduce_generate_expression: $ => seq('reduce_generate', field('function', $._atom), field('initial', $._atom), field('shape', $._atom), field('generator', $._atom)),
    foldl_expression: $ => seq('foldl', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    foldl_while_expression: $ => seq('foldl_while', field('predicate', $._atom), field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    scan_expression: $ => seq('scan', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    scan_inclusive_expression: $ => seq('scan_inclusive', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    scanr_expression: $ => seq('scanr', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    scanr_inclusive_expression: $ => seq('scanr_inclusive', field('function', $._atom), field('initial', $._atom), field('source', $._atom)),
    segmented_reduce_expression: $ => seq('segmented_reduce', field('function', $._atom), field('initial', $._atom), field('offsets', $._atom), field('values', $._atom)),
    sort_indices_expression: $ => seq('sort_indices', field('source', $._atom)),
    iota_expression: $ => seq('iota', field('size', $._atom)),
    make_index_expression: $ => seq('make_index', field('bound', $._atom), field('source', $._atom)),
    coo_sum_duplicates_expression: $ => seq('coo_sum_duplicates', field('nrows', $._atom), field('ncols', $._atom), field('nnz', $._atom), field('rows', $._atom), field('cols', $._atom), field('values', $._atom)),
    csr_from_sorted_coo_expression: $ => seq('csr_from_sorted_coo', field('nrows', $._atom), field('ncols', $._atom), field('nnz', $._atom), field('rows', $._atom), field('cols', $._atom), field('values', $._atom)),
    permute_expression: $ => seq('permute', field('combine', $._atom), field('defaults', $._atom), field('indices', $._atom), field('source', $._atom)),
    scatter_expression: $ => seq('scatter', field('combine', $._atom), field('defaults', $._atom), field('indices', $._atom), field('values', $._atom)),
    scatter_guarded_expression: $ => seq('scatter_guarded', field('combine', $._atom), field('defaults', $._atom), field('indices', $._atom), field('values', $._atom), field('guard', $._atom)),
    gather_expression: $ => seq('gather', field('indices', $._atom), field('source', $._atom)),
    unary_special_expression: $ => seq(field('operator', choice('sqrt', 'fst', 'snd', 'expf', 'log', 'sin', 'cos', 'abs_f', 'floor_f', 'ceil_f', 'erf', 'float_of', 'int_of_float')), field('argument', $._atom)),
    index_expression: $ => seq('index', field('index', $._atom), field('source', $._atom)),
    shape_of_expression: $ => seq('shape_of', field('source', $._atom)),
    check_index_expression: $ => seq('check_index', field('index', $._atom), field('default', $._atom), field('source', $._atom)),
    read_array_expression: $ => seq('read_array', field('shape', $._atom), field('path', $._atom)),
    read_array_float_expression: $ => seq('read_array_float', field('shape', $._atom), field('path', $._atom)),
    write_array_expression: $ => seq('write_array', field('source', $._atom), field('path', $._atom)),
    write_array_float_expression: $ => seq('write_array_float', field('source', $._atom), field('path', $._atom)),
    get_env_int_expression: $ => seq('get_env_int', field('name', $._atom)),
    get_env_string_expression: $ => seq('get_env_string', field('name', $._atom)),
    stencil_expression: $ => seq('stencil', field('boundary', $.boundary_condition), field('accessor', $._atom), field('source', $._atom)),

    boundary_condition: $ => choice(
      'clamp',
      'wrap',
      'mirror',
      seq('(', 'constant', field('value', $._atom), ')'),
    ),

    shape_spec: $ => seq('[', commaSep1($.shape_dimension), ']'),
    shape_dimension: $ => choice(
      $.shape_dimension_all,
      $.shape_dimension_any,
      $.shape_dimension_expression,
    ),
    shape_dimension_all: _ => 'All',
    shape_dimension_any: $ => seq('Any', field('value', $._expression)),
    shape_dimension_expression: $ => $._expression,

    slice_spec: $ => seq('[', commaSep1($.slice_dimension), ']'),
    slice_dimension: $ => choice(
      $.slice_dimension_all,
      $.slice_dimension_range,
    ),
    slice_dimension_all: _ => 'All',
    slice_dimension_range: $ => seq('[', field('start', $._expression), ',', field('length', $._expression), ']'),

    _atom: $ => choice(
      $.postfix_expression,
      $.projection_expression,
      $.vector_expression,
      $.record_expression,
      $.unit_expression,
      $.pair_expression,
      $.parenthesized_expression,
      $.first_class_operator,
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.boolean_literal,
      $.identifier,
    ),

    postfix_expression: $ => prec.left(PREC.postfix, seq(
      field('target', choice(
        $.vector_expression,
        $.record_expression,
        $.unit_expression,
        $.pair_expression,
        $.parenthesized_expression,
        $.first_class_operator,
        $.integer_literal,
        $.float_literal,
        $.string_literal,
        $.boolean_literal,
        $.identifier,
      )),
      repeat1(seq('.', field('field', $.identifier))),
    )),

    projection_expression: $ => seq('proj', field('index', $.integer_literal), field('target', $._atom)),
    vector_expression: $ => seq('[', commaSep1($._expression), ']'),
    record_expression: $ => seq('{', commaSep1($.record_field), '}'),
    record_field: $ => seq(field('name', $.identifier), '=', field('value', $._expression)),
    unit_expression: _ => seq('(', ')'),
    pair_expression: $ => seq('(', field('left', $._expression), ',', field('right', $._expression), ')'),
    parenthesized_expression: $ => seq('(', $._expression, ')'),

    first_class_operator: _ => choice(
      seq('(', '+', ')'),
      seq('(', '-', ')'),
      seq('(', '*', ')'),
      seq('(', '/', ')'),
      seq('(', '=', ')'),
      seq('(', '<>', ')'),
      seq('(', '<', ')'),
      seq('(', '<=', ')'),
      seq('(', '>', ')'),
      seq('(', '>=', ')'),
      seq('(', '&', ')'),
      seq('(', '|', ')'),
      seq('(', '+.', ')'),
      seq('(', '-.', ')'),
      seq('(', '*.', ')'),
      seq('(', '/.', ')'),
      seq('(', '=.', ')'),
      seq('(', '<>.', ')'),
      seq('(', '<.', ')'),
      seq('(', '<=.', ')'),
      seq('(', '>.', ')'),
      seq('(', '>=.', ')'),
    ),

    _refine_predicate: $ => choice(
      $.bound_refine_predicate,
      $.comparison_refine_predicate,
      $.conjunctive_refine_predicate,
      $.parenthesized_refine_predicate,
    ),

    bound_refine_predicate: $ => seq('bound', field('name', $.identifier), field('value', $._refine_term)),
    comparison_refine_predicate: $ => seq(field('left', $._refine_term), field('operator', choice('<', '<=', '>', '>=', '=', '<>')), field('right', $._refine_term)),
    conjunctive_refine_predicate: $ => prec.left(PREC.and, seq(field('left', $._refine_predicate), '&', field('right', $._refine_predicate))),
    parenthesized_refine_predicate: $ => seq('(', $._refine_predicate, ')'),

    _refine_term: $ => choice(
      $.refine_variable,
      $.refine_integer,
      $.refine_dim,
      $.refine_elem,
      $.refine_add,
      $.refine_subtract,
      $.refine_scale,
      $.parenthesized_refine_term,
    ),

    refine_variable: $ => $.identifier,
    refine_integer: $ => $.integer_literal,
    refine_dim: $ => seq('dim', field('name', $.identifier), field('index', $.integer_literal)),
    refine_elem: $ => seq('elem', field('name', $.identifier)),
    refine_add: $ => prec.left(PREC.add, seq(field('left', $._refine_term), '+', field('right', $._refine_term))),
    refine_subtract: $ => prec.left(PREC.add, seq(field('left', $._refine_term), '-', field('right', $._refine_term))),
    refine_scale: $ => prec.left(PREC.multiply, seq(field('scale', $.integer_literal), '*', field('value', $._refine_term))),
    parenthesized_refine_term: $ => seq('(', $._refine_term, ')'),
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}
