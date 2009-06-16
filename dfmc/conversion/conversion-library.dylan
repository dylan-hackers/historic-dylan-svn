module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-conversion
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-flow-graph;
  export dfmc-conversion;
end library;

define module dfmc-conversion
  use functional-dylan;
  use generic-arithmetic,
    prefix: "generic/";
  use transcendentals;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-flow-graph;
  export 
    $ignore, $all-rest,
    convert,
    convert-type-expression,
    convert-value-reference,
    convert-object-reference,
    convert-object-reference-1,
    convert-method-reference,
    convert-values,
    convert-global-reference,
    convert-dylan-reference,
    make-global-reference,
    convert-error-call,
    convert-1,
    convert-top-level-initializer;

  export // utilities
    make-with-temporary*,
    extractable-constant-value?,
    extract-constant,
    <ignore-value-context>, 
    <multiple-value-context>,
    match-values-with-context,
    bind-local-variable,
    do-convert;

  export
    ^function-key-type*,
    ^function-value-type*,
    ^function-required-type*,
    ^function-rest-value-type*;

  export
    &top-level-eval,
    ^top-level-eval,
    ^top-level-eval-sequence,
    ^top-level-eval-type;

  export
    check-model,
    <heap-deferred-all-classes-model>,
    finish-class-models,
    finish-generic-function-models;

  export // generic functions
    ^generic-function-explicitly-defined-methods,
    ^generic-function-explicitly-defined-domains;

  export // methods
    method-inlineable?,
    maybe-compute-and-install-method-dfm,
    compute-and-install-method-dfm,
    retract-method-dfm,
    ensure-optimized-method-model,
    ensure-method-model,
    ensure-method-dfm,
    ensure-method-dfm-or-heap,
    empty-method?;

  export // classes
    ^ensure-class-complete,
    do-instance-slot-values, \for-instance-slot-value;

  export
    \for-layout-fixed-slot-value, do-layout-fixed-slot-values,
    \for-layout-repeated-slot-value, do-layout-repeated-slot-values,
    fixed-slot-primitive-fixup-info,
    repeated-slot-primitive-fixup-info;
  export
    define-compiler-metaclass;

  export
    trace-macro, untrace-macro;

  export
    copy-down-body;

  export
    *strip-enabled?*;

end module;

// eof
