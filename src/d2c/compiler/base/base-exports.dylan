module: dylan-user
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/base-exports.dylan,v 1.31 1996/03/20 01:44:03 rgs Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

define library compiler-base
  use Dylan;
  use Collection-Extensions,
    import: {self-organizing-list}, export: all;
  use Random;
  use Streams, export: all;
  use Print, export: all;
  use Format, export: all;
#if (mindy)
  use Debugger-Format;
  use String-extensions, export: all;
#end
  
  export c-representation;
  export classes;
  export common;
  export compile-time-functions;
  export compile-time-values;
  export ctype;
  export definitions;
  export errors;
  export flow;
  export forwards;
  export header;
  export names;
  export od-format;
  export policy;
  export representation;
  export signature-interface;
  export source;
  export tokens;
  export transformers;
  export utils;
  export variables;

end;

define module params
  use Dylan;
  use Extensions, import: {<extended-integer>};

  export $minimum-integer, $maximum-integer;
end;

define module common
  use Dylan,
    exclude: {direct-superclasses, direct-subclasses},
    export: all;
  use Extensions,
    import: {<general-integer>, <extended-integer>,
	     $maximum-integer, ratio,
	     false-or, one-of, <false>, <true>, ignore,
	     $minimum-integer, <byte-character>, $not-supplied,
#if (mindy)
             *debug-output*, main, key-exists?,
#else
             <ratio>, numerator, denominator,
#end
	     <equal-table>, <string-table>, equal-hash},
    export: all;
  use Streams, export: all;
  use Print, export: all;
  use PPrint, export: all;
  use Format, export: all;
  use Params, prefix: "runtime-", export: all;
#if (~mindy)
  create
     *debug-output*;
#end
end;

define module utils
  use common;
  use standard-io;
#if (mindy)
  use Introspection, import: {object-address, class-name};
  use System, import: {copy-bytes};
#else
  use Introspection, import: {class-name};
  use System, import: {object-address, copy-bytes};
#end

  // Stuff defined in utils
  export
    write-class-name, write-address, pprint-fields,
    find-in, size-in,
    dformat, assert,
    <annotatable>, info, info-setter,
    key-of, list?, pair?,
    symcat, stringify,
    log-target, log-dependency, spew-dependency-log;
end;

define module od-format
  use common;
#if (mindy)
  use system, import: {get-time-of-day};
#else
  use system, import: {call-out};
#end
  use standard-io;
  use introspection, import: {function-name};
  use utils;
  use self-organizing-list;
  export
    $odf-header-flag,
    $odf-etype-mask,
    $odf-object-definition-etype,
    $odf-end-entry-etype,
    $odf-local-reference-etype,
    $odf-external-reference-etype,
    $odf-subobjects-flag,
    $odf-raw-format-mask,
    $odf-no-raw-data-format,
    $odf-byte-raw-data-format,
    $odf-16bit-raw-data-format,
    $odf-32bit-raw-data-format,
    $odf-64bit-raw-data-format,
    $odf-untranslatable-raw-data-format,
    $odf-word-raw-data-format,
    $od-format-major-version,
    $od-format-minor-version,
    $like-an-hp-platform-characteristics,
    $library-summary-unit-type,
    $word-bytes,
    $word-bits,
    buffer-word,
    <dump-buffer>,
    current-pos,
    <dump-state>,
    dump-od,
    begin-dumping,
    end-dumping,
    dump-word,
    dump-raw-data,
    dump-definition-header,
    dump-end-entry,
    dump-simple-object,
    new-local-id,
    label-next-object,
    dump-local-reference,
    <load-state>,
    od-stream,
    od-buffer,
    od-next,
    od-next-setter,
    od-end,
    <dispatcher>,
    *default-dispatcher*,
    add-od-loader,
    find-data-unit,
    $end-object,
    load-object-dispatch,
    fill-at-least,
    load-raw-data,
    load-subobjects-vector,
    load-sole-subobject,
    assert-end-object,
    <forward-ref>,
    actual-obj,
    obj-resolved?,
    request-backpatch,
    resolve-forward-ref,
    <identity-preserving-mixin>,
    defined-externally?,
    maybe-dump-reference,
    load-external-definition,
    add-make-dumper,
    invert-registry,
    *Data-Unit-Search-Path*;

end;

define module dylan-dump
  use common;
  use standard-io;
  use utils;
  use od-format;
end;

define module forwards
  create
    <library>, <module>,
    <ctype>,
    <cclass>,
    <abstract-variable>,
    <ct-function>;
end;

define module compile-time-values
  use common;

  use utils;
  use forwards, import: {<ctype>};
  use od-format;

  export
    <ct-value>,
    <eql-ct-value>, ct-value-singleton, ct-value-singleton-setter,
    <literal>, literal-value, <eql-literal>,
    <ct-not-supplied-marker>,
    <literal-number>, <literal-real>, <literal-rational>,
    <literal-general-integer>, <literal-integer>, <literal-extended-integer>,
    <literal-ratio>,
    <literal-float>, <literal-single-float>, <literal-double-float>,
    <literal-extended-float>, <literal-symbol>, <literal-character>,
    <literal-boolean>, <literal-true>, <literal-false>,
    <literal-sequence>,
    <literal-list>,
    <literal-pair>, literal-head, literal-tail,
    <literal-empty-list>,
    <literal-vector>,
    <literal-simple-object-vector>,
    <literal-string>, concat-strings,
    *compiler-dispatcher*, merge-ctv-infos;
end;

define module source
  use common;
  use System, import: {copy-bytes};
  use utils;
  use od-format;
  use compile-time-values;

  export
    <source-location>, source-location-span,
    <source-location-mixin>, source-location,
    <unknown-source-location>,

    <source-file>, contents, <file-contents>,
    <file-source-location>, source-file,
    start-posn, start-line, start-column,
    end-posn, end-line, end-column,
    file-name,

    extract-string;
end;

define module tokens
  use common;
  use self-organizing-list;

  use utils;
  use source;
  use compile-time-values;
  use forwards, import: {<module>};
  use od-format;

  export

    $eof-token,
    $error-token,
    $left-paren-token,
    $right-paren-token,
    $comma-token,
    $dot-token,
    $semicolon-token,
    $left-bracket-token,
    $right-bracket-token,
    $left-brace-token,
    $right-brace-token,
    $double-colon-token,
    $minus-token,
    $equal-token,
    $double-equal-token,
    $arrow-token,
    $sharp-paren-token,
    $sharp-bracket-token,
    $double-sharp-token,
    $question-token,
    $double-question-token,
    $question-equal-token,
    $ellipsis-token,
    $true-token,
    $false-token,
    $next-token,
    $rest-token,
    $key-token,
    $all-keys-token,
    $include-token,
    $define-token,
    $end-token,
    $handler-token,
    $let-token,
    $local-token,
    $macro-token,
    $otherwise-token,
    $raw-ordinary-word-token,
    $raw-begin-word-token,
    $raw-function-word-token,
    $ordinary-define-body-word-token,
    $begin-and-define-body-word-token,
    $function-and-define-body-word-token,
    $ordinary-define-list-word-token,
    $begin-and-define-list-word-token,
    $function-and-define-list-word-token,
    $quoted-name-token,
    $constrained-name-token,
    $tilde-token,
    $other-binary-operator-token,
    $literal-token,
    $string-token,
    $symbol-token,
    $parsed-definition-macro-call-token,
    $parsed-special-definition-token,
    $parsed-local-declaration-token,
    $parsed-expression-token,
    $parsed-constant-token,
    $parsed-macro-call-token,
    $parsed-parameter-list-token,
    $parsed-variable-list-token,
    $feature-if-token,
    $feature-elseif-token,
    $feature-else-token,
    $feature-end-token,

    <token>, token-kind, 
    <symbol-token>, token-symbol, 
    <identifier-token>, token-module, token-uniquifier,
    <uniquifier>, same-id?,
    <operator-token>, operator-precedence, operator-associativity,
    <constrained-name-token>, token-constraint,
    <literal-token>, token-literal,
    <pre-parsed-token>, token-parse-tree,

    <syntax-table>, syntax-for-name, category-merge-okay?, merge-category,

    <tokenizer>, get-token, unget-token, note-potential-end-point;

end;

define module header
  use common;
  use System, import: {copy-bytes};

  use utils;
  use source;

  export
    <header>, parse-header;
end;


define module errors
  use common;
  use utils;
  use source;
  use standard-io;
  use tokens;
  export
    compiler-warning, *warnings*, compiler-error,
    compiler-warning-location, compiler-error-location,
    extract-source;
end module;

define module signature-interface
  create
    <signature>, specializers, next?, rest-type, key-infos, all-keys?,
    returns,

    <key-info>, key-name, key-type, required?, key-default,
    key-needs-supplied?-var;
end;


define module names
  use common;
  use forwards, import: {<module>};
  use utils;
  use tokens;
  use signature-interface;
  use od-format;
  use compile-time-values;

  export
    <name>,
    <basic-name>, id-name, name-symbol, name-module,
    <type-cell-name>, type-cell-name-base,
    <method-name>, method-name-generic-function, method-name-specializers,
    <generated-name>, generated-name-description,

    load-basic-name;
end;

define module definitions
  use common;

  use utils;
  use tokens;
  use source;
  use compile-time-values;
  use names;
  use od-format;
  use forwards, import: {<library>, <ctype>, <ct-function>};
  use signature-interface;
  use errors;

  export
    <definition>, defn-name, defn-library, defn-type, ct-value,
    install-transformers, $definition-slots,
    check-syntax-table-additions, make-syntax-table-additions,
    <abstract-constant-definition>, <abstract-variable-definition>,
    <implicit-definition>,
    <class-definition>, class-defn-maker-function,
    class-defn-defered-evaluations-function,
    

    <function-definition>,
    function-defn-signature, function-defn-signature-setter,
    function-defn-hairy?, function-defn-hairy?-setter,
    function-defn-ct-value, function-defn-ct-value-setter,
    function-defn-transformers,
    function-defn-movable?, function-defn-flushable?;

end;

define module variables
  use common;

  use utils;
  use errors;
  use compile-time-values;
  use tokens;
  use names;
  use definitions;
  use od-format;

  use forwards, import: {<library>, <module>}, export: all;
  export
    $Dylan-Library, $Dylan-Module, *Current-Library*, *Current-Module*,

    $Bootstrap-Module, add-bootstrap-export, define-bootstrap-module,

    find-library, library-name, note-library-definition,
    find-module, use-module, module-name, module-syntax-table,
    note-module-definition,
    <variable>, find-variable, variable-name, variable-definition,
    variable-transformers, variable-transformers-setter,
    variable-ct-evaluator, variable-ct-evaluator-setter,
    variable-fragment-expander, variable-fragment-expander-setter,
    note-variable-definition,
    <use>, <renaming>, orig-name, new-name,

    module-home, variable-home,
    name-inherited-or-exported?,

    dylan-var, dylan-defn, dylan-value;
end;

define module policy
  use common;
  use utils;
  use od-format;
  use compile-time-values;

  export <policy>, $Default-Policy;
end;

define module ctype
  use common;
  use Introspection, import: {class-name};
  use Random, import: {random-bits};

  use utils;
  use compile-time-values;
  use names;
  use variables;
  use forwards, import: {<cclass>};

  use forwards, import: {<ctype>}, export: all;
  export
    // The various types, their accessors and constructors.
    <values-ctype>,
    <multi-value-ctype>, make-values-ctype, min-values, positional-types,
	rest-value-type,
    // <ctype> is picked from from the forwards.
    <unknown-ctype>, type-exp,
    <union-ctype>, members,
    <limited-ctype>, base-class,
    <singleton-ctype>, make-canonical-singleton, singleton-value,
    <limited-integer-ctype>, make-canonical-limited-integer,
    low-bound, high-bound,
    <direct-instance-ctype>,
    <byte-character-ctype>, 

    // Operations on types.
    values-subtype?, values-types-intersect?, values-type-intersection,
    values-type-union, cinstance?, csubtype?, ctype-union, ctype-intersection,
    ctype-difference, ctypes-intersect?, ctype-eq?, ctype-neq?, 
    find-direct-classes,

    // Shorthand constructor functions.
    ct-value-cclass, wild-ctype, object-ctype, function-ctype, empty-ctype,

    // Type specifiers.
    <type-specifier>, specifier-type,

    // Ctype extension generic functions.
    csubtype-dispatch, ctype-intersection-dispatch;
end;

define module transformers
  use common;

  use utils;
  use variables;
  use ctype;

  export
    <transformer>, transformer-name, transformer-specializers,
    transformer-function, define-transformer;
end;

define module representation
  use common;

  use utils;
  use variables;
  use ctype;

  export
    <representation>, pick-representation, representation-alignment,
    representation-size, representation-has-bottom-value?;
end;

define module classes
  use common;

  use utils;
  use errors;
  use names;
  use definitions;
  use variables;
  use compile-time-values;
  use ctype;
  use representation;
  use od-format;

  use forwards, import: {<cclass>}, export: all;

  export
    cclass-name, loaded?, direct-superclasses,
    closest-primary-superclass, closest-primary-superclass-setter,
    precedence-list, subclasses, direct-subclasses, sealed?,
    abstract?, primary?, functional?, not-functional?, all-slot-infos,
    all-slot-infos-setter, new-slot-infos, new-slot-infos-setter,
    override-infos, override-infos-setter, unique-id,
    set-and-record-unique-id, subclass-id-range-min,
    subclass-id-range-max, direct-type, space-representation,
    space-representation-setter, speed-representation,
    speed-representation-setter, instance-slots-layout, vector-slot,
    vector-slot-setter, class-heap-fields, class-heap-fields-setter,
    <defined-cclass>, class-defn, class-defn-setter,

    <slot-allocation>, <slot-info>, slot-introduced-by,
    slot-type, slot-type-setter, slot-getter, slot-read-only?,
    slot-guaranteed-initialized?, slot-init-value, slot-init-value-setter,
    slot-init-function, slot-init-function-setter, slot-init-keyword,
    slot-init-keyword-required?, slot-overrides,

    <instance-slot-info>, slot-representation, slot-initialized?-slot,
    slot-positions, find-slot-offset, best-idea-of-class,

    <vector-slot-info>, slot-size-slot, slot-size-slot-setter,

    <virtual-slot-info>, <class-slot-info>, <each-subclass-slot-info>,

    <override-info>, override-introduced-by, override-introduced-by-setter, 
    override-getter, override-slot,
    override-init-value, override-init-value-setter,
    override-init-function, override-init-function-setter,

    <layout-table>, layout-length, layout-holes,

    <proxy>, proxy-for,

    inherit-slots, inherit-overrides, assign-unique-ids,
    assign-slot-representations, layout-instance-slots,

    // For dumper...
    <limited-cclass>, each-subclass-slots-count;
end;

define module type-dump
  use common;
  use standard-io;
  use utils;
  use od-format;
  use compile-time-values;
  use ctype;
  use classes;
end;

define module c-representation
  use common;

  use utils;
  use variables;
  use ctype;
  use representation;
  use classes;
  use od-format;
  use compile-time-values;

  export
    seed-representations,

    <c-representation>, more-general-representation, representation-depth,
    representation-to-more-general, representation-from-more-general,
    representation-c-type,

    <general-representation>,
    <heap-representation>,
    <immediate-representation>,

    <data-word-representation>, representation-class,
    representation-data-word-member,

    *general-rep*, *heap-rep*, *boolean-rep*,
    *long-rep*, *int-rep*, *uint-rep*, *short-rep*, *ushort-rep*,
    *byte-rep*, *ubyte-rep*, *ptr-rep*,
    *float-rep*, *double-rep*, *long-double-rep*;
end;

define module compile-time-functions
  use common;

  use utils;
  use compile-time-values;
  use signature-interface;
  use definitions;
  use ctype;
  use classes;
  use od-format;
  use forwards, import: {<ct-function>}, export: all;

  export
    ct-function-name, ct-function-signature,
    ct-function-definition, ct-function-closure-var-types,
    has-general-entry?, has-general-entry?-setter,

    <ct-generic-function>, <ct-open-generic>, <ct-sealed-generic>,

    <ct-method>, ct-method-hidden?,
    <ct-accessor-method>, ct-accessor-method-slot-info,
    ct-accessor-standin, ct-accessor-standin-setter,
    has-generic-entry?, has-generic-entry?-setter,

    <ct-entry-point>, ct-entry-point-for, ct-entry-point-kind;
end;

define module flow
  use common;
  use utils;
  use ctype;
  use source;
  use od-format;
  use forwards, import: {<abstract-variable>}, export: all;
  export 
    <region>, <linear-region>, <simple-region>, <compound-region>,
    <empty-region>,
    <join-region>, <if-region>, <body-region>, <block-region-mixin>,
    <block-region>, <function-region>, <loop-region>, <exit>, <return>,
    <component>,

    parent, parent-setter, first-assign, first-assign-setter, last-assign,
    last-assign-setter, regions, regions-setter, join-region,
    join-region-setter, then-region,
    then-region-setter, else-region, else-region-setter, body, body-setter,
    exits, exits-setter, block-of, block-of-setter,
    next-exit, next-exit-setter, returned-type, returned-type-setter,
    guessed-returned-type, guessed-returned-type-setter,
    initial-variables, initial-variables-setter,
    reoptimize-queue, reoptimize-queue-setter,
    add-to-queue, all-function-regions,

    <expression>, <dependency>, <queueable-mixin>, <dependent-mixin>,
    <leaf>, <variable-info>, <definition-site-variable>,
    <ssa-variable>, <initial-definition>, <multi-definition-variable>,
    <initial-variable>, <operation>,
    <join-operation>, <abstract-assignment>, <assignment>,
    <join-assignment>,

    dependents, derived-type, guessed-type, source-exp, source-next,
    dependent, dependent-next, var-info, asserted-type, definer,
    definer-next, needs-type-check?, queue-next, definition-of,
    definitions, next-initial-variable, next-initial-variable-setter,
    defines, region, next-op, prev-op, depends-on, component-of,

    dependents-setter, derived-type-setter, guessed-type-setter,
    source-exp-setter, source-next-setter, dependent-setter,
    dependent-next-setter, var-info-setter, asserted-type-setter,
    definer-setter, definer-next-setter, needs-type-check?-setter,
    queue-next-setter, definition-of-setter, definitions-setter,
    defines-setter, region-setter, next-op-setter, depends-on-setter,
    prev-op-setter;

end;

define module signature
  use signature-interface;
  use compile-time-values;
  use common;
  use utils;
  use ctype;
  use definitions;
  use representation;
  use od-format;
end;

