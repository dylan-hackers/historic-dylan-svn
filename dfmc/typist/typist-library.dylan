Module:    Dylan-User
Author:    Steve Rowley
Synopsis:  Library and module definitions for the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// The Typist -- A type-inference module for the DFM.
///

define library dfmc-typist
  // Library for the type inference tool.
  use functional-dylan;
  use dfmc-core;
  use dfmc-reader;

  export dfmc-typist;
end;

define module dfmc-typist
  // Module for the typist.
  use functional-dylan;
  use dfmc-core;
  use dfmc-reader, import: { <compiler-range-source-location>, source-location-record };
  use dfmc-imports;

  export type-estimate,
    type-estimate-object,
    type-infer,
    type-estimate-top-level-form,
    initialize-type-environment!,
    retract-type!,
    empty-retype-queue,
    solve-and-upgrade;

  //visualizer
  export *typist-visualize*;

  //method upgrading
  export *call-upgrading?*,
    *profile-all-calls?*,
    *colorize-dispatch*,
    color-dispatch, color-location,
    incf-dynamic-dispatch-count,
    guaranteed-disjoint?,
    guaranteed-joint?,
    effectively-disjoint?,
    <unknown-keyword-in-call>,
    all-applicable-methods-guaranteed-known?,
    guaranteed-method-precedes?,
    guaranteed-sorted-applicable-methods,
    slot-fixed-offset-in,
    argument-type-estimates,
    simplify-call-to-call-to-object!,
    maybe-check-function-call,
    estimate-effective-methods,
    maybe-upgrade-call,
    generate-stack-vector,
    upgrade-to-congruent-call!,
    //better: dfmc-flow-graph?
    do-callers;

  export maybe-convert-box, maybe-convert-unbox;

  export method-upgrade?;

  //constant folding method calls
  export fold-function-call,
    replace-call-with-values,
    maybe-fold-function-call;

  export best-function-key?;
end;
