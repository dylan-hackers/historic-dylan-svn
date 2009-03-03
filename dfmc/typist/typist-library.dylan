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
  use dfmc-reader;
  use dfmc-imports;

  export <type-variable>,
    type-variable-contents,
    type-contents-callback,
    type-contents-callback-setter;

  export type-estimate,
    lookup-type,
    type-estimate-top-level-form;

  export constant-value?;

  //visualizer
  export *typist-visualize*;

  //old stuff, need to get rid of this
  export <type-estimate>,
    <type-estimate-bottom>,
    <type-estimate-class>,
    type-estimate-class,
    <type-estimate-raw>,
    type-estimate-raw,
    <type-estimate-union>,
    type-estimate-unionees,
    <type-estimate-values>,
    type-estimate-fixed-values,
    type-estimate-rest-values,
    <type-estimate-limited>,
    <type-estimate-limited-function>,
    <type-estimate-limited-collection>,
    type-estimate-concrete-class,
    type-estimate-dimensions,
    type-estimate-size,
    type-estimate-of,
    <type-estimate-limited-instance>,
    type-estimate-singleton,
    type-estimate-debug-name,
    type-estimate-values-rest-subtype?,
    type-estimate-values-element-subtype?,
    type-estimate-subtype?,
    type-estimate-retract,
    make-type-estimate,
    type-estimate-values-ref;
end;
