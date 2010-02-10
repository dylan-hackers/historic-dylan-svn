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
  use dfmc-reader, import: { <compiler-range-source-location>, source-location-record, fragment-name };
  use dfmc-imports;

  export type-estimate,
    type-estimate-object,
    type-infer,
    type-estimate-top-level-form,
    initialize-type-environment!,
    retract-type!,
    upgrade-cells,
    re-type,
    solve-and-upgrade;

  export guaranteed-disjoint?;

  //visualizer
  export *typist-visualize*;
end;
