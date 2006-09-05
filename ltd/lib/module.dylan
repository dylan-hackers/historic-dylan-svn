Module:    Dylan-User
Author:    Scott McKay

define library CL
  use common-dylan;
  use collections;
  use duim-utilities;
  export
    CL-macros,
    CL-sequences,
    CL-plists,
    CL-strings;
end;

define module CL-macros
  use dylan;
  create \push!, \pop!;
end;

define module CL-sequences
  use dylan;
  create cl-position, cl-position-if,
         cl-find, cl-find-if,
         cl-assoc, cl-assoc-if,
         cl-count, cl-count-if,
         cl-remove, cl-remove-if,
         cl-remove!, cl-remove-if!,
         cl-substitute, cl-substitute-if,
         cl-substitute!, cl-substitute-if!,
         cl-remove-duplicates, cl-remove-duplicates!,
         cl-search,
         cl-mismatch,
         cl-merge;
end;

define module CL-plists
  use dylan;
  use plists, export: all;
end;

define module CL-strings
  use dylan;
  use duim-utilities,
    export: { char-equal?, char-not-equal?,
              char-less?, char-not-less?,
              char-greater?, char-not-greater?,
              string-equal?, string-not-equal?,
              string-less?, string-not-less?,
              string-greater?, string-not-greater?,
              alpha-char?, digit-char?,
              alphanumeric-char?,
              upper-case?, lower-case?,
              standard-char?,
              graphic-char?,
              ordinary-char?,
              whitespace-char?,
              string-capitalize, string-capitalize!,
              string-capitalize-words, string-capitalize-words!,
              string-trim, string-left-trim, string-right-trim,
              string-search-set, string-search-not-set,
              string-pluralize,
              string-a-or-an };
end;

define module CL-internals
  use common-dylan;
  use CL-macros;
  use CL-sequences;
  use CL-plists;
  use CL-strings;
end;
