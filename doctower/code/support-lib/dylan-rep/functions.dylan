module: dylan-rep
synopsis: Classes and methods for representing Dylan functions & arguments.


//
// Definitions
//


define class <generic-defn> (<definition>)
   slot explicit-defn :: false-or(<explicit-generic-defn>) = #f,
      init-keyword: #"explicit";
   slot implicit-defns = make(<stretchy-vector> /* of <implicit-generic-defn> */),
      init-keyword: #"implicit";
   slot sealed-domains = make(<stretchy-vector> /* of <sealed-domain> */);
   slot sealed? :: <boolean> = #t, init-keyword: #"sealed";
end class;

define class <function-defn> (<definition>)
   slot explicit-defn :: <explicit-function-defn>,
      required-init-keyword: #"explicit";
end class;

define class <sealed-domain> (<object>)
   slot sealed-types :: <sequence> /* of <type-fragment> */,
      required-init-keyword: #"types";
end class;

define method \= (seal1 :: <sealed-domain>, seal2 :: <sealed-domain>)
=> (equal? :: <boolean>)
   seal1.sealed-types = seal2.sealed-types
end method;


//
// Generics and functions
//


define abstract class <func/gen-definition> (<documentable-api-element>)
   slot adjs = make(<stretchy-vector> /* of #"sealed", #"abstract", etc. */);
   slot parameter-list :: <parameter-list>, init-keyword: #"parameter-list";
end class;

define class <explicit-generic-defn> (<func/gen-definition>, <source-location-mixin>)
   slot vendor-options = make(<stretchy-vector> /* of <vendor-option> */);
end class;

define class <implicit-generic-defn> (<func/gen-definition>, <source-location-mixin>)
end class;

define class <explicit-function-defn> (<func/gen-definition>, <source-location-mixin>)
end class;

define class <vendor-option> (<object>)
   slot symbol :: <string>, required-init-keyword: #"symbol";
   slot code-fragment :: <code-fragment>, required-init-keyword: #"fragment";
end class;


//
// Parameter lists
//


define class <parameter-list> (<object>)
   slot param-list :: <param-list>, required-init-keyword: #"param-list";
   slot value-list :: <value-list>, required-init-keyword: #"value-list";
end class;

define abstract class <param-list> (<object>)
   slot req-params = make(<stretchy-vector> /* of <req-param> */),
      init-keyword: #"req-params";
end class;

define class <fixed-param-list> (<param-list>)
end class;

define class <key-param-list> (<param-list>)
   slot key-params = make(<stretchy-vector> /* of <key-param> */);
   slot all-keys? :: <boolean> = #f;
   slot rest-param :: false-or(<rest-param>) = #f;
end class;

define class <var-param-list> (<param-list>)
   slot rest-param :: <rest-param>;
end class;

define class <value-list> (<object>)
   slot req-values = make(<stretchy-vector> /* of <req-value> */),
      init-keyword: #"req-values";
   slot rest-value :: false-or(<rest-value>) = #f;
end class;

define abstract class <param> (<documentable-api-element>)
   slot local-name :: <string>, required-init-keyword: #"name";
end class;

define class <req-param> (<param>)
   slot type :: false-or(<type-fragment>) = #f, init-keyword: #"type";
end class;

define class <key-param> (<param>)
   slot symbol :: <string>, required-init-keyword: #"name";
   slot type :: false-or(<type-fragment>) = #f;
   slot expr :: false-or(<code-fragment>) = #f;
end class;

define class <rest-param> (<param>)
end class;

define abstract class <value> (<documentable-api-element>)
   slot local-name :: <string>, required-init-keyword: #"name";
   slot type :: false-or(<type-fragment>) = #f;
end class;

define class <req-value> (<value>)
end class;

define class <rest-value> (<value>)
end class;
