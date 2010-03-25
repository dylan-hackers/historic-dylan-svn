module: dylan-rep
synopsis: Representation of Dylan functions, arguments, and values.


define class <generic-binding> (<binding>)
   /// False if generic is implicitly defined.
   slot explicit-defn :: false-or(<explicit-generic-defn>) = #f,
      init-keyword: #"explicit";
   
   /// Sequence of <implicit-generic-defn>.
   slot implicit-defns :: <sequence> = make(<stretchy-vector>), 
      init-keyword: #"implicit";
   
   /// Sequence of <sealed-domain>.
   slot sealed-domains :: <sequence> = make(<stretchy-vector>);
   
   /// True if the generic itself is sealed, regardless of any sealed domains.
   slot sealed? :: <boolean> = #t, init-keyword: #"sealed";
end class;


define class <function-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-function-defn>),
      required-init-keyword: #"explicit";
end class;


define class <sealed-domain> (<object>)
   /// Sequence of <type-fragment>. The types of the required parameters over
   /// which the generic is sealed.
   slot sealed-types :: <sequence>, required-init-keyword: #"types";
end class;


define method \= (seal1 :: <sealed-domain>, seal2 :: <sealed-domain>)
=> (equal? :: <boolean>)
   seal1.sealed-types = seal2.sealed-types
end method;


//
// Implicit/explicit definitions
//


define abstract class <func/gen-defn> (<implicit/explicit-defn>)
   /// Sequence of #"sealed", #"abstract", etc.
   slot adjectives :: <sequence> = make(<stretchy-vector>);
   slot param-list :: <param-list>, init-keyword: #"param-list";
   slot value-list :: <value-list>, init-keyword: #"value-list";
end class;


/// Synopsis: A "define generic" definition.
define class <explicit-generic-defn> (<func/gen-defn>)
   slot vendor-options :: <sequence> = make(<stretchy-vector> /* of <vendor-option> */);
end class;


/// Synopsis: A "define method" definition. This implicitly creates a generic
/// with a method in it.
define class <implicit-generic-defn> (<func/gen-defn>, <documentable-api-object>)
end class;


define class <explicit-function-defn> (<func/gen-defn>)
end class;


define class <vendor-option> (<object>)
   slot symbol :: <string>, required-init-keyword: #"symbol";
   slot code-fragment :: <code-fragment>, required-init-keyword: #"fragment";
end class;


//
// Parameter lists
//


define abstract class <param-list> (<object>)
   slot req-params :: <sequence> = make(<stretchy-vector> /* of <req-param> */),
      init-keyword: #"req-params";
end class;


define class <fixed-param-list> (<param-list>)
end class;


define class <key-param-list> (<param-list>)
   slot key-params :: <sequence> = make(<stretchy-vector> /* of <key-param> */);
   // TODO: slot takes-keys? :: <boolean>
   slot all-keys? :: <boolean> = #f;
   slot rest-param :: false-or(<rest-param>) = #f;
end class;


define class <var-param-list> (<param-list>)
   slot rest-param :: <rest-param>;
end class;


define class <value-list> (<object>)
   slot req-values :: <sequence> = make(<stretchy-vector> /* of <req-value> */),
      init-keyword: #"req-values";
   slot rest-value :: false-or(<rest-value>) = #f;
end class;


/// Its source location is a parameter in a parameter list.
define abstract class <param> (<documentable-api-object>)
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


/// Its source location is a value in a parameter list.
define abstract class <value> (<documentable-api-object>)
   slot local-name :: <string>, required-init-keyword: #"name";
   slot type :: false-or(<type-fragment>) = #f;
end class;


define class <req-value> (<value>)
end class;


define class <rest-value> (<value>)
end class;
