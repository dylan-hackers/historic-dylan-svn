module: dylan-rep
synopsis: Representation of Dylan constant, variable, and other special bindings,
          and methods related to general bindings.


//
// General binding methods
//


define method all-defns 
   (bind :: type-union(<placeholder-binding>, <empty-binding>))
=> (defns :: <sequence>)
   #[]
end method;


define method all-defns (bind :: <generic-binding>) => (defns :: <sequence>)
   concatenate(next-method(), bind.implicit-defns)
end method;


define method all-defns (bind :: <binding>) => (defns :: <sequence>)
   if (bind.explicit-defn) vector(bind.explicit-defn) else #[] end
end method;


define method valid-binding? (bind :: <placeholder-binding>) => (incom? :: <boolean>)
   #f
end method;


define method valid-binding? (bind :: <empty-binding>) => (incom? :: <boolean>)
   #t
end method;


define method valid-binding? (bind :: <generic-binding>) => (incom? :: <boolean>)
   bind.explicit-defn.true? | bind.implicit-defns.size > 0
end method;


define method valid-binding? (bind :: <binding>) => (incom? :: <boolean>)
   bind.explicit-defn.true?
end method;


/// Synopsis: An incomplete binding whose type is not resolved. No implicit or
/// explicit definitions.
///
/// Its source location is whatever expression mentioned it.
define class <placeholder-binding> (<binding>)
end class;


/// Synopsis: A binding created by a "create" clause. No implicit or explicit
/// definitions.
///
/// While <empty-binding> is a subclass of <documentable-api-object>, there is
/// no way to actually provide documentation for it other than via another
/// binding or a dedicated markup topic such as "Generic Function: read-line".
/// Thus, assuming the writer writes any documentation for it, its binding type 
/// will always be known to the reader.
///
/// Its source location is a "create" clause in a module definer macro.
define class <empty-binding> (<placeholder-binding>)
end class;


//
// Implicit/explicit definitions
//


/// Synopsis: One or more of these, along with a name, comprise a binding.
///
/// Its source location is a defining macro.
define abstract class <implicit/explicit-defn> (<api-object>)
end class;


//
// Constants and variables
//


define class <constant-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-constant-defn>),
      required-init-keyword: #"explicit";
end class;


define class <variable-binding> (<binding>)
   slot explicit-defn :: false-or(<explicit-variable-defn>),
      required-init-keyword: #"explicit";
end class;


define abstract class <const/var-defn> (<implicit/explicit-defn>)
   slot adjectives = make(<stretchy-vector>);
   slot type :: false-or(<type-fragment>) = #f, init-keyword: #"type";
   slot value :: <computed-constant>, required-init-keyword: #"value";
end class;


define class <explicit-constant-defn> (<const/var-defn>)
end class;


define class <explicit-variable-defn> (<const/var-defn>)
end class;

