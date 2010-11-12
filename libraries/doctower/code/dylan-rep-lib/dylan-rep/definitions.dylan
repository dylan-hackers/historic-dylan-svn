module: dylan-rep
synopsis: Representation of Dylan libraries, modules, and general bindings.


/// Synopsis: An API object potentially created by a defining macro.
///
/// Subclasses are partitioned into <namespace> and <binding>.
define abstract class <definition> (<documentable-api-object>)
   /// The scoped name of the definition according to its definer macro or
   /// create clause, or whatever else is handy. The best available canonical
   /// name is chosen while merging.
   slot canonical-name :: <source-name>, required-init-keyword: #"local-name";
   
   /// Where this definition instance comes from. Used to select the best
   /// canonical name and source location.
   ///
   /// #"predefined"         - From the DRM.
   /// #"create-clause"      - From a "create" clause in a "define module"
   ///                         statement.
   /// #"generic-definition" - From a "define generic" statement.
   /// #"definition"         - From a "define method", "define class", or other
   ///                         definition statement, or an implicit generic.
   /// #"inference"          - From an export or import option in a "use" clause.
   /// #"declaration"        - From a "slot" clause and its getter and setter
   ///                         declarations, or a "define sealed domain" statement,
   ///                         or a "use" or "export" clause.
   /// #"expression"         - Mentioned in an expression, including type
   ///                         expressions.
   slot provenance :: one-of(#"predefined", #"create-clause", #"generic-definition", 
                             #"definition", #"inference", #"declaration",
                             #"expression"),
      required-init-keyword: #"provenance";
   
   /// All source names of the definition.
   slot aliases :: <vector> = make(<stretchy-vector>);
end class;


define method initialize (obj :: <definition>, #key) => ()
   next-method();
   obj.aliases := add!(obj.aliases, obj.canonical-name);
end method;


define method api-type-name (api :: <library>) => (type :: <string>)
   "library"
end method;

define method api-type-name (api :: <module>) => (type :: <string>)
   "module"
end method;

define method api-type-name (api :: <binding>) => (type :: <string>)
   "binding"
end method;

define method api-type-name (api :: <constant-binding>) => (type :: <string>)
   "constant"
end method;

define method api-type-name (api :: <variable-binding>) => (type :: <string>)
   "variable"
end method;

define method api-type-name (api :: <class-binding>) => (type :: <string>)
   "class"
end method;

define method api-type-name (api :: <generic-binding>) => (type :: <string>)
   "generic function"
end method;

define method api-type-name (api :: <function-binding>) => (type :: <string>)
   "function"
end method;

define method api-type-name (api :: <macro-binding>) => (type :: <string>)
   "macro"
end method;


//
// Namespaces
//


define abstract class <namespace> (<definition>)
   /// Exported definitions in this namespace. These are a subsequence of keys
   /// in 'definitions'. Elements are local names. Each is a <string>. 
   slot exported-names :: <vector> = make(<stretchy-vector>);

   /// Definitions in this namespace. Keyed by local name, a <string>. Includes
   /// both exported and internal definitions.
   slot definitions :: <case-insensitive-skip-list>
      = make(<case-insensitive-skip-list>);
end class;


/// Synopsis: A library or module created by an actual defining macro.
///
/// Its source location is a defining macro.
define abstract class <defined-namespace> (<namespace>)
   /// Sequence of other namespaces from which additional, unlisted definitions
   /// may be exported. The only namespaces in this list are <undefined-namespace>,
   /// and they are only included if the chain of namespaces leading to them
   /// had all imported and reexported all definitions.
   ///
   /// Basically, this slot can be used to add text like "Other bindings from
   /// common-dylan" to a list of exported bindings.
   slot unknown-reexport-sources :: <vector> = make(<stretchy-vector>);

   /// Sequence of <markup-content-token>. These tokens are in the source code
   /// but aren't associated with any particular API object.
   slot file-markup-tokens :: <vector> = make(<stretchy-vector>);
end class;


/// Synopsis: A library or module that does not correspond to a defining macro.
///
/// Its source location is a "use" clause in a defining macro.
define abstract class <undefined-namespace> (<namespace>)
end class;


//
// Libraries
//


define abstract class <library> (<namespace>)
   keyword #"local-name", type: <library-name>;
end class;

define class <defined-library> (<library>, <defined-namespace>)
end class;

define class <undefined-library> (<library>, <undefined-namespace>)
end class;


//
// Modules
//


define abstract class <module> (<namespace>)
   keyword #"local-name", type: <module-name>;
end class;

define class <defined-module> (<module>, <defined-namespace>)
end class;

define class <undefined-module> (<module>, <undefined-namespace>)
end class;


//
// Bindings
//


/// Bindings are owned by specific modules and libraries, but map to several
/// explicit and implicit definitions gathered from different libraries.
///
/// Its source location is whatever determined the canonical name.
define abstract class <binding> (<definition>)
   /// Implicit and explicit definitions associated with the binding.
   virtual slot all-defns :: <sequence>;

   /// False if the binding needs to be merged with another binding to be valid.
   virtual slot valid-binding? :: <boolean>;

   keyword #"local-name", type: <binding-name>;
end class;
