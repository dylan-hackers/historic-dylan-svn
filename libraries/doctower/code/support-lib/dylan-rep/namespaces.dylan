module: dylan-rep
synopsis: Classes and methods for representing Dylan libraries, modules, bindings.


//
// Libraries
//


/**
A library definition lists used libraries and modules. Modules may be created
and exported from a library; created but internal to a library; excluded from
import from another library; imported and re-exported from another library;
imported and re-exported under a different name from another library; or
imported but not re-exported from another library. There can be multiple use
clauses for the same library. Modules created by multiple clauses are merged.

Modules fall into these categories:

   Exported    - Module is listed in export clause. Instance of <local-module>.
   
   Internal    - Module is created via "define module" but not mentioned in
                 "define library". Instance of <local-module>.
                 
   Excluded    - Module is listed in use clause exclude option, or not listed in
                 use clause import option. No representation.
                 
   Reexported  - Module is listed in use clause export option. Instance of
                 <imported-module>.
                 
   Renamed     - Module is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Instance of <imported-module>.
                 
   Imported    - Module is listed in use clause import option, but not listed
                 in export option. Instance of <imported-module>.
   
   Dylan-User  - Every library has an internal Dylan-User module that can't be
                 exported from the library. We are not including it, but if it
                 were included, it would be an instance of <local-module> with
                 pre-defined bindings.
**/
define abstract class <library>
      (<named-api-element>, <documentable-api-element>, <source-location-mixin>)
   slot modules = make(<stretchy-vector> /* of <module> */);
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <library> */);
end class;

define class <known-library> (<library>)
   slot file-markup-tokens = make(<stretchy-vector> /* of <markup-content-token> */);
end class;

define class <unknown-library> (<library>)
end class;

/**
Method: \= (<library>, <library>)
=================================
Two libraries are equal if they refer to the same library.
**/

define method \= (lib1 :: <library>, lib2 :: <library>)
=> (equal? :: <boolean>)
   case
      lib1 == lib2 => #t;
      case-insensitive-equal?(lib1.local-name, lib2.local-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Modules
//


define abstract class <module>
      (<named-api-element>, <documentable-api-element>, <source-location-mixin>)
   slot bindings = make(<stretchy-vector> /* of <binding> */);
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
   slot unknown-reexport-sources = make(<stretchy-vector> /* of <module> */);
end class;

define class <local-module> (<module>)
   slot file-markup-tokens = make(<stretchy-vector> /* of <markup-content-token> */);
end class;

/**
If 'used-library' is false, the module is a stray. It is imported from some
library but we do not know which one. This can happen under the following 
circumstances:

 - The module is mentioned in a module definition's use clause, but has no local
   definition in this library. The module must be imported from some other
   library, but is not mentioned in any of the library definition's use clauses.
   
 - Another library imports the module, but the module has no local definition in
   this library. The module must therefore be imported from another library and
   reexported, but the module is not explicitly mentioned in this library's
   definition.
**/
define class <imported-module> (<module>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-library :: false-or(<library>), required-init-keyword: #"used-library";
end class;

define method stray? (mod :: <local-module>) => (stray? :: <boolean>)
   #f
end method;

define method stray? (mod :: <imported-module>) => (stray? :: <boolean>)
   mod.used-library.false?
end method;

/**
Method: \= (<module>, <module>)
===============================
Two modules are equal if they are named the same and either both local or both
imported from the same module in the same library. If the module is a stray, its
'import-name', 'used-library', and class are not reliable and are irrelevant for
comparison purposes; the module is assumed to be equal to any another module
with the same 'local-name'.

Function is not applicable when mod1 and mod2 are in different libraries.
**/

define method \= (mod1 :: <module>, mod2 :: <module>)
=> (equal? :: <boolean>)
   case
      ~case-insensitive-equal?(mod1.local-name, mod2.local-name) => #f;
      mod1.stray? | mod2.stray? => #t;
      otherwise => #f;
   end case;
end method;

define method \= (mod1 :: <local-module>, mod2 :: <local-module>)
=> (equal? :: <boolean>)
   case
      mod1 == mod2 => #t;
      case-insensitive-equal?(mod1.local-name, mod2.local-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (mod1 :: <imported-module>, mod2 :: <imported-module>)
=> (equal? :: <boolean>)
   case
      mod1 == mod2 => #t;
      ~case-insensitive-equal?(mod1.local-name, mod2.local-name) => #f;
      mod1.stray? | mod2.stray? => #t;
      mod1.used-library = mod2.used-library
            & case-insensitive-equal?(mod1.import-name, mod2.import-name) => #t;
      otherwise => #f;
   end case
end method;


//
// Bindings
//


/**
Modules are not shared between libraries, nor are the bindings they contain.
Instead, each library has its own set of modules and bindings. These bindings
may be copied from another library's module, however. If the module is an
imported or reexported module, its bindings are copied verbatim from the library
and module from whence it came. If the module is an internal or exported module,
its bindings come from other (used) modules from the same library as well as any
bindings in the module itself.

Bindings may be defined and exported from a module; defined but internal to a
module; undefined but exported from a module; excluded from import from another
module; imported and re-exported from another module; imported and re-exported
under a different name from another module; or imported but not re-exported from
another module. There can be multiple use clauses for the same module. Bindings
created by multiple clauses are merged.

Bindings fall into these categories:

   Exported    - Binding is listed in export clause and has a definition; also,
                 all bindings in a module imported or reexported from another
                 library are of this type. Instance of <local-binding>.
   
   Internal    - Binding is defined but not mentioned in "define module." These
                 bindings do not "need" [em] to be tracked, as the binding
                 itself and any definitions associated with it are not exported
                 from the module. Any other bindings that mention an internal
                 binding will do so in an expression (i.e. as text).

                 We cannot distinguish an internal binding from an imported or
                 reexported binding that is not specifically listed, therefore
                 it has no representation; otherwise, it would be an instance of
                 <local-binding>.

   Excluded    - Binding is listed in use clause exclude option, or not listed
                 in use clause import option. No representation.
                 
   Reexported  - Binding is listed in use clause export option. Definition may
                 be added to binding. Instance of <imported-binding>.
                 
   Renamed     - Binding is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Definition may be added to binding. Instance of
                 <imported-binding>.
                 
   Imported    - Binding is listed in use clause import option, but not listed
                 in export option. Definitions may be added to binding. Instance
                 of <imported-binding>.
                 
   Created     - Binding is listed in create clause. Its definition is an
                 instance of <deferred-definition> unless resolved. Instance of
                 <local-binding>.

A class, method, or other definition may be associated with a <local-binding> or
an <imported-binding>. If the definition is for an exported binding, it will be
associated with a <local-binding>. In all other cases, we must associate it with
a stray (until resolved) <imported-binding>.

A binding's owner in a library (if known) is the module that has the
<local-binding>.
**/
define abstract class <binding> (<named-api-element>, <source-location-mixin>)
   slot definition :: false-or(<definition>) = #f, init-keyword: #"definition";
   slot exported? :: <boolean> = #f, init-keyword: #"exported";
end class;

define class <local-binding> (<binding>)
end class;

/**
If 'used-module' is false, the binding is a stray. It is imported from some
module but we do not know which one. This can happen under the following 
circumstances:

 - Another module imports the binding, but the binding has no local definition in this
   module. The binding must therefore be imported from another module and
   reexported, but the binding is not explicitly mentioned in this module's
   definition.
**/
define class <imported-binding> (<binding>)
   slot import-name :: <string>, required-init-keyword: #"import-name";
   slot used-module :: false-or(<module>), required-init-keyword: #"used-module";
end class;

define method stray? (mod :: <local-binding>) => (stray? :: <boolean>)
   #f
end method;

define method stray? (mod :: <imported-binding>) => (stray? :: <boolean>)
   mod.used-module.false?
end method;

/**
Method: \= (<binding>, <binding>)
=================================
Two bindings are equal if they are named the same and either both local or both
imported from the same binding in the same module. If the binding is imported
from a stray module, any other module with the same name is assumed to be the
same module. If the binding itself is a stray, any other binding with the same
name is assumed to be the same binding.

Binding equality is not related to the API definition; two bindings may refer to
the same definition but not equal each other, and refer to different definitions
but equal each other.

Function is not applicable when bind1 and bind2 are in different modules.
**/

define method \= (bind1 :: <local-binding>, bind2 :: <local-binding>)
=> (equal? :: <boolean>)
   case
      bind1 == bind2 => #t;
      case-insensitive-equal?(bind1.local-name, bind2.local-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (bind1 :: <imported-binding>, bind2 :: <imported-binding>)
=> (equal? :: <boolean>)
   case
      bind1 == bind2 => #t;
      ~case-insensitive-equal?(bind1.local-name, bind2.local-name) => #f;
      bind1.stray? | bind2.stray? => #t;
      bind1.used-module = bind2.used-module
            & case-insensitive-equal?(bind1.import-name, bind2.import-name) => #t;
      otherwise => #f;
   end case
end method;

define method \= (bind1 :: <local-binding>, bind2 :: <imported-binding>)
=> (equal? :: <boolean>)
   case
      ~case-insensitive-equal?(bind1.local-name, bind2.local-name) => #f;
      bind2.stray? => #t;
      bind2.used-module.stray? => #t;
      otherwise => #f;
   end case
end method;

define inline method \= (bind1 :: <imported-binding>, bind2 :: <local-binding>)
=> (equal? :: <boolean>)
   bind2 = bind1
end method;
