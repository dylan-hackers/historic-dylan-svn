module: dylan-translator


/**
A library definition lists used libraries and modules. Modules may be created
and exported from the current library, created but internal to the current
library, excluded from import from another library, imported and re-exported
from another library, imported and re-exported under a different name from
another library, or imported but not re-exported from another library. There can
be multiple use clauses for the same library.

   Exported    - Module is listed in export clause. Instance of <exported-module>.
   Internal    - Module is created via "define module" but not mentioned in
                 "define library". Instance of <internal-module>.
   Excluded    - Module is listed in use clause exclude option, or not listed in
                 use clause import option. No representation.
   Reexported  - Module is listed in use clause export option. Instance of
                 <reexported-module>.
   Renamed     - Module is listed in use clause export option and given a new
                 name with rename option, import option, or prefix option.
                 Instance of <reexported-module>.
   Imported    - Module is listed in use clause import option, but not listed
                 in export option. Renaming these is possible but has no effect.
                 Instance of <imported-module>.

The intent of processing a library definition is to create a list of exported
modules for the given library, with the proper bindings in each module. The only
real fact of interest in the documentation of the library itself is the modules
being exported, so we can ignore excluded and imported modules and the original
names of renamed modules.

However, this information needs to be preserved for the creation of module
documentation; a module will need to refer to its used modules for reexported
bindings.
**/
define method process-library-definition
   (library :: <library>, token :: <library-definer-token>)
=> (library :: <library>)
   library.local-name := token.api-name;

   let clauses = token.namespace-clauses;

   let exp-clauses = choose(rcurry(instance?, <export-clause-token>), clauses);
   let exp-module-names = apply(concatenate, #[], map(export-names, exp-clauses));
   let exp-modules = map(curry(make, <exported-module>, local-name:), exp-module-names);
   
   let use-clauses = choose(rcurry(instance?, <use-clause-token>), clauses);
   let used-libraries = make(<equal-table>);
   for (clause in use-clauses)
      let used-lib = make(<used-library>, import-name: clause.use-name);
      let imp-all? = clause.use-imports == #"all";
      let exp-all? = clause.use-exports == #"all";
      // TODO: To handle imported and reexported modules across libraries within
      // this same documentation package as well as possible, need to model as
      // many library and module exports as possible. This means, given a set of
      // library file sets, their libraries should be found, and used libraries
      // evaluated recursively and exports determined until reaching libraries and
      // modules that aren't part of the documentation package.
   end for;
   let used-lib-names = map(use-name, use-clauses);
   map-into(library.used-libraries, curry(make, <used-library>, import-name:),
            used-lib-names);

   
end method;