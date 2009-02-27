module: dylan-translator


// /**
// Synopsis: Creates bindings in modules.
// 
// Modules are not shared between libraries, nor are the bindings they contain.
// Instead, each library has its own set of modules and bindings. These bindings
// may be copied from another library's module, however. If the module is an
// imported or reexported module, its bindings are copied verbatim from the library
// and module from whence it came. If the module is an internal or exported module,
// its bindings come from other (used) modules from the same library as well as any
// bindings in the module itself.
// 
// Bindings fall into these categories:
// 
//    Exported    - Binding is listed in export clause and has a definition.
//                  Instance of <exported-binding>.
//    
//    Internal    - Binding is defined but not mentioned in "define module." These
//                  bindings do not need to be tracked, as the binding itself and
//                  any definitions associated with it not exported from the
//                  module. Any other bindings that mention an internal binding
//                  will do so in an expression (i.e. as text). No representation.
//                  
//    Excluded    - Binding is listed in use clause exclude option, or not listed in
//                  use clause import option. No representation.
//                  
//    Reexported  - Binding is listed in use clause export option. Definition may be
//                  added to binding. All bindings from modules reexported by a
//                  library are of this type, though definitions cannot be added to
//                  them. Instance of <reexported-binding>.
//                  
//    Renamed     - Binding is listed in use clause export option and given a new
//                  name with rename option, import option, or prefix option.
//                  Definition may be added to binding. Instance of
//                  <reexported-binding>.
//                  
//    Imported    - Binding is listed in use clause import option, but not listed
//                  in export option. Definitions may be added to an imported
//                  binding. Instance of <imported-binding>.
//                  
//    Created     - Binding is listed in create clause and does not have a definition.
//                  Instance of <exported-binding>.
// 
// A binding's owner (if known) is the module that has the <exported-binding>.
// 
// --- Arguments: ---
// library-sets - A sequence of <pair>. The head of each pair is a <library> and 
//                the tail is a sequence of <interchange-file-token>. The libraries
//                should have their modules all set up in order of dependency, and
//                the libraries themselves should be in order of dependency.
// **/
// define method populate-bindings (library-sets :: <sequence>) => ()
//    local method module-header (file :: <interchange-file-token>)
//          => (header :: <header-token>)
//             let module-headers = choose(
//                   method (hdr :: <header-token>) => (mod-hdr?)
//                      case-insensitive-equal?(hdr.hdr-keyword, "module")
//                   end, file.headers);
// 
//             when (module-headers.empty?)
//                no-header-in-interchange-file(header: "Module:",
//                      location: file.token-src-loc.source-file);
//             end when;
// 
//             let header = module-headers.first;
//             when (header.hdr-value.empty?)
//                empty-header-in-interchange-file(header: "Module:",
//                      location: header.token-src-loc);
//             end when;
//             header
//          end method,
//          
//          method file-in-module? (name :: <string>, file :: <interchange-file-token>)
//          => (file-in-module? :: <boolean>)
//             case-insensitive-equal?(name, file.interchange-module-header.hdr-value)
//          end method;
//          
//    for (library-set in library-sets)
//       let library = library-set.head;
//       let library-files = library-set.tail;
// 
//       let module-tokens =
//             choose-interchange-definitions(<module-definer-token>, library-files);
//       let module-tokens-by-name = make(<case-insensitive-string-table>);
//       for (token in module-tokens)
//          module-tokens-by-name[token.api-name] := token;
//       end for;
//       
//       let modules-by-name = make(<case-insensitive-string-table>);
//       for (module in library.modules)
//          modules-by-name[module.local-name] := module
//       end for;
// 
//       // Process module bindings.
//       for (module in library.modules)
//          let (module-files, remaining-files) =
//                partition(curry(file-in-module?, module.local-name), library-files);
// 
//          when (instance?(module, type-union(<imported-module>, <reexported-module>)))
//             // Error if we have a source record for an imported/reexported module.
//             for (file in module-files)
//                block()
//                   file-in-foreign-module(location: file.module-header.token-src-loc,
//                         name: module.local-name, defn-location: module.source-location);
//                exception (<skip-error-restart>)
//                end block;
//             end for;
//          end when;
// 
//          let module-token = element(module-tokens-by-name, module.local-name, default: #f);
//          module.bindings :=
//                merge-bindings(bindings-from-module(module, module-token, modules-by-name));
//          
//          library-files := remaining-files;
//       end for;
//       
//       // Any files not in a defined module (aside from dylan-user)? Error.
//       let extras = choose(complement(curry(file-in-module?, "dylan-user")),
//                           library-files);
//       for (extra in extras)
//          block()
//             let header = extra.module-header;
//             no-module-for-file(location: header.token-src-loc,
//                   name: header.hdr-value);
//          exception (<skip-error-restart>)
//          end block;
//       end for;
//    end for;
// end method;
// 
// 
// /** Synopsis: Returns "module:" header from interchange file token. **/
// define 
// 
// 
// /**
// Synopsis: Get bindings from module of other library.
// 
// Modules that the library imports or reexports from other libraries have the same
// bindings as the imported or reexported module. Specifically, any bindings that
// the other library's module exports are also exported from the corresponding
// module in this library.
// **/
// define method bindings-from-module
//    (module :: type-union(<imported-module>, <reexported-module>),
//     token :: false-or(<module-definer-token>),
//     all-modules :: <table> /* <string> => <module> */)
// => (bindings :: <sequence> /* of <binding> */)
//    let orig-library = module.used-library;
//    if (instance?(orig-library, <library>))
//       // Get imported/reexported module.
//       let orig-modules = 
//             choose(method (mod :: <module>) => (match?)
//                       case-insensitive-equal?(mod.local-name, module.import-name)
//                    end method, orig-library.modules);
//       when (orig-modules.empty?)
//          no-module-in-foreign-library(location: module.source-location,
//                module-name: module.import-name, library-name: orig-library.local-name)
//       end when;
//       let orig-module = orig-modules.first;
// 
//       // Get bindings from imported/reexported module and duplicate.
//       let orig-bindings =
//             choose(rcurry(instance?, type-union(<exported-binding>, <reexported-binding>)),
//                    orig-module.bindings);
//       map(method (orig-binding :: <binding>) => (new-binding)
//              make(<reexported-binding>, local-name: orig-binding.local-name,
//                   import-name: orig-binding.local-name, used-module: orig-module,
//                   definition: orig-binding.definition)
//           end, orig-bindings);
//    else
//       // No information available for imported/reexported module.
//       #[];
//    end if;
// end method;
// 
// 
// /**
// Synopsis: Get bindings from module definition and top-level definitions.
// **/
// define method bindings-from-module
//    (module :: type-union(<internal-module>, <exported-module>),
//     token :: false-or(<module-definer-token>),
//     all-modules :: <table> /* <string> => <module> */)
// => (bindings :: <sequence> /* of <binding> */)
//    let clauses = token.namespace-clauses;
//    let bindings-per-clause = map(rcurry(bindings-from-clause, module, all-modules),
//                                  clauses);
//    apply(concatenate, #[], bindings-per-clause);
// end method;
// 
// 
// define method bindings-from-clause
//    (clause :: <export-clause-token>, module :: <module>, all-modules :: <table>)
// => (bindings :: <sequence> /* of <binding> */)
//    map(method (name :: <string>) => (binding :: <binding>)
//           make(<exported-binding>, local-name: name,
//                source-location: clause.token-src-loc)
//        end, clause.export-names);
// end method;
// 
// 
// define method bindings-from-clause
//    (clause :: <create-clause-token>, module :: <module>, all-modules :: <table>)
// => (bindings :: <sequence> /* of <binding> */)
//    map(method (name :: <string>) => (binding :: <binding>)
//           make(<exported-binding>, local-name: name,
//                source-location: clause.token-src-loc,
//                definition: make(<deferred-definition>))
//        end, clause.create-names);
// end method;
// 
// 
// define method bindings-from-clause
//    (clause :: <use-clause-token>, module :: <module>, all-modules :: <table>)
// => (bindings :: <sequence> /* of <binding> */)
//    local method binding-has-name? (binding :: <binding>, name :: <string>)
//          => (has-name?)
//             case-insensitive-equal?(binding.local-name, name)
//          end;
//          
//    let used-module = all-modules[clause.used-name];
//    let imp-bindings = difference(used-module.bindings, clause.use-exclusions,
//                                  test: binding-has-name?);
//    let imp-bindings =
//          if (clause.use-imports = #f | clause.use-imports = #"all")
//             imp-bindings
//          else
//             intersection(incl-bindings, clause.use-imports, test: binding-has-name?)
//          end if;
//    
//    
//    #[]
// end method;
