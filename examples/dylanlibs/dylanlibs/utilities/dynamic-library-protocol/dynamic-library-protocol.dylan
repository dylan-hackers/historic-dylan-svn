Module:    dynamic-library-protocol
Synopsis:  Protocol for supporting loading of dynamic Dylan libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define open class <dynamic-library> (<object>)
  // Defines function called when the library is loaded.
  constant slot dynamic-library-load-callback :: false-or(<function>) = #f, init-keyword: load-callback:;

  // Defines function called when the library is unloaded
  constant slot dynamic-library-unload-callback :: false-or(<function>) = #f, init-keyword: unload-callback:;
end class <dynamic-library>;

// A description of the library
define open generic dynamic-library-description( lib :: <dynamic-library> ) => (s :: <string>);

define constant $loaded-libraries = make(<table>);
define thread variable *last-loaded-library* :: false-or(<dynamic-library>) = #f;

// A library that can be dynamically loaded calls this method
// on initialisation to let the Dylan program know how to 
// initialize and uninitialize the library when loading or unloading
// it.
define method register-dynamic-library(lib :: <dynamic-library>) => ()
  *last-loaded-library* := lib;  
end method register-dynamic-library;

// This method is called by clients that want to dynamically load
// some functionality. It results in the DLL given in PATHNAME to
// be loaded. Any particular initialisation required by that library
// is then performed.
//
// TODO: Error checking on LoadLibrary
// TODO: Error checking on name
define method load-dynamic-library(pathname :: <string>) => (lib :: <dynamic-library>)
  let module = LoadLibrary(pathname);
  let lib = *last-loaded-library*;
  $loaded-libraries[lib] := module;
  when(lib.dynamic-library-load-callback)
    lib.dynamic-library-load-callback();
  end when;
  lib;
end method load-dynamic-library;

// The method will result in an already loaded dynamic module
// being unloaded and its registered unintialize callback being
// called.
//
// TODO: Error checking that LIB exists
// TODO: Error checking on FreeLibrary
define method unload-dynamic-library( lib :: <dynamic-library> ) => ()
  when(lib.dynamic-library-unload-callback)
    lib.dynamic-library-unload-callback();
  end when;

  let module = $loaded-libraries[lib];  
  remove-key!($loaded-libraries, lib);
  let value = FreeLibrary(module);
end method unload-dynamic-library;

// Returns a sequence containing all dynamic libraries.
define method dynamic-libraries () => (libs :: <sequence>)
  let libs = make(<stretchy-vector>);
  for(value keyed-by key in $loaded-libraries)
    libs := add!(libs, key)
  end for;
  libs;
end method dynamic-libraries;
