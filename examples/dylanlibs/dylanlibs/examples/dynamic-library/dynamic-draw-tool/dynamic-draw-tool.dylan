Module:    dynamic-draw-tool
Synopsis:  A protocol for tools that can be used in the dynamic draw program
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define open class <dynamic-draw-tool> (<object>)
end class <dynamic-draw-tool>;

// Perform some unspecified action on the given SURFACE starting
// with the coordinates identified by X and Y.
define open generic do-tool(tool :: <dynamic-draw-tool>, surface, x, y) => ();

// Return a description of the tool
define open generic tool-description(tool :: <dynamic-draw-tool>) => (d :: <string>);

// Don't use a <stretchy-vector> here. When doing a remove! from 
// a <stretchy-vector>, the object being removed can sometimes still
// be stored, with a fill-pointer type variable for the vector being
// adjusted. This causes errors after an unload.
define variable *tools* = make(<list>);

define method register-tool(tool :: <dynamic-draw-tool>) => ()
  *tools* := add!(*tools*, tool);
end method register-tool;

define method unregister-tool(tool :: <dynamic-draw-tool>) => ()
  *tools* := remove!(*tools*, tool);
end method unregister-tool;

define method all-tools() => (tools :: <sequence>)
  copy-sequence(*tools*);
end method all-tools;
