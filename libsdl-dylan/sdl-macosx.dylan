module: sdl
author: Rob Myers
copyright: Copyright (c) 2003 Gwydion Dylan Maintainers
license: LGPL

// From Peter Housel's gtk ffi compatibility in gtk.dylan
// This only works if all of the parameters are pointers

define macro C-callable-wrapper-definer
  { define C-callable-wrapper ?wrapper:name of ?wrapped:name
      ?params:*
    end }
    => { define constant ?wrapper
           = make(<function-pointer>, pointer:
                    callback-entry(%wrapper-callback(?params)
                                     %wrapper-aux(?wrapped(?params))
                                   end)) }
end macro;

define macro %wrapper-callback
  { %wrapper-callback ( ?params ) ?:body end }
    => { callback-method(?params) => (value :: <integer>); ?body end }
params:
  { } => { }
  { parameter ?:name :: ?type:expression ; ... }
    => { ?name :: <raw-pointer>, ... }
end macro;

define macro %wrapper-aux
  { %wrapper-aux ( ?wrapped:name(?params) ) }
    => { ?wrapped(?params) }
params:
  { } => { }
  { parameter ?:name :: ?type:expression ; ... }
    => { make(?type, pointer: ?name), ... }
end macro;

// Globals

define variable *fun* = #f;
define variable *name* = #f;
define variable *args* = #f;
define variable *result* = 0;

// Our callback

define constant sdl-main-callback = callback-method() =>();
  		*result* := *fun*( *name*, *args* );
end;

// Main method: saves the variables, installs the callback, runs SDL

define method SDL-Run( fun :: <function>, name, args )
=> (result :: <integer> )
	*fun* := fun;
	*name* := name;
	*args* := args;

	//ERROR IS HERE: Bus error (core dumped)
	// Set the callback
	c-expr( void: "extern void (*SDL_Dylan_main)();" );
	c-variable-ref( ptr: "SDL_Dylan_main" ) := sdl-main-callback.callback-entry;
	
	// Call the main Cocoa entry point, which eventually calls the callback
	c-expr( void: "extern int application_argc;" );
	c-expr( void: "extern char **application_argv;" );
  call-out( "SDL_Cocoa_main", int:, 
  	ptr: c-variable-ref( ptr: "&application_argc" ), 
  	ptr: c-variable-ref( ptr: "application_argv" ) );
		
		*result*;
end method SDL-Run;
