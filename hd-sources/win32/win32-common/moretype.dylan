Module:    Win32-common
Synopsis:  Additional declarations to be loaded after the automatically
	   converted files.
Copyright: 1996, 1997, 1998 Harlequin Group plc.  All rights reserved.
Version:   $HopeName: D-lib-win32-common!moretype.dylan(D-kan.3) $
	   $Date: 1999/10/16 19:36:40 $

// Macros in "winuser.h":

define function MAKEINTRESOURCE( n :: <integer> )
	=> value :: <LPTSTR>;
  make(<LPTSTR>, address: n)
end MAKEINTRESOURCE;


// In Win32, RECT and RECTL, although structurally equivalent, are distinct
// types for the sake of source code compatibility with Win16.  But since we
// have no intention of ever supporting Win16 here, it is simpler to
// unify them. 
define constant <RECTL> = <RECT>;
define constant <PRECTL> = <PRECT>;
define constant <LPRECTL> = <LPRECT>;
define constant <LPCRECTL> = <LPCRECT>;


// Null constants for some commonly used pointer types

define constant $NULL-HANDLE :: <HANDLE> = null-pointer( <HANDLE> );
define constant $NULL-HWND :: <HWND> = null-pointer( <HWND> );
define constant $NULL-RECT :: <LPRECT> = null-pointer( <LPRECT> );
define constant $NULL-POINT :: <LPPOINT> = null-pointer( <LPPOINT> );
define constant $NULL-VOID :: <C-void*> = null-pointer( <C-void*> );
define constant $NULL-string :: <C-string> = null-pointer( <C-string> );
define constant $NULL-HDC :: <HDC> = null-pointer(<HDC>);
define constant $NULL-HMENU :: <HMENU> = null-pointer(<HMENU>);
define constant $NULL-HINSTANCE :: <HINSTANCE> = null-pointer(<HINSTANCE>);

// convenience functions

define method as (class :: subclass(<HANDLE>), n :: <integer>)
	=> value :: <HANDLE>;
  make(class, address: n)
end;

define method as (class :: subclass(<HANDLE>), n :: <machine-word>)
	=> value :: <HANDLE>;
  make(class, address: n)
end;

define method as (class == <integer>, h :: <HANDLE>)
	=> value :: <integer>;
  as(<integer>, pointer-address(h))
end;

define inline method as (class == <machine-word>, h :: <HANDLE>)
	=> value :: <machine-word>;
  pointer-address(h)
end;

define sealed method null-handle ( class :: subclass(<HANDLE>) )
	=> value :: <HANDLE>;
  null-pointer(class)
end;

define inline sealed method null-handle? ( object :: <HANDLE> )
	=> value :: <boolean>;
  null-pointer?(object)
end;

// Handles are represented as pointers, but they never have space
// allocated by `make', so it would not be meaningful to call `destroy' on one.
define method destroy ( h :: <HANDLE>, #key ) => ();
  error("destroy not valid on handle %=", h);
end;

// Seal for efficiency.
define sealed domain make (subclass(<HANDLE>));
define sealed domain initialize (<HANDLE>);

// ----

// The type designator <BOOLEAN-BYTE> corresponds to the C type BOOLEAN,
// renamed to avoid conflicting with the Dylan type <boolean>.

define inline-only function import-boolean
    (value :: <integer>) => (b :: <boolean>) 
  ~ zero?(value)
end;

define inline-only function export-boolean
    (value :: <boolean>) => (value :: <integer>)
  if (value) 1 else 0 end if
end;

define C-mapped-subtype <BOOLEAN-BYTE> (<C-BYTE>)
  map <boolean>,
    import-function: import-boolean,
    export-function: export-boolean;
  // pointer-type <PBOOLEAN>; // defined in "winnt.h" but never used.
end;

/* // not allowed because of sealing
define inline method c-type-cast
    (class == <BOOLEAN-BYTE>, value :: <object>) => (true? :: <boolean>)
  c-type-cast(<boolean>, value)
end method c-type-cast;
*/

define inline-only function import-wchar 
    (value :: <integer>) => (char :: <character>)
  as(<character>, value)
end;

define inline-only function export-wchar
    (value :: <character>) => (i :: <integer>)
  as(<integer>, value)
end;

define C-mapped-subtype <WCHAR> (<C-unsigned-short>)
  map <character>,
    import-function: import-wchar,
    export-function: export-wchar;
end;

/* // not allowed because of sealing
define inline method c-type-cast
    (class == <WCHAR>, value :: <object>) => (char :: <character>)
  c-type-cast(<character>, value)
end method c-type-cast;
*/

// Temporary for backward compatibility
define inline function %free(pointer :: <C-pointer>) => ();
  destroy(pointer);
  values()
end;

