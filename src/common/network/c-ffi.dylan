Module:       sockets-internals
Author:       Peter S. Housel
Synopsis:     C-FFI emulation
Copyright:    Original Code is Copyright (c) 2003 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual License: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method null-pointer
    (pointer-designator-class :: subclass(<statically-typed-pointer>))
 => (null :: <statically-typed-pointer>);
  as(pointer-designator-class, 0);
end method;

define method null-pointer?
    (null :: <statically-typed-pointer>)
 => (null? :: <boolean>);
  as(<integer>, null) == 0;
end method;

define method pointer-address
    (c-pointer :: <statically-typed-pointer>)
 => (address :: <integer>)
  as(<integer>, c-pointer.raw-value);
end method;

define method pointer-cast
    (pointer-designator-class :: subclass(<statically-typed-pointer>),
     c-pointer :: <statically-typed-pointer>)
 => (new-c-pointer :: <statically-typed-pointer>);
  make(pointer-designator-class, pointer: c-pointer.raw-value);
end method;


