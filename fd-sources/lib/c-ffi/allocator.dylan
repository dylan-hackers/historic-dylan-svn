module:    c-ffi-implementation
copyright: Copyright (c) 1997-2001 Functional Objects, Inc. All rights reserved.


define inline function default-allocator
    (size :: <integer>) => (m :: <machine-word>);
  primitive-manual-allocate(max(size, 16))
end;


define inline function default-deallocator (pointer :: <machine-word>) => ();
  primitive-manual-free(make(<C-heap-pointer>, address: pointer))
end;
