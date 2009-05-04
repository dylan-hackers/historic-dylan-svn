module: dylan-user
author: Dustin Voss


/**
Module: sequence-stream
-----------------------
Synopsis: A replacement for the normal <sequence-stream> classes.

This module is motivated by various issues with <sequence-stream> in Common
Dylan. It provides implementations for stream methods that act as described by
the "Functional Developer System and I/O Reference". [bib]

These classes use the 'contents:' initialization argument as a backing store as
much as possible, but may use a new backing store if necessary. You cannot
expect changes to the initialization <sequence> to be reflected in the
<sequence-stream> after the <sequence-stream> is created, and you cannot expect
changes to the <sequence-stream> to "not" [em] be reflected in the
initialization <sequence>. This differs from the normal <sequence-stream>
classes, in which using a <stretchy-vector> as the 'contents:' initialization
argument guarantees that all changes to the <stretchy-vector> are reflected in
the <sequence-stream> and vice versa.

When a writable <sequence-stream> grows its backing store, it will use the
filler element specified by the 'fill:' initialization argument.
**/


define library sequence-stream
   use common-dylan;
   use io, import: { streams };

   export sequence-stream;
end library;


define module sequence-stream
   // from common-dylan
   use dylan;
   use common-extensions;
   // from io
   use streams, prefix: "ext-";
   use streams, import:
         { <stream>, <basic-stream>, <positionable-stream>, <stream-error>,
           <stream-closed-error>, <end-of-stream-error>, <stream-not-readable>,
           <stream-not-writable>, <incomplete-read-error>, <byte-character>,
           outer-stream };
   
   export <sequence-stream>, <string-stream>, <byte-string-stream>;
end module;
