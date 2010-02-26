module: dylan-user
author: Dustin Voss


/**
Module: sequence-stream
-----------------------
Synopsis: A replacement for the normal <sequence-stream> classes.

This module is motivated by various issues with <sequence-stream> in Common
Dylan. It provides implementations for stream methods that act as described by
the "Functional Developer System and I/O Reference". [bib]

If possible, these classes will use the 'contents:' initialization argument as
the backing store; if not, they will use a copy. The backing store will be a
general instance of <vector> and will be a <stretchy-vector> if and only if the
stream is writable. This differs from the normal <sequence-stream> classes, in
which using a <stretchy-vector> as the 'contents:' initialization argument
guarantees that all changes to the <stretchy-vector> are reflected in the
<sequence-stream> and vice versa.

When a writable <sequence-stream> grows its backing store, it will use the
filler element specified by the 'fill:' initialization argument.

This class supports a 'position-offset:' initialization argument that sets the
effective stream position at the first element of the stream. This allows this
class to act as a subset of another, larger stream. The initialization argument
defaults to 0.
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
