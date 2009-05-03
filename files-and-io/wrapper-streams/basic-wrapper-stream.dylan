module: basic-wrapper-stream
author: Dustin Voss

/**
Class: <basic-wrapper-stream>
-----------------------------
Synopsis: A <wrapper-stream> with implementations for several methods. Use this
between a wrapper stream and a non-wrapper stream affected by bug #7385.

Note: This class is only necessary because Common Dylan I/O convenience
methods do not call primitive methods on outer-stream. See bug #7385.

Several I/O methods can be written in terms of a simpler subset of methods.
This class provides implementations for the following methods that act as
described by the "Functional Developer System and I/O Reference": [bib]
   - read
   - read-into!
   - write
   - read-to
   - read-through
   - read-to-end
   - skip-through
   - read-line
   - read-line-into!
   - read-text
   - read-text-into!
   - write-line
   - new-line

Those methods use the following "primitive" methods, which should be
implemented on subclasses, or left delegated to the inner stream:
   - read-element
   - peek
   - write-element
   - stream-at-end?

Other methods such as 'unread-element' or 'stream-position' are not used by
methods on this class, but should probably be implemented on subclasses
anyway.

Additionally, the following method is implemented on this class to keep it
from being delegated to the inner stream:
   - close
*/

define open class <basic-wrapper-stream> (<wrapper-stream>)
end class;


define method close (wrapper :: <basic-wrapper-stream>, #key, #all-keys) => ()
end method;


define method read
   (wrapper :: <basic-wrapper-stream>, n :: <integer>,
    #rest keys, #key on-end-of-stream = unsupplied())
=> (elements-or-eof :: <object>)
   let results = make(sequence-type-for-inner-stream(wrapper), size: n);
   block ()
      let read-count = read-into!(wrapper.outer-stream, n, results);
      if (read-count < n)
         copy-sequence(results, end: read-count)
      else
         results
      end if;
   exception (type-union(<end-of-stream-error>, <incomplete-read-error>),
              test: always(on-end-of-stream.supplied?))
      on-end-of-stream;
   end block;
end method;


define method read-into!
   (wrapper :: <basic-wrapper-stream>, n :: <integer>, coll :: <mutable-sequence>,
    #rest keys, #key start :: <integer> = 0, on-end-of-stream = unsupplied())
=> (count-or-eof :: <object>)
   let total-read = 0;
   block ()
      // Would rather use replace-subsequence!, but it doesn't guarantee that
      // coll would be modified.
      for (i from start below min(start + n, coll.size))
         coll[i] := read-element(wrapper);
         total-read := total-read + 1;
      end for;
   exception (eos :: <end-of-stream-error>)
      case
         on-end-of-stream.supplied? =>
            total-read := on-end-of-stream;
         total-read > 0 =>
            error(make(<incomplete-read-error>, stream: wrapper, count: n,
                       sequence: copy-sequence(coll, start: start,
                                               end: start + total-read)));
         otherwise =>
            error(eos);
      end case;
   end block;
   total-read
end method;


define method write
   (wrapper :: <basic-wrapper-stream>, elements :: <sequence>,
    #key start: start-pos :: <integer> = 0,
         end: end-pos :: <integer> = elements.size)
=> ()
   for (i from start-pos below end-pos)
      write-element(wrapper, elements[i])
   end for;
end method;


define method read-to
   (wrapper :: <basic-wrapper-stream>, to-elem :: <object>,
    #key on-end-of-stream = unsupplied(), test = \==)
=> (elements-or-eof :: <object>, found? :: <boolean>)
   // We use read-through instead of coding read-to behavior directly because
   // this function is exactly like read-through but drops the terminator, and
   // I didn't want to screw around with EOS logic.
   let (elements, found?) = read-through(wrapper.outer-stream, to-elem,
                                         test: test, on-end-of-stream: #f);
   case
      elements =>
         let elements = copy-sequence(elements,
               end: if (found?) elements.size - 1 else elements.size end if);
         values(elements, found?);
      on-end-of-stream.supplied? =>
         values(on-end-of-stream, #f);
      otherwise =>
         eos-error(wrapper);
   end case;
end method;


define method read-through
   (wrapper :: <basic-wrapper-stream>, to-elem :: <object>,
    #key on-end-of-stream = unsupplied(), test = \==)
=> (elements-or-eof :: <object>, found? :: <boolean>)
   let elements = make(<stretchy-vector>);
   let elems-type = wrapper.sequence-type-for-inner-stream;
   block ()
      let found? = #f;
      until (found?)
         let elem = read-element(wrapper);
         add!(elements, elem);
         found? := test(elem, to-elem);
      end until;
      values(as(elems-type, elements), #t)
   exception (eos :: <end-of-stream-error>)
      case
         ~elements.empty? =>
            values(as(elems-type, elements), #f);
         on-end-of-stream.supplied? =>
            values(on-end-of-stream, #f);
         otherwise =>
            error(eos);
      end case;
   end block;
end method;


define method read-to-end (wrapper :: <basic-wrapper-stream>)
=> (elements :: <sequence>)
   let elements = make(<stretchy-vector>);
   while (~wrapper.stream-at-end?)
      add!(elements, read-element(wrapper))
   end while;
   as(wrapper.sequence-type-for-inner-stream, elements)
end method;


/// 'test' is called with a stream element as the first argument and 'to-elem'
/// as the second argument.
define method skip-through
   (wrapper :: <basic-wrapper-stream>, to-elem :: <object>,
    #key test = \==)
=> (found? :: <boolean>)
  let found? = #f;
  while (~stream-at-end?(wrapper) & ~found?)
    found? := test(read-element(wrapper), to-elem)
  end;
  found?
end method;


define method read-line
   (wrapper :: <basic-wrapper-stream>, #rest keys,
    #key on-end-of-stream = unsupplied())
=> (string-or-eof :: <object>, newline? :: <boolean>)
   // This method is a lot like read-to. We don't use read-to directly, because
   // we need to know whether the terminator is '\r' or '\n' so we can read the
   // next '\n' if necessary.

   // Read to line ending.
   let (elements, found?) = read-through(wrapper.outer-stream, "\r\n",
                                         test: member?, on-end-of-stream: #f);
   case
      elements =>
         // Remove LF of CRLF.
         when (found? & elements.last == '\r' &
               peek(wrapper, on-end-of-stream: #f) == '\n')
            read-element(wrapper);
         end when;

         // Make into <string>.
         let string-type = wrapper.string-type-for-inner-stream;
         let elements = copy-sequence(elements,
               end: if (found?) elements.size - 1 else elements.size end if);
         values(as(string-type, elements), found?);

      on-end-of-stream.supplied? =>
         values(on-end-of-stream, #f);

      otherwise =>
         eos-error(wrapper);
   end case;
end method;


define method read-line-into!
   (wrapper :: <basic-wrapper-stream>, string :: <string>,
    #key start :: <integer> = 0, grow? :: <boolean> = #f,
         on-end-of-stream = unsupplied())
=> (string-or-eof :: <object>, newline? :: <boolean>)
   let (line, found?) = read-line(wrapper.outer-stream, on-end-of-stream: #f);
   case
      line =>
         // Grow dest string if necessary.
         let size-needed = start + line.size;
         if (string.size < size-needed)
            if (grow?)
               let new-string = make(string.object-class, size: size-needed);
               string := replace-subsequence!(new-string, string, end: string.size);
            else
               error("String not large enough for input");
            end if;
         end if;

         // Copy into dest string. Would rather use replace-subsequence!, but
         // that doesn't guarantee that string itself is altered.
         for (i from start, c in line)
            string[i] := c;
         end for;                            

         values(string, found?);

      on-end-of-stream.supplied? =>
         values(on-end-of-stream, #f);

      otherwise =>
         eos-error(wrapper);
   end case;
end method;


define method read-text
   (wrapper :: <basic-wrapper-stream>, n :: <integer>,
    #rest keys, #key on-end-of-stream = unsupplied())
=> (elements-or-eof :: <object>)
   let string-type = wrapper.string-type-for-inner-stream;
   let results = make(string-type, size: n);
   let read-count = read-text-into!(wrapper.outer-stream, n, results,
                                    on-end-of-stream: #f);
   case
      read-count =>
         if (read-count < n)
            copy-sequence(results, end: read-count)
         else
            results
         end if;
      on-end-of-stream.supplied? =>
         on-end-of-stream;
      otherwise =>
         eos-error(wrapper);
   end case;
end method;


define method read-text-into!
   (wrapper :: <basic-wrapper-stream>, n :: <integer>, string :: <string>,
    #rest keys, #key start :: <integer> = 0, on-end-of-stream = unsupplied())
=> (count-or-eof :: <object>)
   let total-read = 0;
   block ()
      for (i from start below min(start + n, string.size))
         let elem = read-element(wrapper);
         if (elem == '\r')
            elem := '\n';
            if (peek(wrapper, on-end-of-stream: #f) == '\n')
               read-element(wrapper);
            end if;
         end if;
         string[i] := elem;
         total-read := total-read + 1;
      end for;
      total-read;
   exception (eos :: <end-of-stream-error>)
      case
         total-read > 0 =>
            total-read;
         on-end-of-stream.supplied? =>
            on-end-of-stream;
         otherwise =>
            error(eos);
      end case;
   end block;
end method;


define method write-line
   (wrapper :: <basic-wrapper-stream>, elements :: <string>,
    #key start: start-pos = 0, end: end-pos = elements.size)
=> ()
   write(wrapper.outer-stream, elements, start: start-pos, end: end-pos);
   new-line(wrapper.outer-stream)
end method;


define method new-line (wrapper :: <basic-wrapper-stream>) => ()
   write-element(wrapper.outer-stream, '\n');
end method;


/// Synopsis: Returns sequence type appropriate for inner stream's element type.
define method sequence-type-for-inner-stream
   (wrapper :: <basic-wrapper-stream>)
=> (sequence-type :: <type>)
   select (wrapper.inner-stream.stream-element-type by subtype?)
      <byte-character> => <byte-string>;
      <character> => <string>;
      otherwise => <vector>;
   end select
end method;


/// Synopsis: Returns string type appropriate for inner stream's element type.
/// Conditions: Signals error if element type isn't allowed in a string.
define method string-type-for-inner-stream
   (wrapper :: <basic-wrapper-stream>)
=> (sequence-type :: <type>)
   select (wrapper.inner-stream.stream-element-type by subtype?)
      <byte-character> => <byte-string>;
      <character> => <string>;
      otherwise =>
         error("Inner stream element type is not subtype of <character>.");
   end select
end method;


define inline function eos-error (wrapper :: <basic-wrapper-stream>) => ()
   error(make(<end-of-stream-error>, stream: wrapper))
end function;
