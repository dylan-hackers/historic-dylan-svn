module: replacing-stream
author: Dustin Voss


/**
Class: <replacing-stream>
-------------------------
Synopsis: A stream wrapper that transparently substitutes other elements for
parts of the underlying stream.

This class replaces subsequences from the inner stream with other subsequences
of possibly different length. Applications include detabbing (in which tab
characters are replaced by a series of spaces), normalizing line separators
(in which line separators are replaced by a canonical separator), redacting
output (in which certain passages are replaced by blackout characters or
removed), or expanding tokens, abbreviations, or shorthand.

To replace a segment of the inner stream, call 'add-replacement-contents',
passing the replacement sequence and the start and end positions of the stream
that should be replaced. If the start and end positions are the same, the
replacement sequence is inserted. If the replacement sequence is empty, the
inner stream segment is removed.

I say "inserted," "removed," and "replaced," but the inner stream is not
actually altered by these operations.

The borders of replaced segments cannot be altered, nor can additional
replacement segments be inserted between existing replacements. The "content"
[em] of a replacement segment can be altered, however, by writing to it --
assuming the replacement sequence is mutable, of course.
*/

define open class <replacing-stream> (<basic-wrapper-stream>, <positionable-stream>)

   // Elements of 'inner-stream-limits', 'segment-contents', and 'segment-limits'
   // correspond. 'inner-stream-limits' are the inner stream positions just past
   // the end of each segment, 'segment-contents' are the contents of each
   // segment or #f if the inner stream's contents should be used, and 
   // 'segment-limits' are the wrapper stream positions just past the end of
   // each segment, considering all preceding replacement contents. Last
   // segment is always a replacement; stream contents beyond that are inner
   // stream contents. There is always at least one element in these arrays;
   // they are prepopulated with an empty segment.
   
   // Inner stream positions could be <stream-position> or <integer>, but I
   // convert to <integer> because I often need to compute sizes and differences
   // and I need <integer> for that.
   slot inner-stream-limits =
         make(limited(<stretchy-vector>, of: <integer>), fill: 0);
   slot segment-contents =
         make(limited(<stretchy-vector>, of: false-or(<sequence>)), fill: #f);
   slot segment-limits =
         make(limited(<stretchy-vector>, of: <integer>), fill: 0);
   slot current-segment :: <integer> = 1;
   slot current-offset :: <integer> = 0;
   keyword inner-stream:, type: <positionable-stream>;
end class;


/**
Method: add-replacement-contents
--------------------------------
Synopsis: Replaces inner stream content of a <replacing-stream>.

'Start' and 'end' are 'wrapper' stream positions but correspond to inner
stream positions. The inner stream is grown if necessary to ensure these
positions exist. The inner stream is not grown if 'start' and 'end'
correspond to the inner stream's end-of-stream position. If the inner stream
cannot be grown, an error is signaled.

The current 'wrapper' stream position is left effectively unchanged (pointing
to the same current element in the inner stream or replacement content) if
possible.

- If the current element was not replaced or removed, the stream position may
  be altered but will still point to the same element.
- If the current element from the inner stream was replaced by a new element,
  the stream position will point to the replacement element.
- If the current element from the inner stream was removed altogether, the
  stream position will point to the next element still present in the stream.

Arguments:

   wrapper     - An instance of <replacing-stream>.

   replacement - An instance of <sequence>. The elements of this sequence
                 replace the elements of the inner stream from 'start' up to
                 but not including 'end' when reading from or writing to
                 'wrapper'. 'Replacement' cannot be a <stretchy-collection>.

   start:      - An instance of <integer> representing a position within
                 'wrapper'. Defaults to the current stream position.

   end:        - An instance of <integer> representing a position within
                 'wrapper'. Defaults to the location corresponding to the end
                 of 'replacement', replacing inner stream elements with
                 'replacement' elements on a one-to-one basis.

Values:

   start-pos - An instance of <integer> identical to 'start'.

   end-pos   - An instance of <integer> representing the new stream position
               corresponding to 'end': `start + replacement.size`.
*/

define method add-replacement-contents
   (wrapper :: <replacing-stream>, replacement :: <sequence>,
    #key start: start-pos :: <integer> = wrapper.stream-position,
         end: end-pos :: <integer> = start-pos + replacement.size)
=> (start-pos :: <integer>, end-pos :: <integer>)

   let inner-limits = wrapper.inner-stream-limits;
   let seg-contents = wrapper.segment-contents;
   let seg-limits = wrapper.segment-limits;
   let inner = wrapper.inner-stream;
   let orig-pos = wrapper.stream-position;
   
   local method acceptable-replacement-elements? () => (acceptable? :: <boolean>)
            every?(acceptable-element?, replacement)
         end method,
         method first-unacceptable-replacement-element () => (elem)
            replacement[find-key(replacement, unacceptable-element?)]
         end method,
         method acceptable-element? (elem) => (acceptable? :: <boolean>)
            instance?(elem, inner.stream-element-type)
         end method,
         method unacceptable-element? (elem) => (acceptable? :: <boolean>)
            ~elem.acceptable-element?
         end method;

   assert(start-pos <= end-pos,
         "Start position %= comes after end position %=", start-pos, end-pos);
   assert(start-pos >= seg-limits.last,
         "Replacements follow position %=", start-pos);
   assert(~instance?(replacement, <stretchy-collection>),
         "Replacement cannot be stretchy");
   assert(inner.stream-element-type = <object> | acceptable-replacement-elements?(),
         "Replacement element %= not of inner stream element type %=",
         first-unacceptable-replacement-element(), inner.stream-element-type);
   
   if (end-pos - start-pos = 0 & replacement.size = 0)
      // If replacement is empty and segment to replace is empty, might as
      // well make no change at all.
      values(start-pos, end-pos)
   else
      // If new segment isn't adjacent to last segment, create inner stream
      // segment to cover the gap. Covering segment runs from last segment to
      // start of new segment, and the inner stream is grown if necessary.
      if (start-pos ~= seg-limits.last)
         let inner-end-pos = adjust-stream-position-below-end(inner,
               origin: inner-limits.last, delta: start-pos - seg-limits.last);
         inner-limits := add!(inner-limits, inner-end-pos);
         seg-limits := add!(seg-limits, start-pos);
         seg-contents := add!(seg-contents, #f);
      end if;
   
      // Add new segment. We want the inner stream increased to just before
      // end-pos, and we want the (eos) stream-position of end-pos itself so we
      // can add it to inner-limits.
      let inner-end-pos = adjust-stream-position-below-end(inner,
            origin: inner-limits.last, delta: end-pos - start-pos);
      wrapper.inner-stream-limits := add!(inner-limits, inner-end-pos);
      wrapper.segment-limits := add!(seg-limits, start-pos + replacement.size);
      wrapper.segment-contents := add!(seg-contents, replacement);
   
      // Adjust current stream position considering new segment.
      case
         orig-pos >= end-pos =>
            // Position lies beyond replaced segment.
            let net-size-change = replacement.size - (end-pos - start-pos);
            orig-pos := orig-pos + net-size-change;
         orig-pos >= start-pos =>
            // Position lies within replaced segment.
            let pos-within-replacement = orig-pos - start-pos;
            if (pos-within-replacement > replacement.size)
               orig-pos := seg-limits.last
            end if;
      end case;
      wrapper.stream-position := orig-pos;
      values(start-pos, seg-limits.last);
   end if
end method;
   

/**
Method: inner-stream-position
-----------------------------
Synopsis: Inner stream position corresponding to given stream position.

Each 'wrapper' stream element may come from the inner stream or from replacement
content. If the element comes from an the inner stream element, or it comes from
replacement content but there is a one-to-one correspondence with an inner
stream element, this method returns the position of the element in the inner
stream. Otherwise, if there is not a one-to-one correspondence with an inner
stream element, extra elements are considered to have been removed from or added
to the beginning of the replacement content and the remaining inner stream
elements are the corresponding elements. This method also returns a flag
indicating if the position was inserted into the stream and does not correspond
to a real inner stream position.

In the following examples, stream elements ("Elt") are shown with their
corresponding stream positions ("Pos"), inner stream positions ("InP") and
insertion flag values ("Ins"). Only the one's place is shown.

- The inner stream's "?" characters are replaced by "x" characters.
  : Elt  VM-38?? => VM-38xx
  : Pos  0123456    0123456
  : InP             0123456
  : Ins             fffffff

- The "and/" in the inner stream's "and/or" is deleted.
  : Elt  This and/or that => This or that
  : Pos  0123456789012345    012345678901
  : InP                      012349012345
  : Ins                      ffffffffffff

- The phrase "et cetera" is replaced by "etc."
  : Elt  Red, et cetera, => Red, etc.,
  : Pos  012345678901234    0123456789
  : InP                     0123401234
  : Ins                     ffffffffff

- The "-" is replaced by the word "through."
  : Elt  A-Z => A through Z
  : Pos  012    01234567890
  : InP         01111111112
  : Ins         fttttttttff

- The text "[sic]" is inserted immediately after "ain't" in "ain't nothing."
  : Elt  ain't nothing => ain't [sic] nothing
  : Pos  0123456789012    0123456789012345678
  : InP                   0123455555556789012
  : Ins                   fffffttttttffffffff

- The "Mr." in the inner stream's "Mr./Mrs." is replaced by "Mister."
  : Elt  Mr./Mrs. => Mister/Mrs.
  : Pos  01234567    01234567890
  : InP              00001234567
  : Ins              tttffffffff

- The inner stream's text is double-parenthesized. Note that the inner stream
  positions of the last characters of the wrapper stream are end-of-stream
  positions.
  : Elt  Budget => ((Budget))
  : Pos  012345 => 0123456789
  : InP            0001234566
  : Ins            ttfffffftt
   
Arguments:

   wrapper - An instance of <replacing-stream>.
           
   at:     - An instance of <integer> representing a position in 'wrapper'.
             Defaults to 'wrapper''s current stream position.

Values:

   inner-pos - An instance of <integer> representing a position in or just
               beyond the end of 'wrapper''s inner stream.
             
   inserted? - An instance of <boolean>, indicating if the element at the 'at:'
               position was inserted into the inner stream before the 'inner-pos'
               position.
*/

define method inner-stream-position
   (wrapper :: <replacing-stream>, #key at: at-pos :: false-or(<integer>) = #f)
=> (inner-pos :: <integer>, inserted? :: <boolean>)
   let inner = wrapper.inner-stream;
   let inner-limits = wrapper.inner-stream-limits;
   let seg-count = inner-limits.size;
   let (seg, off) =
         if (at-pos)
            segment-from-position(wrapper, at-pos);
         else
            values(wrapper.current-segment, wrapper.current-offset);
         end if;

   let inner-seg-start = inner-limits[seg - 1];
   if (seg < seg-count)
      // Return inner stream position corresponding to start of segment plus
      // whatever one-to-one correspondence we have between inner stream content
      // and segment content past the segment's inserted content.
      let inner-seg-size = inner-limits[seg] - inner-seg-start;
      let inserted-extent = segment-size(wrapper, seg) - inner-seg-size;
      let inserted? = off < inserted-extent;
      let inner-off = max(0, off - inserted-extent);
      values(inner-seg-start + inner-off, inserted?)
   else
      // Return corresponding inner stream position at end of inner stream.
      values(inner-seg-start + off, #f);
   end if;
end method;


define method read-element
   (wrapper :: <replacing-stream>, #rest keys, #key on-end-of-stream)
=> (element-or-eof :: <object>)
   let seg-contents = wrapper.segment-contents;
   let seg-count = seg-contents.size;
   let cur-seg = wrapper.current-segment;
   let cur-off = wrapper.current-offset;
   let inner = wrapper.inner-stream;

   let elem = #f;
   if (cur-seg < seg-count & seg-contents[cur-seg])
      elem := seg-contents[cur-seg][cur-off];
      adjust/grow-stream-position(wrapper, +1, grow: #f);
   else
      set-inner-stream-position(wrapper);
      if (inner.stream-at-end?)
         elem := apply(read-element, inner, keys);
      else
         elem := read-element(inner);
         adjust/grow-stream-position(wrapper, +1, grow: #f);
      end if;
   end if;

   elem
end method;


define method unread-element
   (wrapper :: <replacing-stream>, expected-elem :: <object>)
=> (elem :: <object>)
   let seg-contents = wrapper.segment-contents;
   let seg-count = seg-contents.size;
   let inner = wrapper.inner-stream;

   adjust/grow-stream-position(wrapper, -1, grow: #f);
   let cur-seg = wrapper.current-segment;
   let cur-off = wrapper.current-offset;

   let elem =
         if (cur-seg < seg-count & seg-contents[cur-seg])
            seg-contents[cur-seg][cur-off];
         else
            set-inner-stream-position(wrapper);
            peek(inner);
         end if;
   
   if (elem ~== expected-elem)
      error("Unread to %=, expecting %=", elem, expected-elem);
   end if;
   elem
end method;


define method peek
   (wrapper :: <replacing-stream>, #rest keys, #key on-end-of-stream)
=> (elem :: <object>)
   let seg-contents = wrapper.segment-contents;
   let cur-seg = wrapper.current-segment;
   let cur-off = wrapper.current-offset;
   let seg-count = seg-contents.size;
   let inner = wrapper.inner-stream;

   let elem = #f;
   if (cur-seg < seg-count & seg-contents[cur-seg])
      elem := seg-contents[cur-seg][cur-off];
   else
      set-inner-stream-position(wrapper);
      elem := apply(peek, inner, keys);
   end if;

   elem
end method;


define method stream-input-available? (wrapper :: <replacing-stream>)
=> (available? :: <boolean>)
   wrapper.current-segment < wrapper.segment-limits.size
      | wrapper.inner-stream.stream-input-available?
end method;


define method write-element (wrapper :: <replacing-stream>, elem :: <object>)
=> ()
   let seg-contents = wrapper.segment-contents;
   let cur-seg = wrapper.current-segment;
   let cur-off = wrapper.current-offset;
   let seg-count = seg-contents.size;
   let inner = wrapper.inner-stream;

   let elem = #f;
   if (cur-seg < seg-count & seg-contents[cur-seg])
      seg-contents[cur-seg][cur-off] := elem;
      adjust/grow-stream-position(wrapper, +1, grow: #f);
   else
      set-inner-stream-position(wrapper);
      write-element(inner, elem);
      adjust/grow-stream-position(wrapper, +1, grow: #f);
   end if;
end method;


define method stream-at-end? (wrapper :: <replacing-stream>)
=> (at-end? :: <boolean>)
   if (wrapper.current-segment >= wrapper.segment-limits.size)
      set-inner-stream-position(wrapper);
      wrapper.inner-stream.stream-at-end?
   end if
end method;


define method stream-position (wrapper :: <replacing-stream>)
=> (position :: <integer>)
   position-from-segment(wrapper, wrapper.current-segment, wrapper.current-offset)
end method;


define method stream-position-setter
   (position :: <integer>, wrapper :: <replacing-stream>)
=> (position :: <integer>)
   let (new-seg, new-off) = segment-from-position(wrapper, position);
   wrapper.current-segment := new-seg;
   wrapper.current-offset := new-off;
   position
end method;


define method stream-position-setter
   (position == #"start", wrapper :: <replacing-stream>)
=> (position :: <integer>)
   adjust/grow-stream-position(wrapper, 0, from: #"start", grow: #f);
end method;


define method stream-position-setter
   (position == #"end", wrapper :: <replacing-stream>)
=> (position :: <integer>)
   adjust/grow-stream-position(wrapper, 0, from: #"end", grow: #f);
end method;


define method adjust-stream-position
   (wrapper :: <replacing-stream>, delta :: <integer>,
    #key from :: one-of(#"current", #"start", #"end") = #"current")
=> (new-position :: <integer>)
   adjust/grow-stream-position(wrapper, delta, from: from)
end method;


define method adjust/grow-stream-position
   (wrapper :: <replacing-stream>, delta :: <integer>,
    #key from :: one-of(#"current", #"start", #"end") = #"current",
         grow: grow? :: <boolean> = #t)
=> (new-position :: <integer>)
   let inner-limits = wrapper.inner-stream-limits;
   let seg-contents = wrapper.segment-contents;
   let seg-limits = wrapper.segment-limits;
   let seg-count = seg-limits.size;
   let inner = wrapper.inner-stream;

   let (cur-seg, cur-off) =
         select (from)
            #"current" =>
               values(wrapper.current-segment, wrapper.current-offset);
            #"start" =>
               values(1, 0);
            #"end" =>
               values(seg-count, inner.stream-size - inner-limits.last);
         end select;

   if (delta = 0)
      if (grow?)
         // Even if delta was 0, adjust the inner stream position in case we are
         // at the end of the inner stream and need to add a position per
         // adjust-stream-position contract.
         set-inner-stream-position(wrapper);
         adjust-stream-position(inner, 0);
      end if;
   else
      while (delta ~= 0)
         if (delta < 0)
            if (cur-seg > 0)
               // Move no further than just before segment.
               let seg-size-left = cur-off;
               let adj-size = min(seg-size-left + 1, abs(delta));
               cur-off := cur-off - adj-size;
            
               // While before the start of the segment, move to prev segment.
               // This also skips empty segments.
               while (cur-off < 0 & cur-seg > 0)
                  cur-seg := cur-seg - 1;
                  cur-off := segment-size(wrapper, cur-seg) - 1;
               end while;
            
               delta := delta + adj-size; // Delta < 0, so decrease it by adding.
            else
               // This branch handles case where we move to before any wrapper
               // stream content. Just move to start.
               delta := 0;
               cur-seg := 1;
               cur-off := 0;
            end if;
         elseif (delta > 0)
            if (cur-seg < seg-count)
               // Move no further than just past segment.
               let seg-size-left = segment-size(wrapper, cur-seg) - cur-off;
               let adj-size = min(seg-size-left, delta);
               cur-off := cur-off + adj-size;
            
               // While past end of the segment, move to next segment. This also
               // skips empty segments.
               while (cur-seg < seg-count & cur-off >= segment-size(wrapper, cur-seg))
                  cur-seg := cur-seg + 1;
                  cur-off := 0;
               end while;

               delta := delta - adj-size;
            else
               // After end of segments, can just delegate to inner stream.
               let inner-pos = inner-limits.last + cur-off;
               let pos =
                     if (grow?)
                        inner.stream-position := inner-pos;
                        adjust-stream-position(inner, delta);
                     else
                        adjust-stream-position-below-end(inner,
                              origin: inner-pos, delta: delta);
                     end if;
               cur-off := pos - inner-limits.last;
               delta := 0;
            end if;
         end if;
      end while;
   end if;
   
   // Even if delta was 0, ensure cur-seg/cur-off is a valid position, i.e.,
   // cur-seg is not an empty segment and cur-off is not past the end of the
   // segment.
   while (cur-seg < seg-count & cur-off >= segment-size(wrapper, cur-seg))
      cur-off := cur-off - segment-size(wrapper, cur-seg);
      cur-seg := cur-seg + 1;
   end while;
   
   wrapper.current-segment := cur-seg;
   wrapper.current-offset := cur-off;
   position-from-segment(wrapper, cur-seg, cur-off)
end method;


define method stream-size (wrapper :: <replacing-stream>)
=> (sz :: <integer>)
   wrapper.inner-stream.stream-position := #"end";
   let last-seg-start = wrapper.inner-stream-limits.last;
   let last-seg-end = as(<integer>, wrapper.inner-stream.stream-position);
   wrapper.segment-limits.last + (last-seg-end - last-seg-start)
end method;


define inline method stream-limit (wrapper :: <replacing-stream>)
=> (limit :: <integer>)
   wrapper.stream-size
end method;


/// This method does not clear the contents of the inner stream.
define method stream-contents
   (wrapper :: <replacing-stream>, #key clear-contents? :: <boolean> = #t)
=> (contents :: <sequence>)
   stream-contents-as(wrapper.sequence-type-for-inner-stream, wrapper,
                      clear-contents?: clear-contents?)
end method;


/// This method does not clear the contents of the inner stream.
define method stream-contents-as
   (type :: subclass(<sequence>), wrapper :: <replacing-stream>,
    #key clear-contents? :: <boolean> = #t)
=> (contents :: <sequence>)
   let inner = wrapper.inner-stream;
   let inner-limits = wrapper.inner-stream-limits;
   let seg-contents = wrapper.segment-contents;
   let contents-sequences = make(<stretchy-vector>);

   for (content in seg-contents, seg-num from 0)
      if (content)
         // Get content from replacement sequence.
         contents-sequences := add!(contents-sequences, copy-sequence(content));
      else
         // Get content from inner stream.
         let cont-start = inner-limits[max(seg-num - 1, 0)];
         let cont-end = inner-limits[seg-num];
         inner.stream-position := cont-start;
         let content = read(inner, cont-end - cont-start);
         contents-sequences := add!(contents-sequences, content);
      end if;
   end for;

   // Get trailing content from inner stream.
   inner.stream-position := inner-limits.last;
   contents-sequences := add!(contents-sequences, read-to-end(inner));

   if (clear-contents?)
      clear-contents(wrapper);
   end if;
   
   reduce1(curry(concatenate-as, type), contents-sequences)
end method;


// TODO: Could provide more efficient read and write implementations than the
// character-by-character implementation in <basic-wrapper-stream>.


//
// Internal
//


define method initialize (wrapper :: <replacing-stream>, #key)
=> ()
   next-method();
   clear-contents(wrapper);
end method;


define function clear-contents (wrapper :: <replacing-stream>) => ()
   wrapper.inner-stream-limits.size := 0;
   wrapper.segment-contents.size := 0;
   wrapper.segment-limits.size := 0;
   wrapper.inner-stream.stream-position := #"start";
   let inner-pos = as(<integer>, wrapper.inner-stream.stream-position);
   wrapper.inner-stream-limits := add!(wrapper.inner-stream-limits, inner-pos);
   wrapper.segment-contents := add!(wrapper.segment-contents, #());
   wrapper.segment-limits := add!(wrapper.segment-limits, 0);
   wrapper.stream-position := #"start";
end function;


define function segment-from-position
   (wrapper :: <replacing-stream>, position :: <integer>)
=> (segment :: <integer>, offset :: <integer>)
   let seg-limits = wrapper.segment-limits;
   
   // Binary search.
   let partition-start = 0;
   let partition-end = seg-limits.size;
   while (partition-start < partition-end)
      let pivot = partition-start + floor/(partition-end - partition-start, 2);
      if (position < seg-limits[pivot])
         partition-end := pivot;
      else
         partition-start := pivot + 1;
      end if;
   end while;

   let prev-seg = partition-start - 1;
   let off = position - seg-limits[prev-seg];
   values(prev-seg + 1, off)
end function;


define function position-from-segment
   (wrapper :: <replacing-stream>, segment :: <integer>, offset :: <integer>)
=> (position :: <integer>)
   wrapper.segment-limits[segment - 1] + offset
end function;


define function set-inner-stream-position
   (wrapper :: <replacing-stream>,
    #key segment: cur-seg = wrapper.current-segment,
         offset: cur-off = wrapper.current-offset)
=> ()
   let inner-start = wrapper.inner-stream-limits[cur-seg - 1];
   adjust-stream-position-below-end(wrapper.inner-stream,
         origin: inner-start, delta: cur-off);
end function;


// Just like adjust-stream-position, except that you can move to the
// end-of-stream position without growing the stream. However, if you move past
// the end-of-stream position, it does grow the stream.
define function adjust-stream-position-below-end
   (stream :: <positionable-stream>,
    #key origin = stream.stream-position, delta :: <integer>)
=> (new-position :: <integer>)
   stream.stream-position := origin;
   if (delta > 0)
      adjust-stream-position(stream, delta - 1);
      read-element(stream, on-end-of-stream: #f);
   elseif (delta < 0)
      adjust-stream-position(stream, delta);
   end if;
   as(<integer>, stream.stream-position)
end function;


// Do not call for post-segment content.
define function segment-size
   (wrapper :: <replacing-stream>, seg :: <integer>)
=> (sz :: <integer>)
   wrapper.segment-limits[seg] - wrapper.segment-limits[seg - 1]
end function;
