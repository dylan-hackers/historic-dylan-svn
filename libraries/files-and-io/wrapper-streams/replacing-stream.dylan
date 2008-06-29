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
   // correspond. 'inner-stream-limits' are the base stream positions just past
   // the end of each segment, 'segment-contents' are the contents of each
   // segment or #f if the base stream's contents should be used, and 
   // 'segment-limits' are the wrapper stream positions just past the end of
   // each segment, considering all preceding replacement contents. Last
   // segment is always a replacement; stream contents beyond that are base
   // stream contents. There is always at least one element in these arrays;
   // they are prepopulated with an empty segment.
   
   constant slot inner-stream-limits =
      make(<stretchy-vector> /* of <stream-position> or <integer> */);
   constant slot segment-contents =
      make(<stretchy-vector> /* of <sequence> or #f */);
   constant slot segment-limits =
      make(<stretchy-vector> /* of <integer> */);
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
positions exist. It is not necessary to grow the inner stream if 'start' and
'end' correspond to the inner stream's end-of-stream position. If the inner
stream cannot be grown, an error is signaled.

The current 'wrapper' stream position is left effectively unchanged (pointing
to the same current element) if possible.

- If the current element was not replaced or removed, the stream position may
  be altered but will still point to the same element.
- If the current element was replaced by a new element, the stream position
  will point to the replacement element.
- If the current element was removed altogether, the stream position will
  point to the next element still present in the stream.

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
   
   assert(start-pos <= end-pos,
          "Start position %= comes after end position %=", start-pos, end-pos);
   assert(start-pos >= seg-limits.last,
          "Replacements follow position %=", start-pos);
   assert(~instance?(replacement, <stretchy-collection>),
          "Replacement cannot be stretchy");
   assert(inner.stream-element-type = <object> |
         every?(rcurry(instance?, inner.stream-element-type), replacement),
         "Replacement element %= not of inner stream element type %=",
         any?(rcurry(complement(instance?), inner.stream-element-type),
              replacement),
         inner.stream-element-type);
   
   if (end-pos - start-pos = 0 & replacement.size = 0)
      // If replacement is empty and segment to replace is empty, might as
      // well make no change at all.
      values(start-pos, end-pos)
   else
      // If new segment isn't adjacent to last segment, create base stream segment
      // to cover the gap. Covering segment runs from last segment to start of
      // new segment.
      if (start-pos ~= seg-limits.last)
         inner.stream-position := inner-limits.last;
         adjust-stream-position(inner, start-pos - seg-limits.last);
         add!(inner-limits, inner.stream-position);
         add!(seg-limits, start-pos);
         add!(seg-contents, #f);
      end if;
   
      // Add new segment. The adjust skips to the position before end-pos. We do
      // not skip to end-pos itself, because that may be beyond eos and adjust
      // would then increase the size of the stream; we don't want that, though
      // we do want the stream increased to just before end-pos, and we do want
      // the (eos) stream-position of end-pos itself so we can add it to
      // inner-limits. So we advance the stream position by one more element
      // after the adjust using read-element.
      inner.stream-position := inner-limits.last;
      adjust-stream-position(inner, max(end-pos - start-pos - 1, 0));
      read-element(inner, on-end-of-stream: #f);
      add!(inner-limits, inner.stream-position);
      add!(seg-limits, start-pos + replacement.size);
      add!(seg-contents, replacement);
   
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
      adjust-stream-position(wrapper, +1);
   else
      adjust-inner-stream-position(wrapper);

      elem := apply(read-element, inner, keys);
      if (wrapper.stream-at-end?)
         // Don't want to grow inner stream trying to adjust wrapper position.
         adjust-stream-position(wrapper, 0, from: #"end");
      else
         adjust-stream-position(wrapper, +1);
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

   adjust-stream-position(wrapper, -1);
   let cur-seg = wrapper.current-segment;
   let cur-off = wrapper.current-offset;

   let elem = #f;
   if (cur-seg < seg-count & seg-contents[cur-seg])
      elem := seg-contents[cur-seg][cur-off];
   else
      adjust-inner-stream-position(wrapper);
      elem := peek(inner);
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
      adjust-inner-stream-position(wrapper);
      elem := apply(peek, inner, keys);
   end if;

   elem
end method;


define method stream-input-available? (wrapper :: <replacing-stream>)
=> (available? :: <boolean>)
  wrapper.current-segment < wrapper.segment-limits.size |
  wrapper.inner-stream.stream-input-available?
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
      adjust-stream-position(wrapper, +1);
   else
      adjust-inner-stream-position(wrapper);
      write-element(inner, elem);
      if (wrapper.stream-at-end?)
         // Don't want to grow inner stream trying to adjust wrapper position.
         adjust-stream-position(wrapper, 0, from: #"end");
      else
         adjust-stream-position(wrapper, +1);
      end if;
   end if;
end method;


define method stream-at-end? (wrapper :: <replacing-stream>)
=> (at-end? :: <boolean>)
   adjust-inner-stream-position(wrapper);
   wrapper.current-segment >= wrapper.segment-contents.size &
         wrapper.inner-stream.stream-at-end?;
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
   adjust-stream-position(wrapper, 0, from: #"start");
end method;


define method stream-position-setter
   (position == #"end", wrapper :: <replacing-stream>)
=> (position :: <integer>)
   adjust-stream-position(wrapper, 0, from: #"end");
end method;


define method adjust-stream-position
   (wrapper :: <replacing-stream>, delta :: <integer>,
    #key from :: one-of(#"current", #"start", #"end") = #"current")
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
            #"start" => values(1, 0);
            #"end" =>
               let post-segment-size =
                     inner.stream-size - as(<integer>, inner-limits.last);
               values(seg-count, post-segment-size);
         end select;
   
   while (delta ~= 0)
      if (delta < 0)
         if (cur-seg > 0)
            // Move no further than just before segment.
            let adj-size = min(cur-off + 1, -delta);
            cur-off := cur-off - adj-size;
            
            // While before the start of the segment, move to prev segment.
            // This also skips empty segments.
            while (cur-off < 0 & cur-seg > 0)
               cur-seg := cur-seg - 1;
               cur-off := segment-size(wrapper, cur-seg) - 1;
            end while;
            
            delta := delta + adj-size;
         else
            // This branch handles case where we move to before any wrapper
            // stream content. Should signal error, if not, just move to start.
            adjust-stream-position(inner, delta, from: #"start");
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
            adjust-inner-stream-position(
                  wrapper, segment: cur-seg, offset: cur-off);
            adjust-stream-position(inner, delta);
            cur-off := cur-off + delta;
            delta := 0;
         end if;
      end if;
   end while;
   
   // Ensure cur-seg, cur-off has something there or is at end of stream, even
   // if delta was 0.
   while (cur-seg < seg-count & cur-off >= segment-size(wrapper, cur-seg))
      cur-seg := cur-seg + 1;
      cur-off := 0;
   end while;
   
   wrapper.current-segment := cur-seg;
   wrapper.current-offset := cur-off;
   position-from-segment(wrapper, cur-seg, cur-off)
end method;


define method stream-size (wrapper :: <replacing-stream>)
=> (sz :: <integer>)
   // Would prefer to use #"end" instead of stream-size, but apparently that
   // needs a stream limit, which is something almost all streams lack.
   wrapper.inner-stream.stream-position := wrapper.inner-stream.stream-size;
   let last-seg-start = as(<integer>, wrapper.inner-stream-limits.last);
   let last-seg-end = as(<integer>, wrapper.inner-stream.stream-position);
   wrapper.segment-limits.last + (last-seg-end - last-seg-start)
end method;


/// Not sure what a stream limit is, but I think it is supposed to be the
/// stream position at #"end".
define inline method stream-limit (wrapper :: <replacing-stream>)
=> (limit :: <integer>)
   wrapper.stream-size
end method;


define method stream-contents
   (wrapper :: <replacing-stream>, #rest keys, #key clear-contents?)
=> (contents :: <sequence>)
   apply(stream-contents-as, wrapper.sequence-type-for-inner-stream, wrapper,
         keys)
end method;


define method stream-contents-as
   (type :: <type>, wrapper :: <replacing-stream>, #rest keys,
    #key clear-contents? :: <boolean> = #t)
=> (contents :: <sequence>)
   assert(subtype?(type, <sequence>), "Type must be a <sequence>");
   let inner-limits = wrapper.inner-stream-limits;
   let seg-contents = wrapper.segment-contents;

   let inner-contents = apply(stream-contents, wrapper.inner-stream, keys);
   let contents-sequences = make(<stretchy-vector>);

   for (content in seg-contents, seg-num from 0)
      if (content)
         // Get content from replacement sequence.
         contents-sequences := add!(contents-sequences, content);
      else
         // Get content from inner stream.
         let cont-start = as(<integer>, inner-limits[max(seg-num - 1, 0)]);
         let cont-end = as(<integer>, inner-limits[seg-num]);
         let content =
               copy-sequence(inner-contents, start: cont-start, end: cont-end);
         contents-sequences := add!(contents-sequences, content);
      end if;
   end for;

   // Get trailing content from inner stream.
   let cont-start = as(<integer>, inner-limits.last);
   let cont-end = inner-contents.size;
   let content = copy-sequence(inner-contents, start: cont-start, end: cont-end);
   contents-sequences := add!(contents-sequences, content);

   if (clear-contents?)
      // Clear-contents for inner stream was done while binding inner-contents.
      clear-contents(wrapper);
   end if;
   
   apply(concatenate-as, type, contents-sequences)
end method;


// TODO: stream-limit-setter or stream-size-setter?


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
   add!(wrapper.inner-stream-limits, wrapper.inner-stream.stream-position);
   add!(wrapper.segment-contents, #());
   add!(wrapper.segment-limits, 0);
   wrapper.stream-position := #"start";
end function;


define function segment-from-position
   (wrapper :: <replacing-stream>, position :: <integer>)
=> (segment :: <integer>, offset :: <integer>)
   let seg-limits = wrapper.segment-limits;
   let prev-seg = find-last-key(seg-limits, rcurry(\<=, position));
   let off = position - seg-limits[prev-seg];
   values(prev-seg + 1, off)
end function;


define function position-from-segment
   (wrapper :: <replacing-stream>, segment :: <integer>, offset :: <integer>)
=> (position :: <integer>)
   wrapper.segment-limits[segment - 1] + offset
end function;


define function adjust-inner-stream-position
   (wrapper :: <replacing-stream>,
    #key segment: cur-seg = wrapper.current-segment,
         offset: cur-off = wrapper.current-offset)
=> ()
   let inner = wrapper.inner-stream;
   let inner-limits = wrapper.inner-stream-limits;
   let seg-contents = wrapper.segment-contents;
   let seg-count = inner-limits.size;

   let inner-start = inner-limits[cur-seg - 1];
   inner.stream-position := inner-start;
   adjust-stream-position(inner,
         if (cur-seg < seg-count) min(cur-off, segment-size(wrapper, cur-seg))
         else cur-off end if);
end function;


// Do not call for post-segment content.
define function segment-size
   (wrapper :: <replacing-stream>, seg :: <integer>)
=> (sz :: <integer>)
   wrapper.segment-limits[seg] - wrapper.segment-limits[seg - 1]
end function;
