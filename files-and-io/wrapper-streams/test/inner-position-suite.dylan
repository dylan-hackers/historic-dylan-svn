module: wrapper-streams-tester
author: Dustin Voss


define test one-to-one-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccdd
   add-replacement-contents(stream, "x", start: 2);
   add-replacement-contents(stream, "yy", start: 4);
   let result = read(stream, 8);
   check-equal("expected value", "aaxbyydd", result);

   let (pos, ins?) = inner-stream-position(stream, at: 2);
   check-equal("one char position check", 2, pos);
   check-equal("one char inserted flag", #f, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 4);
   check-equal("1st char position check", 4, pos);
   check-equal("1st char inserted flag", #f, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 5);
   check-equal("2nd char position check", 5, pos);
   check-equal("2nd char inserted flag", #f, ins?);
end test;


define test one-to-many-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccdd
   add-replacement-contents(stream, "xxx", start: 2, end: 4);
   let result = read(stream, 9);
   check-equal("expected value", "aaxxxccdd", result);
   
   let (pos, ins?) = inner-stream-position(stream, at: 2);
   check-equal("1st char position check", 2, pos);
   check-equal("1st char inserted flag", #t, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 3);
   check-equal("2nd char position check", 2, pos);
   check-equal("2nd char inserted flag", #f, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 4);
   check-equal("3rd char position check", 3, pos);
   check-equal("3rd char inserted flag", #f, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 5);
   check-equal("next char position check", 4, pos);
   check-equal("next char inserted flag", #f, ins?);
end test;


define test zero-to-many-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccdd
   add-replacement-contents(stream, "xx", start: 2, end: 2);
   let result = read(stream, 6);
   check-equal("expected value", "aaxxbb", result);

   let (pos, ins?) = inner-stream-position(stream, at: 2);
   check-equal("1st char position check", 2, pos);
   check-equal("1st char inserted flag", #t, ins?);

   let (pos, ins?) = inner-stream-position(stream, at: 3);
   check-equal("2nd char position check", 2, pos);
   check-equal("2nd char inserted flag", #t, ins?);
   
   let (pos, ins?) = inner-stream-position(stream, at: 4);
   check-equal("next char position check", 2, pos);
   check-equal("next char inserted flag", #f, ins?);
end test;


define test zero-to-many-at-start-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabb
   add-replacement-contents(stream, "x", start: 0, end: 0);
   stream.stream-position := #"start"; // Position will have been shifted.
   let result = read(stream, 5);
   check-equal("expected value", "xaabb", result);

   let (pos, ins?) = inner-stream-position(stream, at: 0);
   check-equal("start char position check", 0, pos);
   check-equal("start char inserted flag", #t, ins?);
   
   let (pos, ins?) = inner-stream-position(stream, at: 1);
   check-equal("after start char position check", 0, pos);
   check-equal("after start char inserted flag", #f, ins?);
end test;


define test zero-to-many-past-end-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccddeeffgg
   add-replacement-contents(stream, "x",
                            start: stream.stream-size, end: stream.stream-size);
   let result = read-to-end(stream);
   check-equal("expected value", "aabbccddeeffggx", result);

   let (pos, ins?) = inner-stream-position(stream, at: 13);
   check-equal("before end char position check", 13, pos);
   check-equal("before end char inserted flag", #f, ins?);
   
   let (pos, ins?) = inner-stream-position(stream, at: 14);
   check-equal("end char position check", 14, pos);
   check-equal("end char inserted flag", #t, ins?);
end test;


define test many-to-one-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbcc
   add-replacement-contents(stream, "x", start: 2, end: 4);
   let result = read(stream, 5);
   check-equal("expected value", "aaxcc", result);
   
   let (pos, ins?) = inner-stream-position(stream, at: 2);
   check-equal("1st char position check", 3, pos);
   check-equal("1st char inserted flag", #f, ins?);
   
   let (pos, ins?) = inner-stream-position(stream, at: 3);
   check-equal("next char position check", 4, pos);
   check-equal("next char inserted flag", #f, ins?);
end test;


define test many-to-zero-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbcc
   add-replacement-contents(stream, "", start: 2, end: 4);
   let result = read(stream, 4);
   check-equal("expected value", "aacc", result);
   
   let (pos, ins?) = inner-stream-position(stream, at: 1);
   check-equal("prev char position check", 1, pos);
   check-equal("prev char inserted flag", #f, ins?);
   
   let (pos, ins?) = inner-stream-position(stream, at: 2);
   check-equal("next char position check", 4, pos);
   check-equal("next char inserted flag", #f, ins?);
end test;


define test many-to-zero-at-end-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccddeeffgg
   add-replacement-contents(stream, "", start: 4, end: stream.stream-size);
   let result = read-to-end(stream);
   check-equal("expected value", "aabb", result);
   
   let (pos, ins?) = inner-stream-position(stream);
   check-equal("eos position check", 14, pos);
   check-equal("eos inserted flag", #f, ins?);
end test;


define test before-replacement-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbcc
   add-replacement-contents(stream, "xx", start: 2);
   
   let (pos, ins?) = inner-stream-position(stream, at: 0);
   check-equal("position check", 0, pos);
   check-equal("inserted flag", #f, ins?);
end test;


define test between-replacement-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbccddee
   let (s, e) = add-replacement-contents(stream, "x", start: 2, end: 4);
   let (s, e) = add-replacement-contents(stream, "y", start: e + 2, end: e + 4);
   let result = read(stream, 8);
   check-equal("expected value", "aaxccyee", result);
   
   let (pos, ins?) = inner-stream-position(stream, at: 4);
   check-equal("position check", 5, pos);
   check-equal("inserted flag", #f, ins?);
end test;


define test after-replacement-inner-pos ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // contents start out as aabbcc
   add-replacement-contents(stream, "x", start: 2, end: 4);
   let result = read(stream, 5);
   check-equal("expected value", "aaxcc", result);

   let (pos, ins?) = inner-stream-position(stream, at: 4);
   check-equal("position check", 5, pos);
   check-equal("inserted flag", #f, ins?);
end test;
