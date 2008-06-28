module: wrapper-streams-tester
author: Dustin Voss

define test replacing-stream-size ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, #(), start: 0, end: 4);
   add-replacement-contents(stream, "xxxx", start: 3, end: 4);
   check-equal("length check",
               $base-stream-contents.size - 4 + 3, stream.stream-size);
end test;


define test position-in-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 0, end: 2);
   // should now be xxxxbbccdd
   adjust-stream-position(stream, +1, from: #"start");
   check-equal("position check", 1, stream.stream-position);
   let result = read(stream, 5);
   check-equal("expected data check", "xxxbb", result);
end test;


define test position-in-inner-stream ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 4, end: 6);
   // should now be aabbxxxxdd
   adjust-stream-position(stream, +1, from: #"start");
   check-equal("position check", 1, stream.stream-position);
   let result = read(stream, 5);
   check-equal("expected data check", "abbxx", result);
end test;


define test dec-position ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 4, end: 6);
   // should now be aabbxxxxdd
   stream.stream-position := 5;
   adjust-stream-position(stream, -1);
   check-equal("position check", 4, stream.stream-position);
   let result = read(stream, 6);
   check-equal("expected data check", "xxxxdd", result);
end test;


define test inc-position ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 4, end: 6);
   // should now be aabbxxxxdd
   stream.stream-position := 5;
   adjust-stream-position(stream, +2);
   check-equal("position check", 7, stream.stream-position);
   let result = read(stream, 3);
   check-equal("expected data check", "xdd", result);
end test;


define test dec-position-across-boundary ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 4, end: 6);
   // should now be aabbxxxxdd
   stream.stream-position := 5;
   adjust-stream-position(stream, -4);
   check-equal("position check", 1, stream.stream-position);
   let result = read(stream, 6);
   check-equal("expected data check", "abbxxx", result);
end test;


define test inc-position-across-boundary ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   read(stream, 7);
   add-replacement-contents(stream, "xxxx", start: 4, end: 6);
   // should now be aabbxxxxddee
   stream.stream-position := 5;
   adjust-stream-position(stream, +4);
   check-equal("position check", 9, stream.stream-position);
   let result = read(stream, 3);
   check-equal("expected data check", "dee", result);
end test;


define test inc-position-across-empties ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   let (s, e) = add-replacement-contents(stream, #(), start: 4, end: 6);
   let (s, e) = add-replacement-contents(stream, #(), start: e, end: e);
   let (s, e) = add-replacement-contents(stream, #(), start: e, end: e + 2);
   // should now be aabbee
   stream.stream-position := 3;
   let pos = adjust-stream-position(stream, +1);
   check-equal("position check", 4, pos);
   let result = read(stream, 3);
   check-equal("expected data check", "eef", result);
end test;


define test dec-position-across-empties ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   let (s, e) = add-replacement-contents(stream, #(), start: 4, end: 6);
   let (s, e) = add-replacement-contents(stream, #(), start: e, end: e);
   let (s, e) = add-replacement-contents(stream, #(), start: e, end: e + 2);
   // should now be aabbee
   stream.stream-position := 4;
   let pos = adjust-stream-position(stream, -1);
   check-equal("position check", 3, pos);
   let result = read(stream, 3);
   check-equal("expected data check", "bee", result);
end test;
