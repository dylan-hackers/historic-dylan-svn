module: wrapper-streams-tester
author: Dustin Voss

define test read-larger-replacement ()
   check-true("tested by replacing-suite", #t);
end test;


define test read-smaller-replacement ()
   check-true("tested by replacing-suite", #t);
end test;


define test read-empty-replacement ()
   check-true("tested by replacing-suite", #t);
end test;


define test read-inner-stream ()
   check-true("tested by replacing-suite", #t);
end test;


define test read-entire-contents ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   // originally aabbccddeeffgg
   add-replacement-contents(stream, "xxx", start: 2, end: 4);
   // now aaxxxccddeeffgg
   add-replacement-contents(stream, "y", start: 9, end: 11);
   // now aaxxxccddyffgg
   let result = stream.stream-contents;
   check-equal("expected data check", "aaxxxccddyffgg", result);
end test;
