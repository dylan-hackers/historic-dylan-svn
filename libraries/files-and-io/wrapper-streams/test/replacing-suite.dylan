module: wrapper-streams-tester
author: Dustin Voss 


define test replace-at-start ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "xx");
   let result = read(stream, 4);
   check-equal("expected data check", "xxbb", result);
end test;


define test replace-in-middle ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "xxxx", start: 5);
   let result = read(stream, 10);
   check-equal("expected data check", "aabbcxxxxe", result);
end test;


define test replace-at-end ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "xx", start: 12);
   let result = read(stream, 14);
   check-equal("expected data check", "aabbccddeeffxx", result);
end test;


define test replace-past-end ()
   let inner = make(<string-stream>, contents: $base-stream-contents,
                    direction: #"input-output");
   let stream = make(<replacing-stream>, inner-stream: inner);
   add-replacement-contents(stream, "xx", start: stream.stream-size);
   let result = read(stream, 16);
   check-equal("expected data check", "aabbccddeeffggxx", result);
end test;


define test insert-at-start ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "x", start: 0, end: 0);
   stream.stream-position := #"start"; // Position will have been shifted.
   let result = read(stream, 5);
   check-equal("expected data check", "xaabb", result);
end test;


define test insert-in-middle ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "x", start: 4, end: 4);
   let result = read(stream, 9);
   check-equal("expected data check", "aabbxccdd", result);
end test;


define test insert-past-end ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "x",
                            start: stream.stream-size, end: stream.stream-size);
   let result = read-to-end(stream);
   check-equal("expected data check", "aabbccddeeffggx", result);
end test;


define test sequential-replacements ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   let (s, e) = add-replacement-contents(stream, "xx", start: 2);
   // contents should now be aaxxccdd
   add-replacement-contents(stream, "yy", start: e);
   // contents should now be aaxxyydd
   let result = read(stream, 8);
   check-equal("expected data check", "aaxxyydd", result);
end test;


define test empty-replacements ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, #(), start: 2, end: 4);
   // contents should now be aa()ccdd
   let result = read(stream, 6);
   check-equal("removed elements check", "aaccdd", result);
   
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, #(), start: 2, end: 2);
   // contents should now be aa()bbccdd
   let result = read(stream, 6);
   check-equal("no replaced elements check", "aabbcc", result);
end test;


define test no-stretchy-replacements ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   let rep = as(<stretchy-vector>, "xx");
   check-condition("error check", <error>,
         add-replacement-contents(stream, rep, start: 2));
end test;


define test no-addl-replacements ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   add-replacement-contents(stream, "xx", start: 4, end: 6);
   // contents should now be aabbxxdd
   check-condition("error check", <error>,
         add-replacement-contents(stream, "yy", start: 0));
end test;


define test no-illegal-elements ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   check-condition("error check", <error>,
         add-replacement-contents(stream, #(0), start: 4, end: 6));
end test;


define test replaced-position-unchanged-by-equal-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   stream.stream-position := 3;
   // contents start out as aabBccdd
   add-replacement-contents(stream, "xx", start: 2);
   // contents should now be aaxXccdd
   check-equal("position check", 3, stream.stream-position);
   let result = read(stream, 4);
   check-equal("expected data check", "xccd", result);
end test;


define test replaced-position-pushed-by-smaller-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   stream.stream-position := 4;
   // contents start out as aabbCcdd
   add-replacement-contents(stream, "x", start: 2, end: 6);
   // contents should now be aaxDd
   check-equal("position check", 3, stream.stream-position);
   let result = read(stream, 4);
   check-equal("expected data check", "ddee", result);
end test;
   

define test replaced-position-unchanged-by-larger-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   stream.stream-position := 3;
   // contents start out as aabBccdd
   add-replacement-contents(stream, "yyyy", start: 2, end: 4);
   // contents should now be aayYyyccdd
   check-equal("position check", 3, stream.stream-position);
   let result = read(stream, 4);
   check-equal("expected data check", "yyyc", result);
end test;


define test later-position-pulled-by-smaller-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   stream.stream-position := 6;
   // contents start out as aabbccDd
   add-replacement-contents(stream, "x", start: 2, end: 4);
   // contents should now be aaxccDd
   check-equal("position check", 5, stream.stream-position);
   let result = read(stream, 4);
   check-equal("expected data check", "ddee", result);
end test;
   

define test later-position-pushed-by-larger-replacement ()
   let stream = make(<replacing-stream>, inner-stream: $base-stream);
   stream.stream-position := 6;
   // contents start out as aabbccDd
   add-replacement-contents(stream, "yyyy", start: 2, end: 4);
   // contents should now be aayyyyccDd
   check-equal("position check", 8, stream.stream-position);
   let result = read(stream, 4);
   check-equal("expected data check", "ddee", result);
end test;
