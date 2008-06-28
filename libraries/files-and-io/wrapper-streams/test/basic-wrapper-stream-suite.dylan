module: wrapper-streams-tester
author: Dustin Voss


define test read-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);
   check-equal("expected data check", "Hello.\nMy",
         read(stream, 9));
   check-condition("EOS condition check", <incomplete-read-error>,
         read(stream, 100));
   check-equal("EOS value check", #t,
         read(stream, 1, on-end-of-stream: #t));
end test;


define test read-into!-test ()
   check-true("partially tested by read-test", #t);
end test;


define test write-test ()
   let inner-stream = make(<sequence-stream>, element-type: <character>,
         direction: #"output", contents: make(<stretchy-vector>));
   let stream = make(<basic-wrapper-stream>, inner-stream: inner-stream);
   let data = "Existential questions";
   write(stream, data);
   check-equal("expected data check", data, stream-contents-as(<string>, stream));
end test;


define test read-to-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);

   let (val, found?) = read-to(stream, '\n');
   check-equal("expected data check", "Hello.", val);
   check-true("found flag check", found?);
   check-equal("position check", 7, stream.stream-position);

   let (val, found?) = read-to(stream, 'x');
   let exp = copy-sequence($text-stream-contents, start: 7);
   check-equal("EOS value check", exp, val);
   check-false("not found flag check", found?);
   
   let (val, found?) = read-to(stream, 'x', on-end-of-stream: "missing");
   check-equal("at EOS value check", "missing", val);
   check-false("not found at EOS flag check", found?);
   
   let inner-stream = make(<sequence-stream>, contents: "$",
         element-type: <character>, start: 0, end: 1);
   let stream = make(<basic-wrapper-stream>, inner-stream: inner-stream);
   let (val, found?) = read-to(stream, '$');
   check-equal("empty data check", "", val);
   check-true("found empty flag check", found?);
end test;


define test read-through-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);

   let (val, found?) = read-through(stream, '\n');
   check-equal("expected data check", "Hello.\n", val);
   check-true("found flag check", found?);
   check-equal("position check", 7, stream.stream-position);

   let (val, found?) = read-through(stream, 'x');
   let exp = copy-sequence($text-stream-contents, start: 7);
   check-equal("EOS value check", exp, val);
   check-false("not found flag check", found?);

   let (val, found?) = read-to(stream, 'x', on-end-of-stream: "missing");
   check-equal("at EOS value check", "missing", val);
   check-false("not found at EOS flag check", found?);
end test;


define test read-to-end-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);
   read(stream, 7);
   let all = read-to-end(stream);
   let all-start = copy-sequence(all, end: 7);
   let all-end = copy-sequence(all, start: all.size - 4);
   check-equal("expected start check", "My name", all-start);
   check-equal("expected end check", "die!", all-end);
end test;


define test skip-through-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);
   skip-through(stream, '\n');
   check-equal("position check", 7, stream.stream-position);
   check-equal("expected data check", "My name", read(stream, 7));
   check-false("not found check", skip-through(stream, 'x'));
end test;


define test read-line-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);

   let (val, eol?) = stream.read-line;
   check-equal("LF data check", "Hello.", val);
   check-true("LF EOL flag check", eol?);

   let (val, eol?) = stream.read-line;
   check-equal("CR data check", "My name is Inigo Montoya.", val);
   check-true("CR EOL flag check", eol?);

   let (val, eol?) = stream.read-line;
   check-equal("CRLF data check", "You killed my father.", val);
   check-true("CRLF EOL flag check", eol?);

   let (val, eol?) = stream.read-line;
   check-equal("EOS data check", "Prepare to die!", val);
   check-false("EOS EOL flag check", eol?);
   
   check-condition("EOS condition check", <end-of-stream-error>,
                   stream.read-line);
end test;


define test read-line-into!-test ()
   check-true("tested by read-line-test", #t);
end test;


define test read-text-test ()
   $text-stream.stream-position := #"start";
   let stream = make(<basic-wrapper-stream>, inner-stream: $text-stream);
   check-equal("N check", "Hel", read-text(stream, 3));
   check-equal("LF check", "lo.\n", read-text(stream, 4));
   check-equal("CR check", "My name is Inigo Montoya.\n", read-text(stream, 26));
   check-equal("CRLF check", "You killed my father.\nPre", read-text(stream, 25));
   check-equal("EOS check", "pare to die!", read-text(stream, 20));
end test;


define test read-text-into!-test ()
   check-true("partially tested by read-text-test", #t)
end test;


define test write-line-test ()
   let inner-stream = make(<sequence-stream>, element-type: <character>,
         direction: #"output", contents: make(<stretchy-vector>));
   let stream = make(<basic-wrapper-stream>, inner-stream: inner-stream);
   write-line(stream, "Existential questions");
   check-equal("expected data check", "Existential questions\n",
               stream-contents-as(<string>, stream));
end test;


define test new-line-test ()
   let inner-stream = make(<sequence-stream>, element-type: <character>,
         direction: #"output", contents: make(<stretchy-vector>));
   let stream = make(<basic-wrapper-stream>, inner-stream: inner-stream);
   stream.new-line;
   check-equal("expected data check", "\n", stream-contents-as(<string>, stream));
end test;
