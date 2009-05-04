module: common
synopsis: Useful I/O functions.


define macro with-open-file
   {  with-open-file (?:name = ?locator:expression, #rest ?keys:expression)
         ?:body
      end }
   => {  block ()
            let ?name = make(<file-stream>, locator: ?locator, ?keys);
            block ()
               ?body
            cleanup
               close(?name)
            end block
         exception (err :: <file-does-not-exist-error>)
            file-not-found(location: #f, filename: ?locator)
         exception (err :: <file-system-error>)
            file-error(filename: ?locator, error: condition-to-string(err))
         end block }
end macro;


define method read-expected
   (stream :: <stream>, expected :: <sequence>,
    #key test :: <function> = \=, failure = unsupplied())
=> (result-or-failure :: <object>)
   if (expected.empty?)
      expected
   else
      let next = read(stream, expected.size, on-end-of-stream: #[]);
      let found? = test(expected, next);
      case
         found? => next;
         failure.supplied? => failure;
         otherwise => error("Expected stream elements not found");
      end case;
   end if;
end method;


define method read-lines-to-end (stream :: <stream>)
=> (lines :: <sequence>)
   let lines = make(<stretchy-vector>);
   iterate next-line ()
      let line = read-line(stream, on-end-of-stream: #f);
      if (line)
         add!(lines, line);
         next-line();
      end if;
   end iterate;
   lines
end method;
