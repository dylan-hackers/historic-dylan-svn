module: common
synopsis: Useful I/O functions.


define macro with-file-error-handlers
   {  with-file-error-handlers (#key ?default-locator:expression) 
         ?:body
      end }
   => {  block ()
            ?body
         exception (err :: <file-does-not-exist-error>)
            file-not-found(location: #f, filename: err.file-error-locator)
         exception (err :: <file-error>)
            file-error(filename: err.file-error-locator, error: condition-to-string(err))
         exception (err :: <file-system-error>)
            file-error(filename: ?default-locator, error: condition-to-string(err))
         end block }
end macro;


define macro with-open-file
   {  with-open-file (?:name = ?locator:expression, #rest ?keys:expression)
         ?:body
      end }
   => {  with-file-error-handlers (default-locator: ?locator)
            let ?name = make(<file-stream>, locator: ?locator, ?keys);
            block ()
               ?body
            cleanup
               close(?name)
            end block
         end with-file-error-handlers }
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


define method delete-directory (directory :: <directory-locator>) => ()
   do-directory(
         method (directory, filename, type) => ()
            select (type)
               #"directory" =>
                  let subdir-loc = merge-locators(
                        as(<directory-locator>, filename),
                        directory);
                  delete-directory(subdir-loc);
               otherwise =>
                  let file-loc = merge-locators(
                        as(<file-locator>, filename),
                        directory);
                  delete-file(file-loc);
            end select
         end method,
         directory);
   sys-delete-directory(directory);
end method;
