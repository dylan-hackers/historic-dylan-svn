module: wrapper-streams-tester
author: Dustin Voss

define test cts-works ()
   let stream = make(<canonical-text-stream>, inner-stream: $tabbed-text-stream);

   stream.stream-position := 20;
   check-equal("partial expansion check", ' ', stream.read-element);
   
   stream.stream-position := #"start";
   let contents = stream.read-to-end;
   check-equal("full expansion check",
         "Inigo Montoya:\n"
         "        Hello.  My name is...\n",
         contents);
   
   stream.stream-position := #"start";
   read-through(stream, '.'); // Stream is now positioned after '.'
   let (line, col) = stream.line-col-position;
   check-equal("line check", 2, line);
   check-equal("col check", 15, col);
   
   stream.stream-position := 20;
   let (line, col) = line-col-position(stream, at: 3);
   check-equal("expl line check", 1, line);
   check-equal("expl col check", 4, col);
   check-equal("expl pos unchanged", 20, stream.stream-position);
end test;
