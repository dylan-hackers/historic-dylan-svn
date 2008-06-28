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
   let (row, col) = stream.row-col-position;
   check-equal("row check", 2, row);
   check-equal("col check", 15, col);
end test;
