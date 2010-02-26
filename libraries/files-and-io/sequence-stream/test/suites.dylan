module: sequence-stream-tester
author: Dustin Voss 

define constant $text-contents =
      "Hello.\n"
      "My name is Inigo Montoya.\r"
      "You killed my father.\r\n"
      "Prepare to die!";


define suite sequence-stream-suite ()
   test position-offset-test;
   test read-test;
   test read-into!-test;
   test write-test;
   test read-to-test;
   test read-through-test;
   test read-to-end-test;
   test skip-through-test;
   test read-line-test;
   test read-line-into!-test;
   test read-text-test;
   test read-text-into!-test;
   test write-line-test;
   test new-line-test;
   test grow-test;
end suite;
