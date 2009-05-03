module: dylan-user
author: Dustin Voss 

define library sequence-stream-tester
   use common-dylan;
   use io;
   use testworks;

   use sequence-stream;
end library;

define module sequence-stream-tester
   use common-dylan;
   use format-out;
   use streams, exclude:
         { <sequence-stream>, <string-stream>, <byte-string-stream> };
   use testworks;

   use sequence-stream;
end module;
