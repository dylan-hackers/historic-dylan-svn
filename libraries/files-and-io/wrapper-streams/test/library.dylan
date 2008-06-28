module: dylan-user
author: Dustin Voss 

define library wrapper-streams-tester
   use common-dylan;
   use io;
   use testworks;

   use wrapper-streams;
end library;

define module wrapper-streams-tester
   use common-dylan;
   use format-out;
   use streams;
   use testworks;

   use basic-wrapper-stream;
   use replacing-stream;
   use canonical-text-stream;
end module;
