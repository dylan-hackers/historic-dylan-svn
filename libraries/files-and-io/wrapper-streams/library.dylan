module: dylan-user
author: Dustin Voss

define library wrapper-streams
   use common-dylan;
   use io;
   use collection-extensions, import: { vector-search };

   export
      basic-wrapper-stream,
      replacing-stream,
      canonical-text-stream;
end library;


define module basic-wrapper-stream
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from io
   use streams;
   
   export
      <basic-wrapper-stream>, 
      sequence-type-for-inner-stream, string-type-for-inner-stream;
end module;
   

define module replacing-stream
   use basic-wrapper-stream;
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from io
   use streams;

   export
      <replacing-stream>,
      add-replacement-contents;
end module;


define module canonical-text-stream
   use basic-wrapper-stream;
   use replacing-stream;
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from io
   use streams;
   // from collection-extensions
   use vector-search;

   export
      <canonical-text-stream>,
      tabstop-size, row-col-position;
end module;
