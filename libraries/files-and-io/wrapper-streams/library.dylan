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
   use format-out;
   
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
   use format-out;
   // from collection-extensions
   use vector-search;

   export
      <replacing-stream>,
      add-replacement-contents, inner-stream-position;
end module;


define module canonical-text-stream
   use basic-wrapper-stream;
   use replacing-stream;
   // from common-dylan
   use dylan;
   use common-extensions, exclude: { format-to-string };
   // from io
   use streams;
   use format-out;
   // from collection-extensions
   use vector-search;

   export
      <canonical-text-stream>,
      tabstop-size, line-col-position;
end module;
