module: dylan-user
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define library icfp2002
  use common-dylan;
  use io;
  use collection-extensions;
  use string-extensions;
//  use time; // gabor does not have this present at the moment
  use garbage-collection;
  use network;
end library;

define module icfp2002
  use common-dylan, exclude: {string-to-integer}, export: all;
  use format-out;
  use format;
  use subseq;
  use streams;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
//  use time;
  use garbage-collection;
  use network;
end module;


define module board
  use icfp2002, export: all;
  
  export <board>;
end module board;

define module client
  use board;
  
end module client;

define module server
  use board;
  
end module server;




