module: dylan-user
copyright: this program may be freely used by anyone, for any purpose

define library test-server
  use common-dylan;
  use io;
  use collection-extensions;
  use string-extensions;
//  use time; // gabor does not have this present at the moment
  use garbage-collection;
  use network;
end library;

define module test-server
  use common-dylan, exclude: {string-to-integer}, export: all;
  use format-out;
  use format;
  use subseq;
  use streams, export: all;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
//  use time;
  use garbage-collection;
  use network;
end module;




