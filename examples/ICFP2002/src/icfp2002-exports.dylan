module: dylan-user
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define library icfp2002
  use common-dylan;
  use io;
  use collection-extensions;
  use string-extensions;
  use time;
  use garbage-collection;
end library;

define module icfp2002
  use common-dylan, exclude: {string-to-integer};
  use format-out;
  use format;
  use subseq;
  use streams;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
  use time;
  use garbage-collection;
end module;
