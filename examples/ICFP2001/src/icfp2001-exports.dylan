module: dylan-user
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define library icfp2001
  use common-dylan;
  use io;
  use collection-extensions;
  use string-extensions;
  use time;
end library;

define module icfp2001
  use common-dylan, exclude: {string-to-integer};
  use format-out;
  use format;
  use subseq;
  use streams;
  use standard-io;
  use string-conversions, import: {string-to-integer};
  use extensions, import: {report-condition};
  use time;
end module;
