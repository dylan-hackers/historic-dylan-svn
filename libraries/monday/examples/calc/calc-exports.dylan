module: dylan-user

define library calc
  use common-dylan;
  use io;
  use system;

  use regular;                  // FIXME
  use grammar;                  // FIXME
  
  use source-location;
  use simple-parser;
end library;

define module calc
  use common-dylan, exclude: { format-to-string };
  use byte-vector;
  use format-out;
  use locators;
  use streams;
  use standard-io;
  use print;
  use format;
  use source-location;
  use source-location-rangemap;
  use simple-parser;
  use simple-lexical-scanner;
end module;
