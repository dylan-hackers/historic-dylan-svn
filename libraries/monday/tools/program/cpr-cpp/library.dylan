Module: Dylan-user

define library cpr-cpp
  use Common-Dylan;
  use IO;
  use System;
  
  use regular;               // shouldn't need this directly 
  use grammar;               // shouldn't need this directly 

  use source-location;
  use simple-parser;
  use cpr;
end library;
          
define module cpr-cpp
  use common-dylan, exclude: { format-to-string };
  use simple-parser;
  use source-location;
  use source-location-rangemap;
  use source-location-conditions;
  use grammar;
  use parser-automaton;
  use operating-system;
  use streams;
  use standard-io;
  use file-system;
  use locators;
  use print;
  use format;
  use format-out;
  use cpr;
end module;
