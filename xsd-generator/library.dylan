Module:    dylan-user
Synopsis:  Parse xsd to dylan classes
Author:    Hannes Mehnert, Bastian Mueller
Copyright: (C) 2007,.  All rights reserved.

define library xsd-generator
  use functional-dylan;
  use io;
  use xml-parser;

  // Add any more module exports here.
  export xsd-generator;
end library xsd-generator;
