Module:    dylan-user
Synopsis:  Tests for the XML converter.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define module web-services-test
  use functional-dylan;
  use dylan-extensions,
    exclude: { test-function };
  use threads;
  use format;
  use format-out;
  use standard-io;
  use streams;
  use date;
  use file-system;
  use operating-system;

  use testworks;
  use xml-parser;

  use namespaces;
  use xml-schema;
  use xml-converter;
  use mailbox;
  use web-services;
  use wsdl;
  use uddi;

  export web-services-test-full-name;
end module web-services-test;
