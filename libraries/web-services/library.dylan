Module:    dylan-user
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define library web-services
  use dylan;
  use common-dylan;
  use io;
  // use dfmc-common;
  // use collection-extensions;
  use xml-parser;

  export web-services-info;
  export xml-schema;
  export xml-converter;
  export mailbox;
  export web-services;
  export wsdl;
  export uddi;
end library web-services;
