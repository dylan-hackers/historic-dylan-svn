Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define function msxml3-initialize () => ()
  ole-initialize();
end function msxml3-initialize;

define function make-document() 
  make-proxy(make-domdocument30());
end function make-document;

define function make-free-threaded-document()
  make-proxy(make-FreeThreadedDOMDocument30());
end function make-free-threaded-document;

define function make-schema-cache()
  make-proxy(make-XMLSchemaCache30());
end function make-schema-cache;

define function make-xsl-template()
  make-proxy(make-XSLTemplate30());
end function make-xsl-template;

