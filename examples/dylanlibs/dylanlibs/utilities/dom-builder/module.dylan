Module:    dylan-user
Synopsis:  Utilities for generating Document Object Models for XML/HTML documents.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Contains a very simple implementation of a basic
// document object model. A *much* more full featured
// one is available as part of Scott McKay's XML/HTML 
// parsers and when that is publically available I'll 
// deprecate this module and use that.
define module simple-dom
  use common-dylan;
  
  export 
    <element>,
    element-parent,
    element-children,
    add-child,
    <document>,
    <attribute>,
    attribute-parent,
    attribute-key,
    attribute-value,
    <node-element>,
    node-element-tag,
    node-element-attributes,
    add-attribute,
    <text-element>,
    text-element-text,
    text-element-text-setter;
end module simple-dom;

// Provides a macro that allows creating XML and HTML
// DOM's using a syntax very similar to Lisp S-Expressions.
// Modelled after AllegroServe's (A Common lisp Web Server)
// HTMLGEN facility.
define module dom-builder
  use common-dylan;
  use threads;
  use simple-dom, export: all;

  export 
    *current-dom-element*,
    with-dom-builder;
end module dom-builder;

// Contains utilities for generating HTML from a DOM.
define module html-generator
  use common-dylan;
  use simple-dom;
  use streams;

  export
    print-html,
    print-html-to-string;
end module html-generator;

// Contains utilities for generating XML from a DOM.
define module xml-generator
  use common-dylan;
  use simple-dom;
  use streams;
  use sequence-utilities;

  export
    print-xml,
    print-xml-to-string;
end module xml-generator;



