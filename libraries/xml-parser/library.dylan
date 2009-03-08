module: dylan-user

define library xml-parser
  use common-dylan;
  use anaphora;
  //use multimap;
  use meta;
  use io;
  use system, import: { file-system, threads };

  export xml-parser,
    xml-stream-parser,
    simple-xml,
    %productions,
    printing;
end library;

define module xml-parser
  create parse-document;

  create <document>, <element>, <node-mixin>, <attribute>, <xml>,
    <processing-instruction>, <entity-reference>, <add-parents>,
    <char-reference>, <comment>, <tag>, <char-string>, <dtd>,
    <internal-entity>, <external-entity>, text, text-setter,
    unfiltered-text, name, name-setter,
    name-with-proper-capitalization, root, char;

  create entity-value, attributes, attributes-setter, *dtd-paths*,
    attribute-value, attribute-value-setter,
    node-children, node-children-setter, 
    element-parent, element-parent-setter,
    make-element, sys-id, pub-id, sys/pub,
    internal-declarations, internal-declarations-setter,
    expansion, expansion-setter, comment, comment-setter;

  create <xml-error>, <xml-parse-error>;

  // for printing
  create <printing>, xml-name, escape-xml;

  create transform, <xform-state>;
end module xml-parser;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;

  use meta;
  use xml-parser, export: { <node-mixin> };

  export
    <reference>, <attributes>, <external-mixin>, *entities*,
    after-open, before-close, $hex-digit, $version-number, trim-string;
end module interface;

define module latin1-entities
  use common-dylan;
  use xml-parser;
  use interface;
  export initialize-latin1-entities;
end module latin1-entities;

define module transform
  use common-dylan, exclude: {format-to-string };
  use streams;
  use format;
  use standard-io;
  use xml-parser;
  use interface;
end module transform;

define module printing
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use pprint;
  use anaphora;
  use threads;

  use xml-parser;

  use interface;
  use transform;
  
  create print-opening, print-attributes, print-closing;
end module printing;

define module %productions
  use common-dylan, exclude: { format-to-string };
  use latin1-entities;
  use standard-io;
  use format-out;
  use streams;
  use format;
  //use multimap;
  use anaphora;
  use file-system, import: { with-open-file, file-exists? };
  use print;
  use threads;

  use meta;
  use interface;
  use xml-parser;

  export scan-xml-decl, scan-name, scan-s?, scan-xml-attributes,
    scan-start-tag, scan-end-tag;
end module %productions;

define module simple-xml
  use common-dylan;
  use common-extensions;
  use streams;
  use xml-parser, export: { escape-xml };
  use printing;

  export \with-xml,
    \with-xml-builder,
    attribute,
    elements,
    add-attribute,
    remove-attribute,
    add-element,
    remove-element,
    import-element,
    namespace,
    add-namespace,
    remove-namespace,
    replace-element-text,
    prefix, prefix-setter,
    real-name,
    start-tag,
    parents;
    
end module simple-xml;

define module xml-stream-parser
  use common-dylan;
  use common-extensions;
  use streams;
  use xml-parser;
  use %productions;

  export <xml-stream-parser>,
    stream, stream-setter,
    parse, monitor;

end module xml-stream-parser;
