module: dylan-user

define library xml-parser
  use common-dylan;
  use anaphora;
  //use multimap;
  use meta;
  use io;
  use system, import: { file-system };
//  use collection-extensions;
//  use string-extensions;

  export xml-parser;
  export namespaces;
end library;

define module xml-parser
  create parse-document;

  create <document>, <element>, <node-mixin>, <attribute>, <xml>, <processing-instruction>,
    <entity-reference>, <add-parents>, <char-reference>, <comment>, <tag>,
    <char-string>, <dtd>, <internal-entity>, <external-entity>,
    text, text-setter, name, name-setter, name-with-proper-capitalization,
    xml-name, xml-name-setter,
    root, char;

  create entity-value, attributes, attributes-setter, *dtd-paths*,
    attribute-value, attribute-value-setter,
    node-children, node-children-setter, 
    element-parent, element-parent-setter,
    collect-elements, make-element, sys-id, pub-id, sys/pub,
    internal-declarations, internal-declarations-setter,
    expansion, expansion-setter, comment, comment-setter;

  // for printing
  create <printing>, xml-as-string,
    *xml-depth*, *open-the-tag*, *close-the-tag*, *ampersand*, *printer-state*;

    // for iteration
  create node-iterator, prepare-document;
  create transform, transform-document, before-transform, <xform-state>;

  // Macros
  create $default-xml-parent, make-xml-element;

  // Namespaces
  create <xml-namespace-error>,
    <namespace-not-found>, <namespace-has-no-short-name>;

  create <xml-name>,
    name-namespace, name-namespace-setter,
    name-local-name, name-local-name-setter,
    xml-name-as-string, string-as-xml-name;

  create <xml-namespace>,
    namespace-names,
    namespace-url, namespace-url-setter,
    namespace-short-name, namespace-short-name-setter;

  create $xml-namespace;

  create \with-default-namespace,
    \with-local-namespace, \with-local-namespaces,
    \with-local-xml-namespaces,
    find-local-namespace;

  create find-namespace, find-namespace-setter,
    register-namespace;
end module xml-parser;


define module namespaces
  use common-dylan,
    exclude: { format-to-string };
  use common-extensions,
    exclude: { format-to-string };
  use locators-protocol;
  use print;
  use format;
  use threads;
  use xml-parser;
  use format-out;
//  use subseq;
//  use character-type;

  export <namespace-binding>,
    local-short-name, local-namespace;

  export default-namespace, default-namespace-setter;

  export xml-namespace-attribute?,
    xml-namespace-prefix, local-xml-namespaces;
end module namespaces;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

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
  use anaphora;

  use xml-parser;

  use interface;
  use transform;
end module printing;

define module collect
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use anaphora;
  use xml-parser, rename: { attribute-value => value,
                           attribute-value-setter => value-setter };
  use interface;
end module collect;

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

  use meta;
  use interface;
  use xml-parser;
  use namespaces;
end module %productions;

