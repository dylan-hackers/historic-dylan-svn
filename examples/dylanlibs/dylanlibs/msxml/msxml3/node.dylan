Module:    msxml3
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define constant <node-type> = one-of(#"invalid",
                                     #"element",
                                     #"attribute",
                                     #"text",
                                     #"cdata-section",
                                     #"entity-reference",
                                     #"entity",
                                     #"processing-instruction",
                                     #"comment",
                                     #"document",
                                     #"document-type",
                                     #"document-fragment",
                                     #"notation");

// Using a generic instead of an array lookup
// to enable extension by other libraries when
// msxml4 comes out for example.
define open generic node-type-from-index(index :: <integer>) => (r :: <node-type>);
define open generic node-classes-from-type(type :: <node-type>);

define method node-type-from-index(index :: <integer>) => (r == #"invalid")
    #"invalid"
end method node-type-from-index;

define method node-type-from-index(index == $NODE-ELEMENT) => (r == #"element");
    #"element"
end method node-type-from-index;

define method node-classes-from-type(type == #"element")
  values(<element>, <IXMLDOMElement>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-ATTRIBUTE) => (r == #"attribute");
    #"attribute"
end method node-type-from-index;

define method node-classes-from-type(type == #"attribute")
  values(<attribute>, <IXMLDOMAttribute>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-TEXT) => (r == #"text");
    #"text"
end method node-type-from-index;

define method node-classes-from-type(type == #"text")
  values(<text>, <IXMLDOMText>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-CDATA-SECTION) => (r == #"cdata-section");
    #"cdata-section"
end method node-type-from-index;

define method node-classes-from-type(type == #"cdata-section")
  values(<cdata-section>, <IXMLDOMCDATASection>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-ENTITY-REFERENCE) => (r == #"entity-reference");
    #"entity-reference"
end method node-type-from-index;

define method node-classes-from-type(type == #"entity-reference")
  values(<entity-reference>, <IXMLDOMEntityReference>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-ENTITY) => (r == #"entity");
    #"entity"
end method node-type-from-index;

define method node-classes-from-type(type == #"entity")
  values(<entity>, <IXMLDOMEntity>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-PROCESSING-INSTRUCTION) => (r == #"processing-instruction");
    #"processing-instruction"
end method node-type-from-index;

define method node-classes-from-type(type == #"processing-instruction")
  values(<processing-instruction>, <IXMLDOMProcessingInstruction>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-COMMENT) => (r == #"comment");
    #"comment"
end method node-type-from-index;

define method node-classes-from-type(type == #"comment")
  values(<comment>, <IXMLDOMComment>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-DOCUMENT) => (r == #"document");
    #"document"
end method node-type-from-index;

define method node-classes-from-type(type == #"document")
  values(<document2>, <IXMLDOMDocument2>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-DOCUMENT-TYPE) => (r == #"document-type");
    #"document-type"
end method node-type-from-index;

define method node-classes-from-type(type == #"document-type")
  values(<document-type>, <IXMLDOMDocumentType>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-DOCUMENT-FRAGMENT) => (r == #"document-fragment");
    #"document-fragment"
end method node-type-from-index;

define method node-classes-from-type(type == #"document-fragment")
  values(<document-fragment>, <IXMLDOMDocumentFragment>);
end method node-classes-from-type;

define method node-type-from-index(index == $NODE-NOTATION) => (r == #"notation");
    #"notation"
end method node-type-from-index;

define method node-classes-from-type(type == #"notation")
  values(<notation>, <IXMLDOMNotation>);
end method node-classes-from-type;

define class <Node> (<msxml-proxy>)
  constant virtual slot node-name :: <string>;
  constant virtual slot node-value :: <object>;
  constant virtual slot node-type :: <node-type>;
  constant virtual slot node-parent-node :: <Node>;
  constant virtual slot node-child-nodes :: <sequence>;
  constant virtual slot node-first-child :: <node>;
  constant virtual slot node-last-child :: <node>;
  constant virtual slot node-previous-sibling :: <node>;
  constant virtual slot node-next-sibling :: <node>;
  constant virtual slot node-attributes :: <named-node-map>;
  constant virtual slot node-owner-document :: <document>;
  constant virtual slot node-type-string :: <string>;
  virtual slot node-text :: <string>;
  constant virtual slot node-specified? :: <boolean>;
  constant virtual slot node-definition :: <node>;
  virtual slot node-typed-value :: <object>;
  virtual slot node-data-type :: <object>;
  constant virtual slot node-xml :: <string>;
  constant virtual slot node-parsed? :: <boolean>;
  constant virtual slot node-namespace-uri :: <string>;
  constant virtual slot node-prefix :: <string>;
  constant virtual slot node-base-name :: <string>;
end class <Node>;

define method proxy-class( interface :: <IXMLDOMNode> ) => (class :: <class>)
  <node>    
end proxy-class;

define method make(class == <node>, #key interface) => (n :: <node>)
  let (node-class, interface-class) = 
     node-classes-from-type(node-type-from-index(%node-type(interface)));
  make(node-class, interface: pointer-cast(interface-class, interface));
end method make;

define macro dom-node-methods-definer
  { define dom-node-methods ?:name ?:body end }
   => { 
        define proxy-method (node-name, ?name, ?name ## "/nodeName") end;
        define proxy-method (node-value, ?name, ?name ## "/nodeValue") end;
        define proxy-method (node-type, ?name, ?name ## "/nodeType") end;
        define proxy-method (parent-node, ?name, ?name ## "/parentNode") end;
        define proxy-method (child-nodes, ?name, ?name ## "/childNodes") end;
        define proxy-method (first-child, ?name, ?name ## "/firstChild") end;
        define proxy-method (last-child, ?name, ?name ## "/lastChild") end;
        define proxy-method (previous-sibling, ?name, ?name ## "/previousSibling") end;
        define proxy-method (next-sibling, ?name, ?name ## "/nextSibling") end;
        define proxy-method (attributes, ?name, ?name ## "/attributes") end;
        define proxy-method (insert-before, ?name, ?name ## "/insertBefore") end;
        define proxy-method (replace-child, ?name, ?name ## "/replaceChild") end;
        define proxy-method (remove-child, ?name, ?name ## "/removeChild") end;
        define proxy-method (append-child, ?name, ?name ## "/appendChild") end;
        define proxy-method (has-child-nodes, ?name, ?name ## "/hasChildNodes") end;
        define proxy-method (owner-document, ?name, ?name ## "/ownerDocument") end;
        define proxy-method (clone-node, ?name, ?name ## "/cloneNode") end;
        define proxy-method (node-type-string, ?name, ?name ## "/nodeTypeString") end;
        define proxy-method (text, ?name, ?name ## "/text") end;
        define proxy-method-setter (text-setter, ?name, ?name ## "/text") end;
        define proxy-method (node-specified, ?name, ?name ## "/specified") end;
        define proxy-method (node-definition, ?name, ?name ## "/definition") end;
        define proxy-method (node-typed-value, ?name, ?name ## "/nodeTypedValue") end;
        define proxy-method-setter (node-typed-value-setter, ?name, ?name ## "/nodeTypedValue") end;
        define proxy-method (node-data-type, ?name, ?name ## "/dataType") end;
        define proxy-method-setter (node-data-type-setter, ?name, ?name ## "/dataType") end;
        define proxy-method (xml, ?name, ?name ## "/xml") end;
        define proxy-method (transform-node, ?name, ?name ## "/transformNode") end;
        define proxy-method (select-nodes, ?name, ?name ## "/selectNodes") end;
        define proxy-method (select-single-node, ?name, ?name ## "/selectSingleNode") end;
        define proxy-method (parsed, ?name, ?name ## "/parsed") end;
        define proxy-method (namespace-uri, ?name, ?name ## "/namespaceURI") end;
        define proxy-method (prefix, ?name, ?name ## "/prefix") end;
        define proxy-method (base-name, ?name, ?name ## "/baseName") end;
        define proxy-method (transform-node-to-object, ?name, ?name ## "/transformNodeToObject") end;
      }
end macro dom-node-methods-definer;

define dom-node-methods IXMLDOMNode end;

define function node-list-to-sequence( node-list :: <IXMLDOMNodeList> ) => (s :: <sequence>)
  let length = IXMLDOMNodeList/Length(node-list);
  if(length == 0)
    #()
  else
    let result :: <vector> = make(<vector>, size: length);
    for(n from 0 below length)
      result[n] := make-proxy(node-list[n]);
    finally
      result
    end for;
  end if;
end function node-list-to-sequence;

define method node-name (node :: <Node>) => (result :: <string>)
  convert-to-string(%node-name(node.msxml3-interface));
end method node-name;

define method node-value (node :: <Node>) => (result :: <object>)
  convert-to-safe-type(%node-value(node.msxml3-interface));
end method node-value;

define method node-type (node :: <Node>) => (result :: <node-type>)
  node-type-from-index(%node-type(node.msxml3-interface));
end method node-type;

define method node-parent-node (node :: <Node>) => (result :: <Node>)
  make-proxy(%parent-node(node.msxml3-interface));
end method node-parent-node;

define method node-child-nodes (node :: <Node>) => (result :: <sequence>)
  node-list-to-sequence(%child-nodes(node.msxml3-interface));
end method node-child-nodes;

define method node-first-child(node :: <node>) => (n :: <node>)
  make-proxy(%first-child(node.msxml3-interface));
end method node-first-child;

define method node-last-child(node :: <node>) => (n :: <node>)
  make-proxy(%last-child(node.msxml3-interface));
end method node-last-child;

define method node-previous-sibling(node :: <node>) => (n :: <node>)
  make-proxy(%previous-sibling(node.msxml3-interface));
end method node-previous-sibling;

define method node-next-sibling(node :: <node>) => (n :: <node>)
  make-proxy(%next-sibling(node.msxml3-interface));
end method node-next-sibling;

define method node-attributes (node :: <Node>) => (r :: <named-node-map>)
  make-proxy(%attributes(node.msxml3-interface));
end method node-attributes;

define method node-insert-before( node :: <node>, new-node :: <node>, before :: false-or(<node>) )
  => (r :: <node>)
  make-proxy(%insert-before(node.msxml3-interface, new-node.msxml3-interface, 
                            if(before) before.msxml3-interface else $null-interface end));
end method node-insert-before;

define method node-replace-child( node :: <node>, new-node :: <node>, old-node :: <node>)
  => (r :: <node>)
  make-proxy(%replace-child(node.msxml3-interface, new-node.msxml3-interface, old-node.msxml3-interface));
end method node-replace-child;

define method node-remove-child( node :: <node>, child-node :: <node>)
  => (r :: <node>)
  make-proxy(%remove-child(node.msxml3-interface, child-node.msxml3-interface));
end method node-remove-child;

define method node-append-child( node :: <node>, new-node :: <node>)
  => (r :: <node>)
  make-proxy(%append-child(node.msxml3-interface, pointer-cast(<IXMLDOMNode>, new-node.msxml3-interface)));
end method node-append-child;

define method node-has-child-nodes?( node :: <node> )  => (r :: <boolean>)
  %has-child-nodes(node.msxml3-interface);
end method node-has-child-nodes?;

define method node-owner-document( node :: <node> )  => (r :: <document>)
  make-proxy(%owner-document(node.msxml3-interface));
end method node-owner-document;

define method node-clone-node( node :: <node>, deep :: <boolean>) => (r :: <node>)
  make-proxy(%clone-node(node.msxml3-interface, deep));
end method node-clone-node;

define method node-type-string (node :: <Node>) => (s :: <string>)
  convert-to-string(%node-type-string(node.msxml3-interface));
end method node-type-string;

define method node-text (node :: <Node>) => (s :: <string>)
  convert-to-string(%text(node.msxml3-interface));
end method node-text;

define method node-text-setter( text :: <string>, node :: <Node> ) => (s :: <string>)
  convert-to-string(%text-setter(text, node.msxml3-interface));
end method node-text-setter;

define method node-specified?( node :: <node> )  => (r :: <boolean>)
  %node-specified(node.msxml3-interface);
end method node-specified?;

define method node-definition( node :: <node>) => (r :: <node>)
  make-proxy(%node-definition(node.msxml3-interface));
end method node-definition;

define method node-typed-value (node :: <Node>) => (r :: <object>)
  convert-to-safe-type(%node-typed-value(node.msxml3-interface));
end method node-typed-value;

define method node-typed-value-setter( o :: <object>, node :: <Node> ) => (o :: <object>)
  convert-to-safe-type(node.msxml3-interface.%node-typed-value := o);
end method node-typed-value-setter;

define method node-data-type (node :: <Node>) => (r :: <object>)
  convert-to-safe-type(%node-data-type(node.msxml3-interface));
end method node-data-type;

define method node-data-type-setter( o :: <object>, node :: <Node> ) => (o :: <object>)
  convert-to-safe-type(node.msxml3-interface.%node-data-type := o);
end method node-data-type-setter;

define method node-xml (node :: <Node>) => (s :: <string>)
  convert-to-string(%xml(node.msxml3-interface));
end method node-xml;

define method node-transform-node( node :: <node>, stylesheet-node :: <node>)
  => (s :: <string>)
  convert-to-string(%transform-node(node.msxml3-interface, stylesheet-node.msxml3-interface));
end method node-transform-node;

define method node-select-nodes( node :: <node>, query :: <string>) => (s :: <sequence>)
  node-list-to-sequence(%select-nodes(node.msxml3-interface, query));
end method node-select-nodes;

define method node-select-single-node( node :: <node>, query :: <string>) => (r :: false-or(<node>))
  make-proxy(%select-single-node(node.msxml3-interface, query));
end method node-select-single-node;

define method node-parsed?( node :: <node> )  => (r :: <boolean>)
  %parsed(node.msxml3-interface);
end method node-parsed?;

define method node-namespace-uri (node :: <Node>) => (s :: <string>)
  convert-to-string(%namespace-uri(node.msxml3-interface));
end method node-namespace-uri;

define method node-prefix (node :: <Node>) => (s :: <string>)
  convert-to-string(%prefix(node.msxml3-interface));
end method node-prefix;

define method node-base-name (node :: <Node>) => (s :: <string>)
  convert-to-string(%base-name(node.msxml3-interface));
end method node-base-name;

define method node-transform-node-to-object( node :: <node>, stylesheet-node :: <node>, output :: <object>)
  => ()
  %transform-node-to-object(node.msxml3-interface, stylesheet-node.msxml3-interface, output);
end method node-transform-node-to-object;

