Module: type-library-module
Creator: created from "e:\projects\msxml3-dispatch\type-library.spec" at 1:34 2001- 4-25 New Zealand Standard Time.


/* Type library: MSXML2 version 3.0
 * Description: Microsoft XML, v3.0
 * GUID: {F5078F18-C551-11D3-89B9-0000F81FE221}
 */


/* Dispatch interface: IXMLDOMImplementation version 0.0
 * GUID: {2933BF8F-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMImplementation>
  uuid "{2933BF8F-7B36-11D2-B20E-00C04F983E60}";

  function IXMLDOMImplementation/hasFeature (arg-feature :: <string>, 
        arg-version :: <string>) => (arg-result :: <boolean>), name: 
        "hasFeature", disp-id: 145;
end dispatch-client <IXMLDOMImplementation>;


/* Dispatch interface: IXMLDOMNode version 0.0
 * GUID: {2933BF80-7B36-11D2-B20E-00C04F983E60}
 * Description: Core DOM node interface
 */
define dispatch-client <IXMLDOMNode>
  uuid "{2933BF80-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMNode/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMNode/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IXMLDOMNode/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMNode/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMNode/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMNode/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMNode/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMNode/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMNode/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMNode/attributes :: <IXMLDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMNode/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMNode/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMNode/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMNode/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMNode/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMNode/ownerDocument :: <IXMLDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IXMLDOMNode/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMNode/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMNode/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMNode/specified :: <boolean>, name: "specified", 
        disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMNode/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMNode/nodeTypedValue :: <object>, name: "nodeTypedValue", 
        disp-id: 25;

  /* the data type of the node */
  property IXMLDOMNode/dataType :: <object>, name: "dataType", disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMNode/xml :: <string>, name: "xml", disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMNode/transformNode (arg-stylesheet :: <IXMLDOMNode>) => 
        (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMNode/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMNode/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMNode/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMNode/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMNode/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMNode/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMNode/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;
end dispatch-client <IXMLDOMNode>;


/* Typedef: DOMNodeType
 * Description: Constants that define a node's type
 */
define constant <DOMNodeType> = <tagDOMNodeType>;


/* Enumeration: tagDOMNodeType
 * Description: Constants that define a node's type
 */
define constant <tagDOMNodeType> = type-union(<integer>, <machine-word>);
define constant $NODE-INVALID = 0;
define constant $NODE-ELEMENT = 1;
define constant $NODE-ATTRIBUTE = 2;
define constant $NODE-TEXT = 3;
define constant $NODE-CDATA-SECTION = 4;
define constant $NODE-ENTITY-REFERENCE = 5;
define constant $NODE-ENTITY = 6;
define constant $NODE-PROCESSING-INSTRUCTION = 7;
define constant $NODE-COMMENT = 8;
define constant $NODE-DOCUMENT = 9;
define constant $NODE-DOCUMENT-TYPE = 10;
define constant $NODE-DOCUMENT-FRAGMENT = 11;
define constant $NODE-NOTATION = 12;


/* Dispatch interface: IXMLDOMNodeList version 0.0
 * GUID: {2933BF82-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMNodeList>
  uuid "{2933BF82-7B36-11D2-B20E-00C04F983E60}";

  /* collection of nodes */
  element constant property IXMLDOMNodeList/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IXMLDOMNode>, name: 
        "item", disp-id: 0;

  /* number of nodes in the collection */
  constant property IXMLDOMNodeList/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 74;

  /* get next node from iterator */
  function IXMLDOMNodeList/nextNode () => (arg-result :: <IXMLDOMNode>), 
        name: "nextNode", disp-id: 76;

  /* reset the position of iterator */
  function IXMLDOMNodeList/reset () => (), name: "reset", disp-id: 77;

  constant property IXMLDOMNodeList/_newEnum :: <LPUNKNOWN>, name: 
        "_newEnum", disp-id: -4;
end dispatch-client <IXMLDOMNodeList>;


/* Dispatch interface: IXMLDOMNamedNodeMap version 0.0
 * GUID: {2933BF83-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMNamedNodeMap>
  uuid "{2933BF83-7B36-11D2-B20E-00C04F983E60}";

  /* lookup item by name */
  function IXMLDOMNamedNodeMap/getNamedItem (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "getNamedItem", disp-id: 83;

  /* set item by name */
  function IXMLDOMNamedNodeMap/setNamedItem (arg-newItem :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "setNamedItem", disp-id: 
        84;

  /* remove item by name */
  function IXMLDOMNamedNodeMap/removeNamedItem (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeNamedItem", disp-id: 
        85;

  /* collection of nodes */
  element constant property IXMLDOMNamedNodeMap/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IXMLDOMNode>, name: 
        "item", disp-id: 0;

  /* number of nodes in the collection */
  constant property IXMLDOMNamedNodeMap/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 74;

  /* lookup the item by name and namespace */
  function IXMLDOMNamedNodeMap/getQualifiedItem (arg-baseName :: <string>, 
        arg-namespaceURI :: <string>) => (arg-result :: <IXMLDOMNode>), 
        name: "getQualifiedItem", disp-id: 87;

  /* remove the item by name and namespace */
  function IXMLDOMNamedNodeMap/removeQualifiedItem (arg-baseName :: 
        <string>, arg-namespaceURI :: <string>) => (arg-result :: 
        <IXMLDOMNode>), name: "removeQualifiedItem", disp-id: 88;

  /* get next node from iterator */
  function IXMLDOMNamedNodeMap/nextNode () => (arg-result :: 
        <IXMLDOMNode>), name: "nextNode", disp-id: 89;

  /* reset the position of iterator */
  function IXMLDOMNamedNodeMap/reset () => (), name: "reset", disp-id: 90;

  constant property IXMLDOMNamedNodeMap/_newEnum :: <LPUNKNOWN>, name: 
        "_newEnum", disp-id: -4;
end dispatch-client <IXMLDOMNamedNodeMap>;


/* hidden? Dispatch interface: IXMLDOMDocument version 0.0
 * GUID: {2933BF81-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMDocument>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentElement. */
  uuid "{2933BF81-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMDocument/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMDocument/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMDocument/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMDocument/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMDocument/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMDocument/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMDocument/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMDocument/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMDocument/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMDocument/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMDocument/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMDocument/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMDocument/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMDocument/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMDocument/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMDocument/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMDocument/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMDocument/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMDocument/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMDocument/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMDocument/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMDocument/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMDocument/dataType :: <object>, name: "dataType", disp-id: 
        26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMDocument/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMDocument/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMDocument/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMDocument/selectSingleNode (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", 
        disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMDocument/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMDocument/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMDocument/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMDocument/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMDocument/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* node corresponding to the DOCTYPE */
  constant property IXMLDOMDocument/doctype :: <IXMLDOMDocumentType>, name: 
        "doctype", disp-id: 38;

  /* info on this DOM implementation */
  constant property IXMLDOMDocument/implementation :: 
        <IXMLDOMImplementation>, name: "implementation", disp-id: 39;

  /* the root of the tree */
  constant property IXMLDOMDocument/documentElement :: <IXMLDOMElement>, 
        name: "documentElement", disp-id: 40;

  /* create an Element node */
  function IXMLDOMDocument/createElement (arg-tagName :: <string>) => 
        (arg-result :: <IXMLDOMElement>), name: "createElement", disp-id: 
        41;

  /* create a DocumentFragment node */
  function IXMLDOMDocument/createDocumentFragment () => (arg-result :: 
        <IXMLDOMDocumentFragment>), name: "createDocumentFragment", 
        disp-id: 42;

  /* create a text node */
  function IXMLDOMDocument/createTextNode (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMText>), name: "createTextNode", disp-id: 43;

  /* create a comment node */
  function IXMLDOMDocument/createComment (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMComment>), name: "createComment", disp-id: 
        44;

  /* create a CDATA section node */
  function IXMLDOMDocument/createCDATASection (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMCDATASection>), name: "createCDATASection", 
        disp-id: 45;

  /* create a processing instruction node */
  function IXMLDOMDocument/createProcessingInstruction (arg-target :: 
        <string>, arg-data :: <string>) => (arg-result :: 
        <IXMLDOMProcessingInstruction>), name: 
        "createProcessingInstruction", disp-id: 46;

  /* create an attribute node */
  function IXMLDOMDocument/createAttribute (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMAttribute>), name: "createAttribute", 
        disp-id: 47;

  /* create an entity reference node */
  function IXMLDOMDocument/createEntityReference (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMEntityReference>), name: 
        "createEntityReference", disp-id: 49;

  /* build a list of elements by name */
  function IXMLDOMDocument/getElementsByTagName (arg-tagName :: <string>) 
        => (arg-result :: <IXMLDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 50;

  /* create a node of the specified node type and name */
  function IXMLDOMDocument/createNode (arg-type :: <object>, arg-name :: 
        <string>, arg-namespaceURI :: <string>) => (arg-result :: 
        <IXMLDOMNode>), name: "createNode", disp-id: 54;

  /* retrieve node from it's ID */
  function IXMLDOMDocument/nodeFromID (arg-idString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "nodeFromID", disp-id: 56;

  /* load document from the specified XML source */
  function IXMLDOMDocument/load (arg-xmlSource :: <object>) => (arg-result 
        :: <boolean>), name: "load", disp-id: 58;

  /* get the state of the XML document */
  constant property IXMLDOMDocument/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: -525;

  /* get the last parser error */
  constant property IXMLDOMDocument/parseError :: <IXMLDOMParseError>, 
        name: "parseError", disp-id: 59;

  /* get the URL for the loaded XML document */
  constant property IXMLDOMDocument/url :: <string>, name: "url", disp-id: 
        60;

  /* flag for asynchronous download */
  property IXMLDOMDocument/async :: <boolean>, name: "async", disp-id: 61;

  /* abort an asynchronous download */
  function IXMLDOMDocument/abort () => (), name: "abort", disp-id: 62;

  /* load the document from a string */
  function IXMLDOMDocument/loadXML (arg-bstrXML :: <string>) => (arg-result 
        :: <boolean>), name: "loadXML", disp-id: 63;

  /* save the document to a specified destination */
  function IXMLDOMDocument/save (arg-destination :: <object>) => (), name: 
        "save", disp-id: 64;

  /* indicates whether the parser performs validation */
  property IXMLDOMDocument/validateOnParse :: <boolean>, name: 
        "validateOnParse", disp-id: 65;

  /* indicates whether the parser resolves references to external 
        DTD/Entities/Schema */
  property IXMLDOMDocument/resolveExternals :: <boolean>, name: 
        "resolveExternals", disp-id: 66;

  /* indicates whether the parser preserves whitespace */
  property IXMLDOMDocument/preserveWhiteSpace :: <boolean>, name: 
        "preserveWhiteSpace", disp-id: 67;

  /* register a readystatechange event handler */
  write-only property IXMLDOMDocument/onreadystatechange :: <object>, name: 
        "onreadystatechange", disp-id: 68;

  /* register an ondataavailable event handler */
  write-only property IXMLDOMDocument/ondataavailable :: <object>, name: 
        "ondataavailable", disp-id: 69;

  /* register an ontransformnode event handler */
  write-only property IXMLDOMDocument/ontransformnode :: <object>, name: 
        "ontransformnode", disp-id: 70;
end dispatch-client <IXMLDOMDocument>;


/* Dispatch interface: IXMLDOMDocumentType version 0.0
 * GUID: {2933BF8B-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMDocumentType>
  uuid "{2933BF8B-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMDocumentType/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMDocumentType/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMDocumentType/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMDocumentType/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMDocumentType/childNodes :: <IXMLDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMDocumentType/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMDocumentType/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMDocumentType/previousSibling :: <IXMLDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMDocumentType/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMDocumentType/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMDocumentType/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMDocumentType/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMDocumentType/removeChild (arg-childNode :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMDocumentType/appendChild (arg-newChild :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMDocumentType/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMDocumentType/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMDocumentType/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMDocumentType/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMDocumentType/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMDocumentType/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMDocumentType/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMDocumentType/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMDocumentType/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMDocumentType/xml :: <string>, name: "xml", 
        disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMDocumentType/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMDocumentType/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMDocumentType/selectSingleNode (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMDocumentType/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMDocumentType/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMDocumentType/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMDocumentType/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMDocumentType/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* name of the document type (root of the tree) */
  constant property IXMLDOMDocumentType/name :: <string>, name: "name", 
        disp-id: 131;

  /* a list of entities in the document */
  constant property IXMLDOMDocumentType/entities :: <IXMLDOMNamedNodeMap>, 
        name: "entities", disp-id: 132;

  /* a list of notations in the document */
  constant property IXMLDOMDocumentType/notations :: <IXMLDOMNamedNodeMap>, 
        name: "notations", disp-id: 133;
end dispatch-client <IXMLDOMDocumentType>;


/* Dispatch interface: IXMLDOMElement version 0.0
 * GUID: {2933BF86-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMElement>
  uuid "{2933BF86-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMElement/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMElement/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMElement/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMElement/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMElement/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMElement/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMElement/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMElement/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMElement/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMElement/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMElement/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMElement/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMElement/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMElement/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMElement/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMElement/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMElement/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMElement/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMElement/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMElement/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMElement/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMElement/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMElement/dataType :: <object>, name: "dataType", disp-id: 
        26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMElement/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMElement/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMElement/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMElement/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMElement/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMElement/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMElement/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMElement/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMElement/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* get the tagName of the element */
  constant property IXMLDOMElement/tagName :: <string>, name: "tagName", 
        disp-id: 97;

  /* look up the string value of an attribute by name */
  function IXMLDOMElement/getAttribute (arg-name :: <string>) => 
        (arg-result :: <object>), name: "getAttribute", disp-id: 99;

  /* set the string value of an attribute by name */
  function IXMLDOMElement/setAttribute (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setAttribute", disp-id: 100;

  /* remove an attribute by name */
  function IXMLDOMElement/removeAttribute (arg-name :: <string>) => (), 
        name: "removeAttribute", disp-id: 101;

  /* look up the attribute node by name */
  function IXMLDOMElement/getAttributeNode (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMAttribute>), name: "getAttributeNode", 
        disp-id: 102;

  /* set the specified attribute on the element */
  function IXMLDOMElement/setAttributeNode (arg-DOMAttribute :: 
        <IXMLDOMAttribute>) => (arg-result :: <IXMLDOMAttribute>), name: 
        "setAttributeNode", disp-id: 103;

  /* remove the specified attribute */
  function IXMLDOMElement/removeAttributeNode (arg-DOMAttribute :: 
        <IXMLDOMAttribute>) => (arg-result :: <IXMLDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 104;

  /* build a list of elements by name */
  function IXMLDOMElement/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 105;

  /* collapse all adjacent text nodes in sub-tree */
  function IXMLDOMElement/normalize () => (), name: "normalize", disp-id: 
        106;
end dispatch-client <IXMLDOMElement>;


/* Dispatch interface: IXMLDOMAttribute version 0.0
 * GUID: {2933BF85-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMAttribute>
  uuid "{2933BF85-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMAttribute/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMAttribute/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMAttribute/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMAttribute/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMAttribute/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMAttribute/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMAttribute/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMAttribute/previousSibling :: <IXMLDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMAttribute/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMAttribute/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMAttribute/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMAttribute/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMAttribute/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMAttribute/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMAttribute/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMAttribute/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMAttribute/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMAttribute/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMAttribute/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMAttribute/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMAttribute/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMAttribute/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMAttribute/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMAttribute/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMAttribute/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMAttribute/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMAttribute/selectSingleNode (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", 
        disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMAttribute/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMAttribute/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMAttribute/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMAttribute/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMAttribute/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* get name of the attribute */
  constant property IXMLDOMAttribute/name :: <string>, name: "name", 
        disp-id: 118;

  /* string value of the attribute */
  property IXMLDOMAttribute/value :: <object>, name: "value", disp-id: 120;
end dispatch-client <IXMLDOMAttribute>;


/* Dispatch interface: IXMLDOMDocumentFragment version 0.0
 * GUID: {3EFAA413-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IXMLDOMDocumentFragment>
  uuid "{3EFAA413-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IXMLDOMDocumentFragment/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMDocumentFragment/nodeValue :: <object>, name: 
        "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IXMLDOMDocumentFragment/nodeType :: <DOMNodeType>, 
        name: "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMDocumentFragment/parentNode :: <IXMLDOMNode>, 
        name: "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMDocumentFragment/childNodes :: 
        <IXMLDOMNodeList>, name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMDocumentFragment/firstChild :: <IXMLDOMNode>, 
        name: "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMDocumentFragment/lastChild :: <IXMLDOMNode>, 
        name: "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMDocumentFragment/previousSibling :: 
        <IXMLDOMNode>, name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMDocumentFragment/nextSibling :: <IXMLDOMNode>, 
        name: "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMDocumentFragment/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMDocumentFragment/insertBefore (arg-newChild :: 
        <IXMLDOMNode>, arg-refChild :: <object>) => (arg-result :: 
        <IXMLDOMNode>), name: "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMDocumentFragment/replaceChild (arg-newChild :: 
        <IXMLDOMNode>, arg-oldChild :: <IXMLDOMNode>) => (arg-result :: 
        <IXMLDOMNode>), name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMDocumentFragment/removeChild (arg-childNode :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMDocumentFragment/appendChild (arg-newChild :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "appendChild", disp-id: 16;

  function IXMLDOMDocumentFragment/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMDocumentFragment/ownerDocument :: 
        <IXMLDOMDocument>, name: "ownerDocument", disp-id: 18;

  function IXMLDOMDocumentFragment/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMDocumentFragment/nodeTypeString :: <string>, 
        name: "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMDocumentFragment/text :: <string>, name: "text", disp-id: 
        24;

  /* indicates whether node is a default value */
  constant property IXMLDOMDocumentFragment/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMDocumentFragment/definition :: <IXMLDOMNode>, 
        name: "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMDocumentFragment/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMDocumentFragment/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMDocumentFragment/xml :: <string>, name: "xml", 
        disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMDocumentFragment/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMDocumentFragment/selectNodes (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNodeList>), name: 
        "selectNodes", disp-id: 29;

  /* execute query on the subtree */
  function IXMLDOMDocumentFragment/selectSingleNode (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMDocumentFragment/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMDocumentFragment/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMDocumentFragment/prefix :: <string>, name: 
        "prefix", disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMDocumentFragment/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMDocumentFragment/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;
end dispatch-client <IXMLDOMDocumentFragment>;


/* Dispatch interface: IXMLDOMText version 0.0
 * GUID: {2933BF87-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMText>
  uuid "{2933BF87-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMText/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMText/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IXMLDOMText/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMText/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMText/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMText/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMText/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMText/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMText/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMText/attributes :: <IXMLDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMText/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMText/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMText/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMText/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMText/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMText/ownerDocument :: <IXMLDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IXMLDOMText/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMText/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMText/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMText/specified :: <boolean>, name: "specified", 
        disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMText/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMText/nodeTypedValue :: <object>, name: "nodeTypedValue", 
        disp-id: 25;

  /* the data type of the node */
  property IXMLDOMText/dataType :: <object>, name: "dataType", disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMText/xml :: <string>, name: "xml", disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMText/transformNode (arg-stylesheet :: <IXMLDOMNode>) => 
        (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMText/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMText/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMText/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMText/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMText/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMText/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMText/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* value of the node */
  property IXMLDOMText/data :: <string>, name: "data", disp-id: 109;

  /* number of characters in value */
  constant property IXMLDOMText/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 110;

  /* retrieve substring of value */
  function IXMLDOMText/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 111;

  /* append string to value */
  function IXMLDOMText/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 112;

  /* insert string into value */
  function IXMLDOMText/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 113;

  /* delete string within the value */
  function IXMLDOMText/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 114;

  /* replace string within the value */
  function IXMLDOMText/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 115;

  /* split the text node into two text nodes at the position specified */
  function IXMLDOMText/splitText (arg-offset :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IXMLDOMText>), name: 
        "splitText", disp-id: 123;
end dispatch-client <IXMLDOMText>;


/* Dispatch interface: IXMLDOMCharacterData version 0.0
 * GUID: {2933BF84-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMCharacterData>
  uuid "{2933BF84-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMCharacterData/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMCharacterData/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMCharacterData/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMCharacterData/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMCharacterData/childNodes :: <IXMLDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMCharacterData/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMCharacterData/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMCharacterData/previousSibling :: <IXMLDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMCharacterData/nextSibling :: <IXMLDOMNode>, 
        name: "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMCharacterData/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMCharacterData/insertBefore (arg-newChild :: 
        <IXMLDOMNode>, arg-refChild :: <object>) => (arg-result :: 
        <IXMLDOMNode>), name: "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMCharacterData/replaceChild (arg-newChild :: 
        <IXMLDOMNode>, arg-oldChild :: <IXMLDOMNode>) => (arg-result :: 
        <IXMLDOMNode>), name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMCharacterData/removeChild (arg-childNode :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMCharacterData/appendChild (arg-newChild :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMCharacterData/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMCharacterData/ownerDocument :: 
        <IXMLDOMDocument>, name: "ownerDocument", disp-id: 18;

  function IXMLDOMCharacterData/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMCharacterData/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMCharacterData/text :: <string>, name: "text", disp-id: 
        24;

  /* indicates whether node is a default value */
  constant property IXMLDOMCharacterData/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMCharacterData/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMCharacterData/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMCharacterData/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMCharacterData/xml :: <string>, name: "xml", 
        disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMCharacterData/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMCharacterData/selectNodes (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMCharacterData/selectSingleNode (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMCharacterData/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMCharacterData/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMCharacterData/prefix :: <string>, name: 
        "prefix", disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMCharacterData/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMCharacterData/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* value of the node */
  property IXMLDOMCharacterData/data :: <string>, name: "data", disp-id: 
        109;

  /* number of characters in value */
  constant property IXMLDOMCharacterData/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 110;

  /* retrieve substring of value */
  function IXMLDOMCharacterData/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 111;

  /* append string to value */
  function IXMLDOMCharacterData/appendData (arg-data :: <string>) => (), 
        name: "appendData", disp-id: 112;

  /* insert string into value */
  function IXMLDOMCharacterData/insertData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "insertData", disp-id: 113;

  /* delete string within the value */
  function IXMLDOMCharacterData/deleteData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (), name: "deleteData", 
        disp-id: 114;

  /* replace string within the value */
  function IXMLDOMCharacterData/replaceData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "replaceData", disp-id: 115;
end dispatch-client <IXMLDOMCharacterData>;


/* Dispatch interface: IXMLDOMComment version 0.0
 * GUID: {2933BF88-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMComment>
  uuid "{2933BF88-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMComment/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMComment/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMComment/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMComment/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMComment/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMComment/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMComment/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMComment/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMComment/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMComment/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMComment/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMComment/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMComment/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMComment/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMComment/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMComment/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMComment/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMComment/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMComment/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMComment/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMComment/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMComment/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMComment/dataType :: <object>, name: "dataType", disp-id: 
        26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMComment/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMComment/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMComment/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMComment/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMComment/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMComment/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMComment/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMComment/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMComment/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* value of the node */
  property IXMLDOMComment/data :: <string>, name: "data", disp-id: 109;

  /* number of characters in value */
  constant property IXMLDOMComment/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 110;

  /* retrieve substring of value */
  function IXMLDOMComment/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 111;

  /* append string to value */
  function IXMLDOMComment/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 112;

  /* insert string into value */
  function IXMLDOMComment/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 113;

  /* delete string within the value */
  function IXMLDOMComment/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 114;

  /* replace string within the value */
  function IXMLDOMComment/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 115;
end dispatch-client <IXMLDOMComment>;


/* Dispatch interface: IXMLDOMCDATASection version 0.0
 * GUID: {2933BF8A-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMCDATASection>
  uuid "{2933BF8A-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMCDATASection/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMCDATASection/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMCDATASection/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMCDATASection/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMCDATASection/childNodes :: <IXMLDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMCDATASection/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMCDATASection/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMCDATASection/previousSibling :: <IXMLDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMCDATASection/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMCDATASection/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMCDATASection/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMCDATASection/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMCDATASection/removeChild (arg-childNode :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMCDATASection/appendChild (arg-newChild :: <IXMLDOMNode>) 
        => (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMCDATASection/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMCDATASection/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMCDATASection/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMCDATASection/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMCDATASection/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMCDATASection/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMCDATASection/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMCDATASection/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMCDATASection/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMCDATASection/xml :: <string>, name: "xml", 
        disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMCDATASection/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMCDATASection/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMCDATASection/selectSingleNode (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMCDATASection/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMCDATASection/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMCDATASection/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMCDATASection/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMCDATASection/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* value of the node */
  property IXMLDOMCDATASection/data :: <string>, name: "data", disp-id: 
        109;

  /* number of characters in value */
  constant property IXMLDOMCDATASection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 110;

  /* retrieve substring of value */
  function IXMLDOMCDATASection/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 111;

  /* append string to value */
  function IXMLDOMCDATASection/appendData (arg-data :: <string>) => (), 
        name: "appendData", disp-id: 112;

  /* insert string into value */
  function IXMLDOMCDATASection/insertData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "insertData", disp-id: 113;

  /* delete string within the value */
  function IXMLDOMCDATASection/deleteData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (), name: "deleteData", 
        disp-id: 114;

  /* replace string within the value */
  function IXMLDOMCDATASection/replaceData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "replaceData", disp-id: 115;

  /* split the text node into two text nodes at the position specified */
  function IXMLDOMCDATASection/splitText (arg-offset :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <IXMLDOMText>), name: "splitText", disp-id: 123;
end dispatch-client <IXMLDOMCDATASection>;


/* Dispatch interface: IXMLDOMProcessingInstruction version 0.0
 * GUID: {2933BF89-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMProcessingInstruction>
  uuid "{2933BF89-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMProcessingInstruction/nodeName :: <string>, 
        name: "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMProcessingInstruction/nodeValue :: <object>, name: 
        "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IXMLDOMProcessingInstruction/nodeType :: <DOMNodeType>, 
        name: "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMProcessingInstruction/parentNode :: 
        <IXMLDOMNode>, name: "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMProcessingInstruction/childNodes :: 
        <IXMLDOMNodeList>, name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMProcessingInstruction/firstChild :: 
        <IXMLDOMNode>, name: "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMProcessingInstruction/lastChild :: 
        <IXMLDOMNode>, name: "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMProcessingInstruction/previousSibling :: 
        <IXMLDOMNode>, name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMProcessingInstruction/nextSibling :: 
        <IXMLDOMNode>, name: "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMProcessingInstruction/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMProcessingInstruction/insertBefore (arg-newChild :: 
        <IXMLDOMNode>, arg-refChild :: <object>) => (arg-result :: 
        <IXMLDOMNode>), name: "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMProcessingInstruction/replaceChild (arg-newChild :: 
        <IXMLDOMNode>, arg-oldChild :: <IXMLDOMNode>) => (arg-result :: 
        <IXMLDOMNode>), name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMProcessingInstruction/removeChild (arg-childNode :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMProcessingInstruction/appendChild (arg-newChild :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "appendChild", disp-id: 16;

  function IXMLDOMProcessingInstruction/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMProcessingInstruction/ownerDocument :: 
        <IXMLDOMDocument>, name: "ownerDocument", disp-id: 18;

  function IXMLDOMProcessingInstruction/cloneNode (arg-deep :: <boolean>) 
        => (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMProcessingInstruction/nodeTypeString :: 
        <string>, name: "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMProcessingInstruction/text :: <string>, name: "text", 
        disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMProcessingInstruction/specified :: <boolean>, 
        name: "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMProcessingInstruction/definition :: 
        <IXMLDOMNode>, name: "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMProcessingInstruction/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMProcessingInstruction/dataType :: <object>, name: 
        "dataType", disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMProcessingInstruction/xml :: <string>, name: 
        "xml", disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMProcessingInstruction/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMProcessingInstruction/selectNodes (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNodeList>), name: 
        "selectNodes", disp-id: 29;

  /* execute query on the subtree */
  function IXMLDOMProcessingInstruction/selectSingleNode (arg-queryString 
        :: <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMProcessingInstruction/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMProcessingInstruction/namespaceURI :: <string>, 
        name: "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMProcessingInstruction/prefix :: <string>, name: 
        "prefix", disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMProcessingInstruction/baseName :: <string>, 
        name: "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMProcessingInstruction/transformNodeToObject 
        (arg-stylesheet :: <IXMLDOMNode>, arg-outputObject :: <object>) => 
        (), name: "transformNodeToObject", disp-id: 35;

  /* the target */
  constant property IXMLDOMProcessingInstruction/target :: <string>, name: 
        "target", disp-id: 127;

  /* the data */
  property IXMLDOMProcessingInstruction/data :: <string>, name: "data", 
        disp-id: 128;
end dispatch-client <IXMLDOMProcessingInstruction>;


/* Dispatch interface: IXMLDOMEntityReference version 0.0
 * GUID: {2933BF8E-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMEntityReference>
  uuid "{2933BF8E-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMEntityReference/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMEntityReference/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMEntityReference/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMEntityReference/parentNode :: <IXMLDOMNode>, 
        name: "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMEntityReference/childNodes :: <IXMLDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMEntityReference/firstChild :: <IXMLDOMNode>, 
        name: "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMEntityReference/lastChild :: <IXMLDOMNode>, 
        name: "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMEntityReference/previousSibling :: 
        <IXMLDOMNode>, name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMEntityReference/nextSibling :: <IXMLDOMNode>, 
        name: "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMEntityReference/attributes :: 
        <IXMLDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMEntityReference/insertBefore (arg-newChild :: 
        <IXMLDOMNode>, arg-refChild :: <object>) => (arg-result :: 
        <IXMLDOMNode>), name: "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMEntityReference/replaceChild (arg-newChild :: 
        <IXMLDOMNode>, arg-oldChild :: <IXMLDOMNode>) => (arg-result :: 
        <IXMLDOMNode>), name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMEntityReference/removeChild (arg-childNode :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMEntityReference/appendChild (arg-newChild :: 
        <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), name: 
        "appendChild", disp-id: 16;

  function IXMLDOMEntityReference/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMEntityReference/ownerDocument :: 
        <IXMLDOMDocument>, name: "ownerDocument", disp-id: 18;

  function IXMLDOMEntityReference/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMEntityReference/nodeTypeString :: <string>, 
        name: "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMEntityReference/text :: <string>, name: "text", disp-id: 
        24;

  /* indicates whether node is a default value */
  constant property IXMLDOMEntityReference/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMEntityReference/definition :: <IXMLDOMNode>, 
        name: "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMEntityReference/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMEntityReference/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMEntityReference/xml :: <string>, name: "xml", 
        disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMEntityReference/transformNode (arg-stylesheet :: 
        <IXMLDOMNode>) => (arg-result :: <string>), name: "transformNode", 
        disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMEntityReference/selectNodes (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMEntityReference/selectSingleNode (arg-queryString :: 
        <string>) => (arg-result :: <IXMLDOMNode>), name: 
        "selectSingleNode", disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMEntityReference/parsed :: <boolean>, name: 
        "parsed", disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMEntityReference/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMEntityReference/prefix :: <string>, name: 
        "prefix", disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMEntityReference/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMEntityReference/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;
end dispatch-client <IXMLDOMEntityReference>;


/* Dispatch interface: IXMLDOMParseError version 0.0
 * GUID: {3EFAA426-272F-11D2-836F-0000F87A7782}
 * Description: structure for reporting parser errors
 */
define dispatch-client <IXMLDOMParseError>
  uuid "{3EFAA426-272F-11D2-836F-0000F87A7782}";

  /* the error code */
  constant property IXMLDOMParseError/errorCode :: type-union(<integer>, 
        <machine-word>), name: "errorCode", disp-id: 0;

  /* the URL of the XML document containing the error */
  constant property IXMLDOMParseError/url :: <string>, name: "url", 
        disp-id: 179;

  /* the cause of the error */
  constant property IXMLDOMParseError/reason :: <string>, name: "reason", 
        disp-id: 180;

  /* the data where the error occurred */
  constant property IXMLDOMParseError/srcText :: <string>, name: "srcText", 
        disp-id: 181;

  /* the line number in the XML document where the error occurred */
  constant property IXMLDOMParseError/line :: type-union(<integer>, 
        <machine-word>), name: "line", disp-id: 182;

  /* the character position in the line containing the error */
  constant property IXMLDOMParseError/linepos :: type-union(<integer>, 
        <machine-word>), name: "linepos", disp-id: 183;

  /* the absolute file position in the XML document containing the error */
  constant property IXMLDOMParseError/filepos :: type-union(<integer>, 
        <machine-word>), name: "filepos", disp-id: 184;
end dispatch-client <IXMLDOMParseError>;


/* Dispatch interface: IXMLDOMSchemaCollection version 0.0
 * GUID: {373984C8-B845-449B-91E7-45AC83036ADE}
 * Description: XML Schemas Collection
 */
define dispatch-client <IXMLDOMSchemaCollection>
  uuid "{373984C8-B845-449B-91E7-45AC83036ADE}";

  /* add a new schema */
  function IXMLDOMSchemaCollection/add (arg-namespaceURI :: <string>, 
        arg-var :: <object>) => (), name: "add", disp-id: 3;

  /* lookup schema by namespaceURI */
  function IXMLDOMSchemaCollection/get (arg-namespaceURI :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "get", disp-id: 4;

  /* remove schema by namespaceURI */
  function IXMLDOMSchemaCollection/remove (arg-namespaceURI :: <string>) => 
        (), name: "remove", disp-id: 5;

  /* number of schema in collection */
  constant property IXMLDOMSchemaCollection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 6;

  /* Get namespaceURI for schema by index */
  element constant property IXMLDOMSchemaCollection/namespaceURI (arg-index 
        :: type-union(<integer>, <machine-word>)) :: <string>, name: 
        "namespaceURI", disp-id: 0;

  /* copye & merge other collection into this one */
  function IXMLDOMSchemaCollection/addCollection (arg-otherCollection :: 
        <IXMLDOMSchemaCollection>) => (), name: "addCollection", disp-id: 
        8;

  constant property IXMLDOMSchemaCollection/_newEnum :: <LPUNKNOWN>, name: 
        "_newEnum", disp-id: -4;
end dispatch-client <IXMLDOMSchemaCollection>;


/* Dispatch interface: IXMLDOMDocument2 version 0.0
 * GUID: {2933BF95-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMDocument2>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentElement. */
  /* Translation error: Cannot translate PROPERTYPUTREF method schemas. */
  uuid "{2933BF95-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMDocument2/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IXMLDOMDocument2/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMDocument2/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMDocument2/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMDocument2/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMDocument2/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMDocument2/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMDocument2/previousSibling :: <IXMLDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMDocument2/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMDocument2/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMDocument2/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMDocument2/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMDocument2/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMDocument2/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMDocument2/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMDocument2/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMDocument2/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMDocument2/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMDocument2/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMDocument2/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMDocument2/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMDocument2/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMDocument2/dataType :: <object>, name: "dataType", 
        disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMDocument2/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMDocument2/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMDocument2/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMDocument2/selectSingleNode (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", 
        disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMDocument2/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMDocument2/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMDocument2/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMDocument2/baseName :: <string>, name: 
        "baseName", disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMDocument2/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* node corresponding to the DOCTYPE */
  constant property IXMLDOMDocument2/doctype :: <IXMLDOMDocumentType>, 
        name: "doctype", disp-id: 38;

  /* info on this DOM implementation */
  constant property IXMLDOMDocument2/implementation :: 
        <IXMLDOMImplementation>, name: "implementation", disp-id: 39;

  /* the root of the tree */
  constant property IXMLDOMDocument2/documentElement :: <IXMLDOMElement>, 
        name: "documentElement", disp-id: 40;

  /* create an Element node */
  function IXMLDOMDocument2/createElement (arg-tagName :: <string>) => 
        (arg-result :: <IXMLDOMElement>), name: "createElement", disp-id: 
        41;

  /* create a DocumentFragment node */
  function IXMLDOMDocument2/createDocumentFragment () => (arg-result :: 
        <IXMLDOMDocumentFragment>), name: "createDocumentFragment", 
        disp-id: 42;

  /* create a text node */
  function IXMLDOMDocument2/createTextNode (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMText>), name: "createTextNode", disp-id: 43;

  /* create a comment node */
  function IXMLDOMDocument2/createComment (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMComment>), name: "createComment", disp-id: 
        44;

  /* create a CDATA section node */
  function IXMLDOMDocument2/createCDATASection (arg-data :: <string>) => 
        (arg-result :: <IXMLDOMCDATASection>), name: "createCDATASection", 
        disp-id: 45;

  /* create a processing instruction node */
  function IXMLDOMDocument2/createProcessingInstruction (arg-target :: 
        <string>, arg-data :: <string>) => (arg-result :: 
        <IXMLDOMProcessingInstruction>), name: 
        "createProcessingInstruction", disp-id: 46;

  /* create an attribute node */
  function IXMLDOMDocument2/createAttribute (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMAttribute>), name: "createAttribute", 
        disp-id: 47;

  /* create an entity reference node */
  function IXMLDOMDocument2/createEntityReference (arg-name :: <string>) => 
        (arg-result :: <IXMLDOMEntityReference>), name: 
        "createEntityReference", disp-id: 49;

  /* build a list of elements by name */
  function IXMLDOMDocument2/getElementsByTagName (arg-tagName :: <string>) 
        => (arg-result :: <IXMLDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 50;

  /* create a node of the specified node type and name */
  function IXMLDOMDocument2/createNode (arg-type :: <object>, arg-name :: 
        <string>, arg-namespaceURI :: <string>) => (arg-result :: 
        <IXMLDOMNode>), name: "createNode", disp-id: 54;

  /* retrieve node from it's ID */
  function IXMLDOMDocument2/nodeFromID (arg-idString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "nodeFromID", disp-id: 56;

  /* load document from the specified XML source */
  function IXMLDOMDocument2/load (arg-xmlSource :: <object>) => (arg-result 
        :: <boolean>), name: "load", disp-id: 58;

  /* get the state of the XML document */
  constant property IXMLDOMDocument2/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: -525;

  /* get the last parser error */
  constant property IXMLDOMDocument2/parseError :: <IXMLDOMParseError>, 
        name: "parseError", disp-id: 59;

  /* get the URL for the loaded XML document */
  constant property IXMLDOMDocument2/url :: <string>, name: "url", disp-id: 
        60;

  /* flag for asynchronous download */
  property IXMLDOMDocument2/async :: <boolean>, name: "async", disp-id: 61;

  /* abort an asynchronous download */
  function IXMLDOMDocument2/abort () => (), name: "abort", disp-id: 62;

  /* load the document from a string */
  function IXMLDOMDocument2/loadXML (arg-bstrXML :: <string>) => 
        (arg-result :: <boolean>), name: "loadXML", disp-id: 63;

  /* save the document to a specified destination */
  function IXMLDOMDocument2/save (arg-destination :: <object>) => (), name: 
        "save", disp-id: 64;

  /* indicates whether the parser performs validation */
  property IXMLDOMDocument2/validateOnParse :: <boolean>, name: 
        "validateOnParse", disp-id: 65;

  /* indicates whether the parser resolves references to external 
        DTD/Entities/Schema */
  property IXMLDOMDocument2/resolveExternals :: <boolean>, name: 
        "resolveExternals", disp-id: 66;

  /* indicates whether the parser preserves whitespace */
  property IXMLDOMDocument2/preserveWhiteSpace :: <boolean>, name: 
        "preserveWhiteSpace", disp-id: 67;

  /* register a readystatechange event handler */
  write-only property IXMLDOMDocument2/onreadystatechange :: <object>, 
        name: "onreadystatechange", disp-id: 68;

  /* register an ondataavailable event handler */
  write-only property IXMLDOMDocument2/ondataavailable :: <object>, name: 
        "ondataavailable", disp-id: 69;

  /* register an ontransformnode event handler */
  write-only property IXMLDOMDocument2/ontransformnode :: <object>, name: 
        "ontransformnode", disp-id: 70;

  /* A collection of all namespaces for this document */
  constant property IXMLDOMDocument2/namespaces :: 
        <IXMLDOMSchemaCollection>, name: "namespaces", disp-id: 201;

  /* The associated schema cache */
  constant property IXMLDOMDocument2/schemas :: <object>, name: "schemas", 
        disp-id: 202;

  /* perform runtime validation on the currently loaded XML document */
  function IXMLDOMDocument2/validate () => (arg-result :: 
        <IXMLDOMParseError>), name: "validate", disp-id: 203;

  /* set the value of the named property */
  function IXMLDOMDocument2/setProperty (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setProperty", disp-id: 204;

  /* get the value of the named property */
  function IXMLDOMDocument2/getProperty (arg-name :: <string>) => 
        (arg-result :: <object>), name: "getProperty", disp-id: 205;
end dispatch-client <IXMLDOMDocument2>;


/* Dispatch interface: IXMLDOMNotation version 0.0
 * GUID: {2933BF8C-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMNotation>
  uuid "{2933BF8C-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMNotation/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMNotation/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IXMLDOMNotation/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMNotation/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMNotation/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMNotation/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMNotation/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMNotation/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMNotation/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMNotation/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMNotation/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMNotation/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMNotation/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMNotation/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMNotation/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMNotation/ownerDocument :: <IXMLDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IXMLDOMNotation/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMNotation/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMNotation/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMNotation/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMNotation/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMNotation/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMNotation/dataType :: <object>, name: "dataType", disp-id: 
        26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMNotation/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMNotation/transformNode (arg-stylesheet :: <IXMLDOMNode>) 
        => (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMNotation/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMNotation/selectSingleNode (arg-queryString :: <string>) 
        => (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", 
        disp-id: 30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMNotation/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMNotation/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMNotation/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMNotation/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMNotation/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* the public ID */
  constant property IXMLDOMNotation/publicId :: <object>, name: "publicId", 
        disp-id: 136;

  /* the system ID */
  constant property IXMLDOMNotation/systemId :: <object>, name: "systemId", 
        disp-id: 137;
end dispatch-client <IXMLDOMNotation>;


/* Dispatch interface: IXMLDOMEntity version 0.0
 * GUID: {2933BF8D-7B36-11D2-B20E-00C04F983E60}
 */
define dispatch-client <IXMLDOMEntity>
  uuid "{2933BF8D-7B36-11D2-B20E-00C04F983E60}";

  /* name of the node */
  constant property IXMLDOMEntity/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXMLDOMEntity/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IXMLDOMEntity/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXMLDOMEntity/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXMLDOMEntity/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXMLDOMEntity/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXMLDOMEntity/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXMLDOMEntity/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXMLDOMEntity/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXMLDOMEntity/attributes :: <IXMLDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IXMLDOMEntity/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXMLDOMEntity/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXMLDOMEntity/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXMLDOMEntity/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXMLDOMEntity/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXMLDOMEntity/ownerDocument :: <IXMLDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IXMLDOMEntity/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXMLDOMEntity/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXMLDOMEntity/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXMLDOMEntity/specified :: <boolean>, name: 
        "specified", disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXMLDOMEntity/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXMLDOMEntity/nodeTypedValue :: <object>, name: 
        "nodeTypedValue", disp-id: 25;

  /* the data type of the node */
  property IXMLDOMEntity/dataType :: <object>, name: "dataType", disp-id: 
        26;

  /* return the XML source for the node and each of its descendants */
  constant property IXMLDOMEntity/xml :: <string>, name: "xml", disp-id: 
        27;

  /* apply the stylesheet to the subtree */
  function IXMLDOMEntity/transformNode (arg-stylesheet :: <IXMLDOMNode>) => 
        (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXMLDOMEntity/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXMLDOMEntity/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXMLDOMEntity/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXMLDOMEntity/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXMLDOMEntity/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXMLDOMEntity/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXMLDOMEntity/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  /* the public ID */
  constant property IXMLDOMEntity/publicId :: <object>, name: "publicId", 
        disp-id: 140;

  /* the system ID */
  constant property IXMLDOMEntity/systemId :: <object>, name: "systemId", 
        disp-id: 141;

  /* the name of the notation */
  constant property IXMLDOMEntity/notationName :: <string>, name: 
        "notationName", disp-id: 142;
end dispatch-client <IXMLDOMEntity>;


/* Dispatch interface: IXTLRuntime version 0.0
 * GUID: {3EFAA425-272F-11D2-836F-0000F87A7782}
 * Description: XTL runtime object
 */
define dispatch-client <IXTLRuntime>
  uuid "{3EFAA425-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IXTLRuntime/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IXTLRuntime/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IXTLRuntime/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IXTLRuntime/parentNode :: <IXMLDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IXTLRuntime/childNodes :: <IXMLDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IXTLRuntime/firstChild :: <IXMLDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* last child of the node */
  constant property IXTLRuntime/lastChild :: <IXMLDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IXTLRuntime/previousSibling :: <IXMLDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IXTLRuntime/nextSibling :: <IXMLDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IXTLRuntime/attributes :: <IXMLDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IXTLRuntime/insertBefore (arg-newChild :: <IXMLDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IXMLDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IXTLRuntime/replaceChild (arg-newChild :: <IXMLDOMNode>, 
        arg-oldChild :: <IXMLDOMNode>) => (arg-result :: <IXMLDOMNode>), 
        name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IXTLRuntime/removeChild (arg-childNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IXTLRuntime/appendChild (arg-newChild :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "appendChild", disp-id: 16;

  function IXTLRuntime/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IXTLRuntime/ownerDocument :: <IXMLDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IXTLRuntime/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IXMLDOMNode>), name: "cloneNode", disp-id: 19;

  /* the type of node in string form */
  constant property IXTLRuntime/nodeTypeString :: <string>, name: 
        "nodeTypeString", disp-id: 21;

  /* text content of the node and subtree */
  property IXTLRuntime/text :: <string>, name: "text", disp-id: 24;

  /* indicates whether node is a default value */
  constant property IXTLRuntime/specified :: <boolean>, name: "specified", 
        disp-id: 22;

  /* pointer to the definition of the node in the DTD or schema */
  constant property IXTLRuntime/definition :: <IXMLDOMNode>, name: 
        "definition", disp-id: 23;

  /* get the strongly typed value of the node */
  property IXTLRuntime/nodeTypedValue :: <object>, name: "nodeTypedValue", 
        disp-id: 25;

  /* the data type of the node */
  property IXTLRuntime/dataType :: <object>, name: "dataType", disp-id: 26;

  /* return the XML source for the node and each of its descendants */
  constant property IXTLRuntime/xml :: <string>, name: "xml", disp-id: 27;

  /* apply the stylesheet to the subtree */
  function IXTLRuntime/transformNode (arg-stylesheet :: <IXMLDOMNode>) => 
        (arg-result :: <string>), name: "transformNode", disp-id: 28;

  /* execute query on the subtree */
  function IXTLRuntime/selectNodes (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNodeList>), name: "selectNodes", disp-id: 
        29;

  /* execute query on the subtree */
  function IXTLRuntime/selectSingleNode (arg-queryString :: <string>) => 
        (arg-result :: <IXMLDOMNode>), name: "selectSingleNode", disp-id: 
        30;

  /* has sub-tree been completely parsed */
  constant property IXTLRuntime/parsed :: <boolean>, name: "parsed", 
        disp-id: 31;

  /* the URI for the namespace applying to the node */
  constant property IXTLRuntime/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: 32;

  /* the prefix for the namespace applying to the node */
  constant property IXTLRuntime/prefix :: <string>, name: "prefix", 
        disp-id: 33;

  /* the base name of the node (nodename with the prefix stripped off) */
  constant property IXTLRuntime/baseName :: <string>, name: "baseName", 
        disp-id: 34;

  /* apply the stylesheet to the subtree, returning the result through a 
        document or a stream */
  function IXTLRuntime/transformNodeToObject (arg-stylesheet :: 
        <IXMLDOMNode>, arg-outputObject :: <object>) => (), name: 
        "transformNodeToObject", disp-id: 35;

  function IXTLRuntime/uniqueID (arg-pNode :: <IXMLDOMNode>) => (arg-result 
        :: type-union(<integer>, <machine-word>)), name: "uniqueID", 
        disp-id: 187;

  function IXTLRuntime/depth (arg-pNode :: <IXMLDOMNode>) => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "depth", disp-id: 
        188;

  function IXTLRuntime/childNumber (arg-pNode :: <IXMLDOMNode>) => 
        (arg-result :: type-union(<integer>, <machine-word>)), name: 
        "childNumber", disp-id: 189;

  function IXTLRuntime/ancestorChildNumber (arg-bstrNodeName :: <string>, 
        arg-pNode :: <IXMLDOMNode>) => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "ancestorChildNumber", disp-id: 190;

  function IXTLRuntime/absoluteChildNumber (arg-pNode :: <IXMLDOMNode>) => 
        (arg-result :: type-union(<integer>, <machine-word>)), name: 
        "absoluteChildNumber", disp-id: 191;

  function IXTLRuntime/formatIndex (arg-lIndex :: type-union(<integer>, 
        <machine-word>), arg-bstrFormat :: <string>) => (arg-result :: 
        <string>), name: "formatIndex", disp-id: 192;

  function IXTLRuntime/formatNumber (arg-dblNumber :: <double-float>, 
        arg-bstrFormat :: <string>) => (arg-result :: <string>), name: 
        "formatNumber", disp-id: 193;

  function IXTLRuntime/formatDate (arg-varDate :: <object>, arg-bstrFormat 
        :: <string>, /*optional*/ arg-varDestLocale :: <object>) => 
        (arg-result :: <string>), name: "formatDate", disp-id: 194;

  function IXTLRuntime/formatTime (arg-varTime :: <object>, arg-bstrFormat 
        :: <string>, /*optional*/ arg-varDestLocale :: <object>) => 
        (arg-result :: <string>), name: "formatTime", disp-id: 195;
end dispatch-client <IXTLRuntime>;


/* Dispatch interface: IXSLTemplate version 0.0
 * GUID: {2933BF93-7B36-11D2-B20E-00C04F983E60}
 * Description: IXSLTemplate Interface
 */
define dispatch-client <IXSLTemplate>
  /* Translation error: Cannot translate PROPERTYPUTREF method stylesheet. 
        */
  uuid "{2933BF93-7B36-11D2-B20E-00C04F983E60}";

  /* stylesheet to use with processors */
  constant property IXSLTemplate/stylesheet :: <IXMLDOMNode>, name: 
        "stylesheet", disp-id: 2;

  /* create a new processor object */
  function IXSLTemplate/createProcessor () => (arg-result :: 
        <IXSLProcessor>), name: "createProcessor", disp-id: 3;
end dispatch-client <IXSLTemplate>;


/* Dispatch interface: IXSLProcessor version 0.0
 * GUID: {2933BF92-7B36-11D2-B20E-00C04F983E60}
 * Description: IXSLProcessor Interface
 */
define dispatch-client <IXSLProcessor>
  uuid "{2933BF92-7B36-11D2-B20E-00C04F983E60}";

  /* XML input tree to transform */
  property IXSLProcessor/input :: <object>, name: "input", disp-id: 2;

  /* template object used to create this processor object */
  constant property IXSLProcessor/ownerTemplate :: <IXSLTemplate>, name: 
        "ownerTemplate", disp-id: 3;

  /* set XSL mode and it's namespace */
  function IXSLProcessor/setStartMode (arg-mode :: <string>, /*optional*/ 
        arg-namespaceURI :: <string>) => (), name: "setStartMode", disp-id: 
        4;

  /* starting XSL mode */
  constant property IXSLProcessor/startMode :: <string>, name: "startMode", 
        disp-id: 5;

  /* namespace of starting XSL mode */
  constant property IXSLProcessor/startModeURI :: <string>, name: 
        "startModeURI", disp-id: 6;

  /* custom stream object for transform output */
  property IXSLProcessor/output :: <object>, name: "output", disp-id: 7;

  /* start/resume the XSL transformation process */
  function IXSLProcessor/transform () => (arg-result :: <boolean>), name: 
        "transform", disp-id: 8;

  /* reset state of processor and abort current transform */
  function IXSLProcessor/reset () => (), name: "reset", disp-id: 9;

  /* current state of the processor */
  constant property IXSLProcessor/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: 10;

  /* set <xsl:param> values */
  function IXSLProcessor/addParameter (arg-baseName :: <string>, 
        arg-parameter :: <object>, /*optional*/ arg-namespaceURI :: 
        <string>) => (), name: "addParameter", disp-id: 11;

  /* pass object to stylesheet */
  function IXSLProcessor/addObject (arg-obj :: <LPDISPATCH>, 
        arg-namespaceURI :: <string>) => (), name: "addObject", disp-id: 
        12;

  /* current stylesheet being used */
  constant property IXSLProcessor/stylesheet :: <IXMLDOMNode>, name: 
        "stylesheet", disp-id: 13;
end dispatch-client <IXSLProcessor>;


/* Dispatch interface: IVBSAXXMLReader version 0.0
 * GUID: {8C033CAA-6CD6-4F73-B728-4531AF74945F}
 * Description: IVBSAXXMLReader interface
 */
define dispatch-client <IVBSAXXMLReader>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        entityResolver. */
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        contentHandler. */
  /* Translation error: Cannot translate PROPERTYPUTREF method dtdHandler. 
        */
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        errorHandler. */
  uuid "{8C033CAA-6CD6-4F73-B728-4531AF74945F}";

  /* Look up the value of a feature. */
  function IVBSAXXMLReader/getFeature (arg-strName :: <string>) => 
        (arg-result :: <boolean>), name: "getFeature", disp-id: 1282;

  /* Set the state of a feature. */
  function IVBSAXXMLReader/putFeature (arg-strName :: <string>, arg-fValue 
        :: <boolean>) => (), name: "putFeature", disp-id: 1283;

  /* Look up the value of a property. */
  function IVBSAXXMLReader/getProperty (arg-strName :: <string>) => 
        (arg-result :: <object>), name: "getProperty", disp-id: 1284;

  /* Set the value of a property. */
  function IVBSAXXMLReader/putProperty (arg-strName :: <string>, 
        arg-varValue :: <object>) => (), name: "putProperty", disp-id: 
        1285;

  /* Allow an application to register an entity resolver or look up the 
        current entity resolver. */
  constant property IVBSAXXMLReader/entityResolver :: 
        <IVBSAXEntityResolver>, name: "entityResolver", disp-id: 1286;

  /* Allow an application to register a content event handler or look up 
        the current content event handler. */
  constant property IVBSAXXMLReader/contentHandler :: 
        <IVBSAXContentHandler>, name: "contentHandler", disp-id: 1287;

  /* Allow an application to register a DTD event handler or look up the 
        current DTD event handler. */
  constant property IVBSAXXMLReader/dtdHandler :: <IVBSAXDTDHandler>, name: 
        "dtdHandler", disp-id: 1288;

  /* Allow an application to register an error event handler or look up the 
        current error event handler. */
  constant property IVBSAXXMLReader/errorHandler :: <IVBSAXErrorHandler>, 
        name: "errorHandler", disp-id: 1289;

  /* Set or get the base URL for the document. */
  property IVBSAXXMLReader/baseURL :: <string>, name: "baseURL", disp-id: 
        1290;

  /* Set or get the secure base URL for the document. */
  property IVBSAXXMLReader/secureBaseURL :: <string>, name: 
        "secureBaseURL", disp-id: 1291;

  /* Parse an XML document. */
  function IVBSAXXMLReader/parse (arg-varInput :: <object>) => (), name: 
        "parse", disp-id: 1292;

  /* Parse an XML document from a system identifier (URI). */
  function IVBSAXXMLReader/parseURL (arg-strURL :: <string>) => (), name: 
        "parseURL", disp-id: 1293;
end dispatch-client <IVBSAXXMLReader>;


/* Dispatch interface: IVBSAXEntityResolver version 0.0
 * GUID: {0C05D096-F45B-4ACA-AD1A-AA0BC25518DC}
 * Description: IVBSAXEntityResolver interface
 */
define dispatch-client <IVBSAXEntityResolver>
  uuid "{0C05D096-F45B-4ACA-AD1A-AA0BC25518DC}";

  /* Allow the application to resolve external entities. */
  function IVBSAXEntityResolver/resolveEntity (arg-strPublicId :: 
        inout-ref(<BSTR>), arg-strSystemId :: inout-ref(<BSTR>)) => 
        (arg-result :: <object>), name: "resolveEntity", disp-id: 1319;
end dispatch-client <IVBSAXEntityResolver>;


/* Dispatch interface: IVBSAXContentHandler version 0.0
 * GUID: {2ED7290A-4DD5-4B46-BB26-4E4155E77FAA}
 * Description: IVBSAXContentHandler interface
 */
define dispatch-client <IVBSAXContentHandler>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentLocator. */
  uuid "{2ED7290A-4DD5-4B46-BB26-4E4155E77FAA}";

  /* Receive notification of the beginning of a document. */
  function IVBSAXContentHandler/startDocument () => (), name: 
        "startDocument", disp-id: 1323;

  /* Receive notification of the end of a document. */
  function IVBSAXContentHandler/endDocument () => (), name: "endDocument", 
        disp-id: 1324;

  /* Begin the scope of a prefix-URI Namespace mapping. */
  function IVBSAXContentHandler/startPrefixMapping (arg-strPrefix :: 
        inout-ref(<BSTR>), arg-strURI :: inout-ref(<BSTR>)) => (), name: 
        "startPrefixMapping", disp-id: 1325;

  /* End the scope of a prefix-URI mapping. */
  function IVBSAXContentHandler/endPrefixMapping (arg-strPrefix :: 
        inout-ref(<BSTR>)) => (), name: "endPrefixMapping", disp-id: 1326;

  /* Receive notification of the beginning of an element. */
  function IVBSAXContentHandler/startElement (arg-strNamespaceURI :: 
        inout-ref(<BSTR>), arg-strLocalName :: inout-ref(<BSTR>), 
        arg-strQName :: inout-ref(<BSTR>), arg-oAttributes :: 
        <IVBSAXAttributes>) => (), name: "startElement", disp-id: 1327;

  /* Receive notification of the end of an element. */
  function IVBSAXContentHandler/endElement (arg-strNamespaceURI :: 
        inout-ref(<BSTR>), arg-strLocalName :: inout-ref(<BSTR>), 
        arg-strQName :: inout-ref(<BSTR>)) => (), name: "endElement", 
        disp-id: 1328;

  /* Receive notification of character data. */
  function IVBSAXContentHandler/characters (arg-strChars :: 
        inout-ref(<BSTR>)) => (), name: "characters", disp-id: 1329;

  /* Receive notification of ignorable whitespace in element content. */
  function IVBSAXContentHandler/ignorableWhitespace (arg-strChars :: 
        inout-ref(<BSTR>)) => (), name: "ignorableWhitespace", disp-id: 
        1330;

  /* Receive notification of a processing instruction. */
  function IVBSAXContentHandler/processingInstruction (arg-strTarget :: 
        inout-ref(<BSTR>), arg-strData :: inout-ref(<BSTR>)) => (), name: 
        "processingInstruction", disp-id: 1331;

  /* Receive notification of a skipped entity. */
  function IVBSAXContentHandler/skippedEntity (arg-strName :: 
        inout-ref(<BSTR>)) => (), name: "skippedEntity", disp-id: 1332;
end dispatch-client <IVBSAXContentHandler>;


/* Dispatch interface: IVBSAXLocator version 0.0
 * GUID: {796E7AC5-5AA2-4EFF-ACAD-3FAAF01A3288}
 * Description: IVBSAXLocator interface
 */
define dispatch-client <IVBSAXLocator>
  uuid "{796E7AC5-5AA2-4EFF-ACAD-3FAAF01A3288}";

  /* Get the column number where the current document event ends. */
  constant property IVBSAXLocator/columnNumber :: type-union(<integer>, 
        <machine-word>), name: "columnNumber", disp-id: 1313;

  /* Get the line number where the current document event ends. */
  constant property IVBSAXLocator/lineNumber :: type-union(<integer>, 
        <machine-word>), name: "lineNumber", disp-id: 1314;

  /* Get the public identifier for the current document event. */
  constant property IVBSAXLocator/publicId :: <string>, name: "publicId", 
        disp-id: 1315;

  /* Get the system identifier for the current document event. */
  constant property IVBSAXLocator/systemId :: <string>, name: "systemId", 
        disp-id: 1316;
end dispatch-client <IVBSAXLocator>;


/* Dispatch interface: IVBSAXAttributes version 0.0
 * GUID: {10DC0586-132B-4CAC-8BB3-DB00AC8B7EE0}
 * Description: IVBSAXAttributes interface
 */
define dispatch-client <IVBSAXAttributes>
  uuid "{10DC0586-132B-4CAC-8BB3-DB00AC8B7EE0}";

  /* Get the number of attributes in the list. */
  constant property IVBSAXAttributes/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 1344;

  /* Look up an attribute's Namespace URI by index. */
  function IVBSAXAttributes/getURI (arg-nIndex :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: "getURI", 
        disp-id: 1345;

  /* Look up an attribute's local name by index. */
  function IVBSAXAttributes/getLocalName (arg-nIndex :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "getLocalName", disp-id: 1346;

  /* Look up an attribute's XML 1.0 qualified name by index. */
  function IVBSAXAttributes/getQName (arg-nIndex :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: "getQName", 
        disp-id: 1347;

  /* Look up the index of an attribute by Namespace name. */
  function IVBSAXAttributes/getIndexFromName (arg-strURI :: <string>, 
        arg-strLocalName :: <string>) => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getIndexFromName", 
        disp-id: 1348;

  /* Look up the index of an attribute by XML 1.0 qualified name. */
  function IVBSAXAttributes/getIndexFromQName (arg-strQName :: <string>) => 
        (arg-result :: type-union(<integer>, <machine-word>)), name: 
        "getIndexFromQName", disp-id: 1349;

  /* Look up an attribute's type by index. */
  function IVBSAXAttributes/getType (arg-nIndex :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: "getType", 
        disp-id: 1350;

  /* Look up an attribute's type by Namespace name. */
  function IVBSAXAttributes/getTypeFromName (arg-strURI :: <string>, 
        arg-strLocalName :: <string>) => (arg-result :: <string>), name: 
        "getTypeFromName", disp-id: 1351;

  /* Look up an attribute's type by XML 1.0 qualified name. */
  function IVBSAXAttributes/getTypeFromQName (arg-strQName :: <string>) => 
        (arg-result :: <string>), name: "getTypeFromQName", disp-id: 1352;

  /* Look up an attribute's value by index. */
  function IVBSAXAttributes/getValue (arg-nIndex :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: "getValue", 
        disp-id: 1353;

  /* Look up an attribute's value by Namespace name. */
  function IVBSAXAttributes/getValueFromName (arg-strURI :: <string>, 
        arg-strLocalName :: <string>) => (arg-result :: <string>), name: 
        "getValueFromName", disp-id: 1354;

  /* Look up an attribute's value by XML 1.0 qualified name. */
  function IVBSAXAttributes/getValueFromQName (arg-strQName :: <string>) => 
        (arg-result :: <string>), name: "getValueFromQName", disp-id: 1355;
end dispatch-client <IVBSAXAttributes>;


/* Dispatch interface: IVBSAXDTDHandler version 0.0
 * GUID: {24FB3297-302D-4620-BA39-3A732D850558}
 * Description: IVBSAXDTDHandler interface
 */
define dispatch-client <IVBSAXDTDHandler>
  uuid "{24FB3297-302D-4620-BA39-3A732D850558}";

  /* Receive notification of a notation declaration event. */
  function IVBSAXDTDHandler/notationDecl (arg-strName :: inout-ref(<BSTR>), 
        arg-strPublicId :: inout-ref(<BSTR>), arg-strSystemId :: 
        inout-ref(<BSTR>)) => (), name: "notationDecl", disp-id: 1335;

  /* Receive notification of an unparsed entity declaration event. */
  function IVBSAXDTDHandler/unparsedEntityDecl (arg-strName :: 
        inout-ref(<BSTR>), arg-strPublicId :: inout-ref(<BSTR>), 
        arg-strSystemId :: inout-ref(<BSTR>), arg-strNotationName :: 
        inout-ref(<BSTR>)) => (), name: "unparsedEntityDecl", disp-id: 
        1336;
end dispatch-client <IVBSAXDTDHandler>;


/* Dispatch interface: IVBSAXErrorHandler version 0.0
 * GUID: {D963D3FE-173C-4862-9095-B92F66995F52}
 * Description: IVBSAXErrorHandler interface
 */
define dispatch-client <IVBSAXErrorHandler>
  uuid "{D963D3FE-173C-4862-9095-B92F66995F52}";

  /* Receive notification of a recoverable error. */
  function IVBSAXErrorHandler/error (arg-oLocator :: <IVBSAXLocator>, 
        arg-strErrorMessage :: inout-ref(<BSTR>), arg-nErrorCode :: 
        type-union(<integer>, <machine-word>)) => (), name: "error", 
        disp-id: 1339;

  /* Receive notification of a non-recoverable error. */
  function IVBSAXErrorHandler/fatalError (arg-oLocator :: <IVBSAXLocator>, 
        arg-strErrorMessage :: inout-ref(<BSTR>), arg-nErrorCode :: 
        type-union(<integer>, <machine-word>)) => (), name: "fatalError", 
        disp-id: 1340;

  /* Receive notification of an ignorable warning. */
  function IVBSAXErrorHandler/ignorableWarning (arg-oLocator :: 
        <IVBSAXLocator>, arg-strErrorMessage :: inout-ref(<BSTR>), 
        arg-nErrorCode :: type-union(<integer>, <machine-word>)) => (), 
        name: "ignorableWarning", disp-id: 1341;
end dispatch-client <IVBSAXErrorHandler>;


/* Dispatch interface: IVBSAXXMLFilter version 0.0
 * GUID: {1299EB1B-5B88-433E-82DE-82CA75AD4E04}
 * Description: IVBSAXXMLFilter interface
 */
define dispatch-client <IVBSAXXMLFilter>
  /* Translation error: Cannot translate PROPERTYPUTREF method parent. */
  uuid "{1299EB1B-5B88-433E-82DE-82CA75AD4E04}";

  /* Set or get the parent reader */
  constant property IVBSAXXMLFilter/parent :: <IVBSAXXMLReader>, name: 
        "parent", disp-id: 1309;
end dispatch-client <IVBSAXXMLFilter>;


/* Dispatch interface: IVBSAXLexicalHandler version 0.0
 * GUID: {032AAC35-8C0E-4D9D-979F-E3B702935576}
 * Description: IVBSAXLexicalHandler interface
 */
define dispatch-client <IVBSAXLexicalHandler>
  uuid "{032AAC35-8C0E-4D9D-979F-E3B702935576}";

  /* Report the start of DTD declarations, if any. */
  function IVBSAXLexicalHandler/startDTD (arg-strName :: inout-ref(<BSTR>), 
        arg-strPublicId :: inout-ref(<BSTR>), arg-strSystemId :: 
        inout-ref(<BSTR>)) => (), name: "startDTD", disp-id: 1358;

  /* Report the end of DTD declarations. */
  function IVBSAXLexicalHandler/endDTD () => (), name: "endDTD", disp-id: 
        1359;

  /* Report the beginning of some internal and external XML entities. */
  function IVBSAXLexicalHandler/startEntity (arg-strName :: 
        inout-ref(<BSTR>)) => (), name: "startEntity", disp-id: 1360;

  /* Report the end of an entity. */
  function IVBSAXLexicalHandler/endEntity (arg-strName :: 
        inout-ref(<BSTR>)) => (), name: "endEntity", disp-id: 1361;

  /* Report the start of a CDATA section. */
  function IVBSAXLexicalHandler/startCDATA () => (), name: "startCDATA", 
        disp-id: 1362;

  /* Report the end of a CDATA section. */
  function IVBSAXLexicalHandler/endCDATA () => (), name: "endCDATA", 
        disp-id: 1363;

  /* Report an XML comment anywhere in the document. */
  function IVBSAXLexicalHandler/comment (arg-strChars :: inout-ref(<BSTR>)) 
        => (), name: "comment", disp-id: 1364;
end dispatch-client <IVBSAXLexicalHandler>;


/* Dispatch interface: IVBSAXDeclHandler version 0.0
 * GUID: {E8917260-7579-4BE1-B5DD-7AFBFA6F077B}
 * Description: IVBSAXDeclHandler interface
 */
define dispatch-client <IVBSAXDeclHandler>
  uuid "{E8917260-7579-4BE1-B5DD-7AFBFA6F077B}";

  /* Report an element type declaration. */
  function IVBSAXDeclHandler/elementDecl (arg-strName :: inout-ref(<BSTR>), 
        arg-strModel :: inout-ref(<BSTR>)) => (), name: "elementDecl", 
        disp-id: 1367;

  /* Report an attribute type declaration. */
  function IVBSAXDeclHandler/attributeDecl (arg-strElementName :: 
        inout-ref(<BSTR>), arg-strAttributeName :: inout-ref(<BSTR>), 
        arg-strType :: inout-ref(<BSTR>), arg-strValueDefault :: 
        inout-ref(<BSTR>), arg-strValue :: inout-ref(<BSTR>)) => (), name: 
        "attributeDecl", disp-id: 1368;

  /* Report an internal entity declaration. */
  function IVBSAXDeclHandler/internalEntityDecl (arg-strName :: 
        inout-ref(<BSTR>), arg-strValue :: inout-ref(<BSTR>)) => (), name: 
        "internalEntityDecl", disp-id: 1369;

  /* Report a parsed external entity declaration. */
  function IVBSAXDeclHandler/externalEntityDecl (arg-strName :: 
        inout-ref(<BSTR>), arg-strPublicId :: inout-ref(<BSTR>), 
        arg-strSystemId :: inout-ref(<BSTR>)) => (), name: 
        "externalEntityDecl", disp-id: 1370;
end dispatch-client <IVBSAXDeclHandler>;


/* Dispatch interface: IMXWriter version 0.0
 * GUID: {4D7FF4BA-1565-4EA8-94E1-6E724A46F98D}
 * Description: IMXWriter interface
 */
define dispatch-client <IMXWriter>
  uuid "{4D7FF4BA-1565-4EA8-94E1-6E724A46F98D}";

  /* Set or get the output. */
  property IMXWriter/output :: <object>, name: "output", disp-id: 1385;

  /* Set or get the output encoding. */
  property IMXWriter/encoding :: <string>, name: "encoding", disp-id: 1387;

  /* Determine whether or not to write the byte order mark */
  property IMXWriter/byteOrderMark :: <boolean>, name: "byteOrderMark", 
        disp-id: 1388;

  /* Enable or disable auto indent mode. */
  property IMXWriter/indent :: <boolean>, name: "indent", disp-id: 1389;

  /* Set or get the standalone document declaration. */
  property IMXWriter/standalone :: <boolean>, name: "standalone", disp-id: 
        1390;

  /* Determine whether or not to omit the XML declaration. */
  property IMXWriter/omitXMLDeclaration :: <boolean>, name: 
        "omitXMLDeclaration", disp-id: 1391;

  /* Set or get the xml version info. */
  property IMXWriter/version :: <string>, name: "version", disp-id: 1392;

  /* When enabled, the writer no longer escapes out its input when writing 
        it out. */
  property IMXWriter/disableOutputEscaping :: <boolean>, name: 
        "disableOutputEscaping", disp-id: 1393;

  /* Flushes all writer buffers forcing the writer to write to the 
        underlying output object */
  function IMXWriter/flush () => (), name: "flush", disp-id: 1394;
end dispatch-client <IMXWriter>;


/* Dispatch interface: IMXAttributes version 0.0
 * GUID: {F10D27CC-3EC0-415C-8ED8-77AB1C5E7262}
 * Description: IMXAttributes interface
 */
define dispatch-client <IMXAttributes>
  uuid "{F10D27CC-3EC0-415C-8ED8-77AB1C5E7262}";

  /* Add an attribute to the end of the list. */
  function IMXAttributes/addAttribute (arg-strURI :: <string>, 
        arg-strLocalName :: <string>, arg-strQName :: <string>, arg-strType 
        :: <string>, arg-strValue :: <string>) => (), name: "addAttribute", 
        disp-id: 1373;

  /* Add an attribute, whose value is equal to the indexed attribute in the 
        input attributes object, to the end of the list. */
  function IMXAttributes/addAttributeFromIndex (arg-varAtts :: <object>, 
        arg-nIndex :: type-union(<integer>, <machine-word>)) => (), name: 
        "addAttributeFromIndex", disp-id: 1383;

  /* Clear the attribute list for reuse. */
  function IMXAttributes/clear () => (), name: "clear", disp-id: 1374;

  /* Remove an attribute from the list. */
  function IMXAttributes/removeAttribute (arg-nIndex :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "removeAttribute", disp-id: 1375;

  /* Set an attribute in the list. */
  function IMXAttributes/setAttribute (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strURI :: <string>, arg-strLocalName :: 
        <string>, arg-strQName :: <string>, arg-strType :: <string>, 
        arg-strValue :: <string>) => (), name: "setAttribute", disp-id: 
        1376;

  /* Copy an entire Attributes object. */
  function IMXAttributes/setAttributes (arg-varAtts :: <object>) => (), 
        name: "setAttributes", disp-id: 1377;

  /* Set the local name of a specific attribute. */
  function IMXAttributes/setLocalName (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strLocalName :: <string>) => (), name: 
        "setLocalName", disp-id: 1378;

  /* Set the qualified name of a specific attribute. */
  function IMXAttributes/setQName (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strQName :: <string>) => (), name: "setQName", 
        disp-id: 1379;

  /* Set the type of a specific attribute. */
  function IMXAttributes/setType (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strType :: <string>) => (), name: "setType", 
        disp-id: 1380;

  /* Set the Namespace URI of a specific attribute. */
  function IMXAttributes/setURI (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strURI :: <string>) => (), name: "setURI", 
        disp-id: 1381;

  /* Set the value of a specific attribute. */
  function IMXAttributes/setValue (arg-nIndex :: type-union(<integer>, 
        <machine-word>), arg-strValue :: <string>) => (), name: "setValue", 
        disp-id: 1382;
end dispatch-client <IMXAttributes>;


/* Dispatch interface: IMXReaderControl version 0.0
 * GUID: {808F4E35-8D5A-4FBE-8466-33A41279ED30}
 * Description: IMXReaderControl interface
 */
define dispatch-client <IMXReaderControl>
  uuid "{808F4E35-8D5A-4FBE-8466-33A41279ED30}";

  /* Abort the reader */
  function IMXReaderControl/abort () => (), name: "abort", disp-id: 1398;

  /* Resume the reader */
  function IMXReaderControl/resume () => (), name: "resume", disp-id: 1399;

  /* Suspend the reader */
  function IMXReaderControl/suspend () => (), name: "suspend", disp-id: 
        1400;
end dispatch-client <IMXReaderControl>;


/* hidden? Dispatch interface: IXMLElementCollection version 0.0
 * GUID: {65725580-9B5D-11D0-9BFE-00C04FC99C8E}
 * Description: IXMLElementCollection helps to enumerate through a XML 
        document tree.
 */
define dispatch-client <IXMLElementCollection>
  uuid "{65725580-9B5D-11D0-9BFE-00C04FC99C8E}";

  property IXMLElementCollection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 65537;

  constant property IXMLElementCollection/_newEnum :: <LPUNKNOWN>, name: 
        "_newEnum", disp-id: -4;

  /* get current item, or (optional) by index and name. */
  function IXMLElementCollection/item (/*optional*/ arg-var1 :: <object>, 
        /*optional*/ arg-var2 :: <object>) => (arg-result :: <LPDISPATCH>), 
        name: "item", disp-id: 65539;
end dispatch-client <IXMLElementCollection>;


/* hidden? Dispatch interface: IXMLDocument version 0.0
 * GUID: {F52E2B61-18A1-11D1-B105-00805F49916B}
 * Description: IXMLDocument loads and saves XML document. This is 
        obsolete. You should use IDOMDocument or IXMLDOMDocument.
 */
define dispatch-client <IXMLDocument>
  uuid "{F52E2B61-18A1-11D1-B105-00805F49916B}";

  /* get root IXMLElement of the XML document. */
  constant property IXMLDocument/root :: <IXMLElement>, name: "root", 
        disp-id: 65637;

  constant property IXMLDocument/fileSize :: <string>, name: "fileSize", 
        disp-id: 65638;

  constant property IXMLDocument/fileModifiedDate :: <string>, name: 
        "fileModifiedDate", disp-id: 65639;

  constant property IXMLDocument/fileUpdatedDate :: <string>, name: 
        "fileUpdatedDate", disp-id: 65640;

  /* set URL to load an XML document from the URL. */
  property IXMLDocument/url :: <string>, name: "url", disp-id: 65641;

  constant property IXMLDocument/mimeType :: <string>, name: "mimeType", 
        disp-id: 65642;

  /* get ready state. */
  constant property IXMLDocument/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: 65643;

  /* get encoding. */
  property IXMLDocument/charset :: <string>, name: "charset", disp-id: 
        65645;

  /* get XML version number. */
  constant property IXMLDocument/version :: <string>, name: "version", 
        disp-id: 65646;

  /* get document type. */
  constant property IXMLDocument/doctype :: <string>, name: "doctype", 
        disp-id: 65647;

  constant property IXMLDocument/dtdURL :: <string>, name: "dtdURL", 
        disp-id: 65648;

  /* create different types of IXMLElements. */
  function IXMLDocument/createElement (arg-vType :: <object>, /*optional*/ 
        arg-var1 :: <object>) => (arg-result :: <IXMLElement>), name: 
        "createElement", disp-id: 65644;
end dispatch-client <IXMLDocument>;


/* hidden? Dispatch interface: IXMLElement version 0.0
 * GUID: {3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}
 * Description: IXMLElement represents an element in the XML document tree.
 */
define dispatch-client <IXMLElement>
  uuid "{3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}";

  /* get tag name. */
  property IXMLElement/tagName :: <string>, name: "tagName", disp-id: 
        65737;

  /* get parent IXMLElement. */
  constant property IXMLElement/parent :: <IXMLElement>, name: "parent", 
        disp-id: 65738;

  /* set attribute. */
  function IXMLElement/setAttribute (arg-strPropertyName :: <string>, 
        arg-PropertyValue :: <object>) => (), name: "setAttribute", 
        disp-id: 65739;

  /* get attribute. */
  function IXMLElement/getAttribute (arg-strPropertyName :: <string>) => 
        (arg-result :: <object>), name: "getAttribute", disp-id: 65740;

  /* remove attribute. */
  function IXMLElement/removeAttribute (arg-strPropertyName :: <string>) => 
        (), name: "removeAttribute", disp-id: 65741;

  /* get a IXMLElementCollection of children. */
  constant property IXMLElement/children :: <IXMLElementCollection>, name: 
        "children", disp-id: 65742;

  /* get type of this IXMLElement. */
  constant property IXMLElement/type :: type-union(<integer>, 
        <machine-word>), name: "type", disp-id: 65743;

  /* get text. */
  property IXMLElement/text :: <string>, name: "text", disp-id: 65744;

  /* add a child. */
  function IXMLElement/addChild (arg-pChildElem :: <IXMLElement>, 
        arg-lIndex :: type-union(<integer>, <machine-word>), arg-lReserved 
        :: type-union(<integer>, <machine-word>)) => (), name: "addChild", 
        disp-id: 65745;

  /* remove a child. */
  function IXMLElement/removeChild (arg-pChildElem :: <IXMLElement>) => (), 
        name: "removeChild", disp-id: 65746;
end dispatch-client <IXMLElement>;


/* hidden? Dispatch interface: IXMLElement2 version 0.0
 * GUID: {2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}
 * Description: IXMLElement2 extends IXMLElement.
 */
define dispatch-client <IXMLElement2>
  uuid "{2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}";

  /* get tag name. */
  property IXMLElement2/tagName :: <string>, name: "tagName", disp-id: 
        65737;

  /* get parent IXMLElement. */
  constant property IXMLElement2/parent :: <IXMLElement2>, name: "parent", 
        disp-id: 65738;

  /* set attribute. */
  function IXMLElement2/setAttribute (arg-strPropertyName :: <string>, 
        arg-PropertyValue :: <object>) => (), name: "setAttribute", 
        disp-id: 65739;

  /* get attribute. */
  function IXMLElement2/getAttribute (arg-strPropertyName :: <string>) => 
        (arg-result :: <object>), name: "getAttribute", disp-id: 65740;

  /* remove attribute. */
  function IXMLElement2/removeAttribute (arg-strPropertyName :: <string>) 
        => (), name: "removeAttribute", disp-id: 65741;

  /* get a IXMLElementCollection of all children. */
  constant property IXMLElement2/children :: <IXMLElementCollection>, name: 
        "children", disp-id: 65742;

  /* get type of this IXMLElement. */
  constant property IXMLElement2/type :: type-union(<integer>, 
        <machine-word>), name: "type", disp-id: 65743;

  /* get text. */
  property IXMLElement2/text :: <string>, name: "text", disp-id: 65744;

  /* add a child. */
  function IXMLElement2/addChild (arg-pChildElem :: <IXMLElement2>, 
        arg-lIndex :: type-union(<integer>, <machine-word>), arg-lReserved 
        :: type-union(<integer>, <machine-word>)) => (), name: "addChild", 
        disp-id: 65745;

  /* remove a child. */
  function IXMLElement2/removeChild (arg-pChildElem :: <IXMLElement2>) => 
        (), name: "removeChild", disp-id: 65746;

  /* get a IXMLElementCollection of all attributes. */
  constant property IXMLElement2/attributes :: <IXMLElementCollection>, 
        name: "attributes", disp-id: 65747;
end dispatch-client <IXMLElement2>;


/* hidden? Dispatch interface: IXMLAttribute version 0.0
 * GUID: {D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}
 * Description: IXMLAttribute allows to get attributes of an IXMLElement.
 */
define dispatch-client <IXMLAttribute>
  uuid "{D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}";

  /* get attribute name. */
  constant property IXMLAttribute/name :: <string>, name: "name", disp-id: 
        65937;

  /* get attribute value. */
  constant property IXMLAttribute/value :: <string>, name: "value", 
        disp-id: 65938;
end dispatch-client <IXMLAttribute>;


/* struct: _xml_error version 0.0
 */
define C-struct <_xml-error>
  slot _xml-error/_nLine :: <C-unsigned-long>;
  slot _xml-error/_pchBuf :: <BSTR>;
  slot _xml-error/_cchBuf :: <C-unsigned-long>;
  slot _xml-error/_ich :: <C-unsigned-long>;
  slot _xml-error/_pszFound :: <BSTR>;
  slot _xml-error/_pszExpected :: <BSTR>;
  slot _xml-error/_reserved1 :: <C-unsigned-long>;
  slot _xml-error/_reserved2 :: <C-unsigned-long>;
end C-struct <_xml-error>;


/* Typedef: XMLELEM_TYPE
 * Description: Constants that define types for IXMLElement.
 */
define constant <XMLELEM-TYPE> = <tagXMLEMEM-TYPE>;


/* Enumeration: tagXMLEMEM_TYPE
 * Description: Constants that define types for IXMLElement.
 */
define constant <tagXMLEMEM-TYPE> = type-union(<integer>, <machine-word>);
define constant $XMLELEMTYPE-ELEMENT = 0;
define constant $XMLELEMTYPE-TEXT = 1;
define constant $XMLELEMTYPE-COMMENT = 2;
define constant $XMLELEMTYPE-DOCUMENT = 3;
define constant $XMLELEMTYPE-DTD = 4;
define constant $XMLELEMTYPE-PI = 5;
define constant $XMLELEMTYPE-OTHER = 6;


/* Dispatch interface: IXMLDOMSelection version 0.0
 * GUID: {AA634FC7-5888-44A7-A257-3A47150D3A0E}
 */
define dispatch-client <IXMLDOMSelection>
  /* Translation error: Cannot translate PROPERTYPUTREF method context. */
  uuid "{AA634FC7-5888-44A7-A257-3A47150D3A0E}";

  /* collection of nodes */
  element constant property IXMLDOMSelection/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IXMLDOMNode>, name: 
        "item", disp-id: 0;

  /* number of nodes in the collection */
  constant property IXMLDOMSelection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 74;

  /* get next node from iterator */
  function IXMLDOMSelection/nextNode () => (arg-result :: <IXMLDOMNode>), 
        name: "nextNode", disp-id: 76;

  /* reset the position of iterator */
  function IXMLDOMSelection/reset () => (), name: "reset", disp-id: 77;

  constant property IXMLDOMSelection/_newEnum :: <LPUNKNOWN>, name: 
        "_newEnum", disp-id: -4;

  /* selection expression */
  property IXMLDOMSelection/expr :: <string>, name: "expr", disp-id: 81;

  /* nodes to apply selection expression to */
  constant property IXMLDOMSelection/context :: <IXMLDOMNode>, name: 
        "context", disp-id: 82;

  /* gets the next node without advancing the list position */
  function IXMLDOMSelection/peekNode () => (arg-result :: <IXMLDOMNode>), 
        name: "peekNode", disp-id: 83;

  /* checks to see if the node matches the pattern */
  function IXMLDOMSelection/matches (arg-pNode :: <IXMLDOMNode>) => 
        (arg-result :: <IXMLDOMNode>), name: "matches", disp-id: 84;

  /* removes the next node */
  function IXMLDOMSelection/removeNext () => (arg-result :: <IXMLDOMNode>), 
        name: "removeNext", disp-id: 85;

  /* removes all the nodes that match the selection */
  function IXMLDOMSelection/removeAll () => (), name: "removeAll", disp-id: 
        86;

  /* clone this object with the same position and context */
  function IXMLDOMSelection/clone () => (arg-result :: <IXMLDOMSelection>), 
        name: "clone", disp-id: 87;

  /* get the value of the named property */
  function IXMLDOMSelection/getProperty (arg-name :: <string>) => 
        (arg-result :: <object>), name: "getProperty", disp-id: 88;

  /* set the value of the named property */
  function IXMLDOMSelection/setProperty (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setProperty", disp-id: 89;
end dispatch-client <IXMLDOMSelection>;


/* hidden? Dispatch interface: XMLDOMDocumentEvents version 0.0
 * GUID: {3EFAA427-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <XMLDOMDocumentEvents>
  uuid "{3EFAA427-272F-11D2-836F-0000F87A7782}";

  function XMLDOMDocumentEvents/ondataavailable () => (arg-result :: 
        <HRESULT>), name: "ondataavailable", disp-id: 198;

  function XMLDOMDocumentEvents/onreadystatechange () => (arg-result :: 
        <HRESULT>), name: "onreadystatechange", disp-id: -609;
end dispatch-client <XMLDOMDocumentEvents>;


/* hidden? Dispatch interface: IDSOControl version 0.0
 * GUID: {310AFA62-0575-11D2-9CA9-0060B0EC3D39}
 * Description: DSO Control
 */
define dispatch-client <IDSOControl>
  uuid "{310AFA62-0575-11D2-9CA9-0060B0EC3D39}";

  property IDSOControl/XMLDocument :: <IXMLDOMDocument>, name: 
        "XMLDocument", disp-id: 65537;

  property IDSOControl/JavaDSOCompatible :: type-union(<integer>, 
        <machine-word>), name: "JavaDSOCompatible", disp-id: 65538;

  constant property IDSOControl/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: -525;
end dispatch-client <IDSOControl>;


/* Dispatch interface: IXMLHTTPRequest version 0.0
 * GUID: {ED8C108D-4349-11D2-91A4-00C04F7969E8}
 * Description: IXMLHTTPRequest Interface
 */
define dispatch-client <IXMLHTTPRequest>
  uuid "{ED8C108D-4349-11D2-91A4-00C04F7969E8}";

  /* Open HTTP connection */
  function IXMLHTTPRequest/open (arg-bstrMethod :: <string>, arg-bstrUrl :: 
        <string>, /*optional*/ arg-varAsync :: <object>, /*optional*/ 
        arg-bstrUser :: <object>, /*optional*/ arg-bstrPassword :: 
        <object>) => (), name: "open", disp-id: 1;

  /* Add HTTP request header */
  function IXMLHTTPRequest/setRequestHeader (arg-bstrHeader :: <string>, 
        arg-bstrValue :: <string>) => (), name: "setRequestHeader", 
        disp-id: 2;

  /* Get HTTP response header */
  function IXMLHTTPRequest/getResponseHeader (arg-bstrHeader :: <string>) 
        => (arg-result :: <string>), name: "getResponseHeader", disp-id: 3;

  /* Get all HTTP response headers */
  function IXMLHTTPRequest/getAllResponseHeaders () => (arg-result :: 
        <string>), name: "getAllResponseHeaders", disp-id: 4;

  /* Send HTTP request */
  function IXMLHTTPRequest/send (/*optional*/ arg-varBody :: <object>) => 
        (), name: "send", disp-id: 5;

  /* Abort HTTP request */
  function IXMLHTTPRequest/abort () => (), name: "abort", disp-id: 6;

  /* Get HTTP status code */
  constant property IXMLHTTPRequest/status :: type-union(<integer>, 
        <machine-word>), name: "status", disp-id: 7;

  /* Get HTTP status text */
  constant property IXMLHTTPRequest/statusText :: <string>, name: 
        "statusText", disp-id: 8;

  /* Get response body */
  constant property IXMLHTTPRequest/responseXML :: <LPDISPATCH>, name: 
        "responseXML", disp-id: 9;

  /* Get response body */
  constant property IXMLHTTPRequest/responseText :: <string>, name: 
        "responseText", disp-id: 10;

  /* Get response body */
  constant property IXMLHTTPRequest/responseBody :: <object>, name: 
        "responseBody", disp-id: 11;

  /* Get response body */
  constant property IXMLHTTPRequest/responseStream :: <object>, name: 
        "responseStream", disp-id: 12;

  /* Get ready state */
  constant property IXMLHTTPRequest/readyState :: type-union(<integer>, 
        <machine-word>), name: "readyState", disp-id: 13;

  /* Register a complete event handler */
  write-only property IXMLHTTPRequest/onreadystatechange :: <LPDISPATCH>, 
        name: "onreadystatechange", disp-id: 14;
end dispatch-client <IXMLHTTPRequest>;


/* Typedef: SERVERXMLHTTP_OPTION
 * Description: Options for ServerXMLHTTPRequest Option property
 */
define constant <SERVERXMLHTTP-OPTION> = <_SERVERXMLHTTP-OPTION>;


/* Enumeration: _SERVERXMLHTTP_OPTION
 * Description: Options for ServerXMLHTTPRequest Option property
 */
define constant <_SERVERXMLHTTP-OPTION> = type-union(<integer>, 
        <machine-word>);
define constant $SXH-OPTION-URL-CODEPAGE = 0;
define constant $SXH-OPTION-ESCAPE-PERCENT-IN-URL = 1;
define constant $SXH-OPTION-IGNORE-SERVER-SSL-CERT-ERROR-FLAGS = 2;
define constant $SXH-OPTION-SELECT-CLIENT-SSL-CERT = 3;


/* Typedef: SXH_SERVER_CERT_OPTION
 * Description: Flags for SXH_OPTION_IGNORE_SERVER_SSL_CERT_ERROR_FLAGS 
        option
 */
define constant <SXH-SERVER-CERT-OPTION> = <_SXH-SERVER-CERT-OPTION>;


/* Enumeration: _SXH_SERVER_CERT_OPTION
 * Description: Flags for SXH_OPTION_IGNORE_SERVER_SSL_CERT_ERROR_FLAGS 
        option
 */
define constant <_SXH-SERVER-CERT-OPTION> = type-union(<integer>, 
        <machine-word>);
define constant $SXH-SERVER-CERT-IGNORE-UNKNOWN-CA = 256;
define constant $SXH-SERVER-CERT-IGNORE-WRONG-USAGE = 512;
define constant $SXH-SERVER-CERT-IGNORE-CERT-CN-INVALID = 4096;
define constant $SXH-SERVER-CERT-IGNORE-CERT-DATE-INVALID = 8192;
define constant $SXH-SERVER-CERT-IGNORE-ALL-SERVER-ERRORS = 13056;


/* Dispatch interface: IServerXMLHTTPRequest version 0.0
 * GUID: {2E9196BF-13BA-4DD4-91CA-6C571F281495}
 * Description: IServerXMLHTTPRequest Interface
 */
define dispatch-client <IServerXMLHTTPRequest>
  uuid "{2E9196BF-13BA-4DD4-91CA-6C571F281495}";

  /* Open HTTP connection */
  function IServerXMLHTTPRequest/open (arg-bstrMethod :: <string>, 
        arg-bstrUrl :: <string>, /*optional*/ arg-varAsync :: <object>, 
        /*optional*/ arg-bstrUser :: <object>, /*optional*/ 
        arg-bstrPassword :: <object>) => (), name: "open", disp-id: 1;

  /* Add HTTP request header */
  function IServerXMLHTTPRequest/setRequestHeader (arg-bstrHeader :: 
        <string>, arg-bstrValue :: <string>) => (), name: 
        "setRequestHeader", disp-id: 2;

  /* Get HTTP response header */
  function IServerXMLHTTPRequest/getResponseHeader (arg-bstrHeader :: 
        <string>) => (arg-result :: <string>), name: "getResponseHeader", 
        disp-id: 3;

  /* Get all HTTP response headers */
  function IServerXMLHTTPRequest/getAllResponseHeaders () => (arg-result :: 
        <string>), name: "getAllResponseHeaders", disp-id: 4;

  /* Send HTTP request */
  function IServerXMLHTTPRequest/send (/*optional*/ arg-varBody :: 
        <object>) => (), name: "send", disp-id: 5;

  /* Abort HTTP request */
  function IServerXMLHTTPRequest/abort () => (), name: "abort", disp-id: 6;

  /* Get HTTP status code */
  constant property IServerXMLHTTPRequest/status :: type-union(<integer>, 
        <machine-word>), name: "status", disp-id: 7;

  /* Get HTTP status text */
  constant property IServerXMLHTTPRequest/statusText :: <string>, name: 
        "statusText", disp-id: 8;

  /* Get response body */
  constant property IServerXMLHTTPRequest/responseXML :: <LPDISPATCH>, 
        name: "responseXML", disp-id: 9;

  /* Get response body */
  constant property IServerXMLHTTPRequest/responseText :: <string>, name: 
        "responseText", disp-id: 10;

  /* Get response body */
  constant property IServerXMLHTTPRequest/responseBody :: <object>, name: 
        "responseBody", disp-id: 11;

  /* Get response body */
  constant property IServerXMLHTTPRequest/responseStream :: <object>, name: 
        "responseStream", disp-id: 12;

  /* Get ready state */
  constant property IServerXMLHTTPRequest/readyState :: 
        type-union(<integer>, <machine-word>), name: "readyState", disp-id: 
        13;

  /* Register a complete event handler */
  write-only property IServerXMLHTTPRequest/onreadystatechange :: 
        <LPDISPATCH>, name: "onreadystatechange", disp-id: 14;

  /* Specify timeout settings (in milliseconds) */
  function IServerXMLHTTPRequest/setTimeouts (arg-resolveTimeout :: 
        type-union(<integer>, <machine-word>), arg-connectTimeout :: 
        type-union(<integer>, <machine-word>), arg-sendTimeout :: 
        type-union(<integer>, <machine-word>), arg-receiveTimeout :: 
        type-union(<integer>, <machine-word>)) => (), name: "setTimeouts", 
        disp-id: 15;

  /* Wait for asynchronous send to complete, with optional timeout (in 
        seconds) */
  function IServerXMLHTTPRequest/waitForResponse (/*optional*/ 
        arg-timeoutInSeconds :: <object>) => (arg-result :: <boolean>), 
        name: "waitForResponse", disp-id: 16;

  /* Get an option value */
  function IServerXMLHTTPRequest/getOption (arg-option :: 
        <SERVERXMLHTTP-OPTION>) => (arg-result :: <object>), name: 
        "getOption", disp-id: 17;

  /* Set an option value */
  function IServerXMLHTTPRequest/setOption (arg-option :: 
        <SERVERXMLHTTP-OPTION>, arg-value :: <object>) => (), name: 
        "setOption", disp-id: 18;
end dispatch-client <IServerXMLHTTPRequest>;


/* COM class: DOMDocument version 0.0
 * GUID: {F6D90F11-9C73-11D3-B32E-00C04F990BB4}
 * Description: W3C-DOM XML Document (Apartment)
 */
define constant $DOMDocument-class-id = as(<REFCLSID>, 
        "{F6D90F11-9C73-11D3-B32E-00C04F990BB4}");

define function make-DOMDocument () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface DOMDocument not supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $DOMDocument-class-id);
  values(default-interface)
end function make-DOMDocument;


/* COM class: DOMDocument26 version 0.0
 * GUID: {F5078F1B-C551-11D3-89B9-0000F81FE221}
 * Description: W3C-DOM XML Document (Apartment)
 */
define constant $DOMDocument26-class-id = as(<REFCLSID>, 
        "{F5078F1B-C551-11D3-89B9-0000F81FE221}");

define function make-DOMDocument26 () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface DOMDocument26 not supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $DOMDocument26-class-id);
  values(default-interface)
end function make-DOMDocument26;


/* COM class: DOMDocument30 version 0.0
 * GUID: {F5078F32-C551-11D3-89B9-0000F81FE221}
 * Description: W3C-DOM XML Document (Apartment)
 */
define constant $DOMDocument30-class-id = as(<REFCLSID>, 
        "{F5078F32-C551-11D3-89B9-0000F81FE221}");

define function make-DOMDocument30 () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface DOMDocument30 not supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $DOMDocument30-class-id);
  values(default-interface)
end function make-DOMDocument30;


/* COM class: FreeThreadedDOMDocument version 0.0
 * GUID: {F6D90F12-9C73-11D3-B32E-00C04F990BB4}
 * Description: W3C-DOM XML Document (Free threaded)
 */
define constant $FreeThreadedDOMDocument-class-id = as(<REFCLSID>, 
        "{F6D90F12-9C73-11D3-B32E-00C04F990BB4}");

define function make-FreeThreadedDOMDocument () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface FreeThreadedDOMDocument not 
        supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $FreeThreadedDOMDocument-class-id);
  values(default-interface)
end function make-FreeThreadedDOMDocument;


/* COM class: FreeThreadedDOMDocument26 version 0.0
 * GUID: {F5078F1C-C551-11D3-89B9-0000F81FE221}
 * Description: W3C-DOM XML Document (Free threaded)
 */
define constant $FreeThreadedDOMDocument26-class-id = as(<REFCLSID>, 
        "{F5078F1C-C551-11D3-89B9-0000F81FE221}");

define function make-FreeThreadedDOMDocument26 () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface FreeThreadedDOMDocument26 not 
        supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $FreeThreadedDOMDocument26-class-id);
  values(default-interface)
end function make-FreeThreadedDOMDocument26;


/* COM class: FreeThreadedDOMDocument30 version 0.0
 * GUID: {F5078F33-C551-11D3-89B9-0000F81FE221}
 * Description: W3C-DOM XML Document (Free threaded)
 */
define constant $FreeThreadedDOMDocument30-class-id = as(<REFCLSID>, 
        "{F5078F33-C551-11D3-89B9-0000F81FE221}");

define function make-FreeThreadedDOMDocument30 () => (default-interface :: 
        <IXMLDOMDocument2>)
  /* Translation error: source interface FreeThreadedDOMDocument30 not 
        supported. */
  let default-interface = make(<IXMLDOMDocument2>, class-id: 
        $FreeThreadedDOMDocument30-class-id);
  values(default-interface)
end function make-FreeThreadedDOMDocument30;


/* COM class: XMLSchemaCache version 0.0
 * GUID: {373984C9-B845-449B-91E7-45AC83036ADE}
 * Description: object for caching schemas
 */
define constant $XMLSchemaCache-class-id = as(<REFCLSID>, 
        "{373984C9-B845-449B-91E7-45AC83036ADE}");

define function make-XMLSchemaCache () => (default-interface :: 
        <IXMLDOMSchemaCollection>)
  let default-interface = make(<IXMLDOMSchemaCollection>, class-id: 
        $XMLSchemaCache-class-id);
  values(default-interface)
end function make-XMLSchemaCache;


/* COM class: XMLSchemaCache26 version 0.0
 * GUID: {F5078F1D-C551-11D3-89B9-0000F81FE221}
 * Description: object for caching schemas
 */
define constant $XMLSchemaCache26-class-id = as(<REFCLSID>, 
        "{F5078F1D-C551-11D3-89B9-0000F81FE221}");

define function make-XMLSchemaCache26 () => (default-interface :: 
        <IXMLDOMSchemaCollection>)
  let default-interface = make(<IXMLDOMSchemaCollection>, class-id: 
        $XMLSchemaCache26-class-id);
  values(default-interface)
end function make-XMLSchemaCache26;


/* COM class: XMLSchemaCache30 version 0.0
 * GUID: {F5078F34-C551-11D3-89B9-0000F81FE221}
 * Description: object for caching schemas
 */
define constant $XMLSchemaCache30-class-id = as(<REFCLSID>, 
        "{F5078F34-C551-11D3-89B9-0000F81FE221}");

define function make-XMLSchemaCache30 () => (default-interface :: 
        <IXMLDOMSchemaCollection>)
  let default-interface = make(<IXMLDOMSchemaCollection>, class-id: 
        $XMLSchemaCache30-class-id);
  values(default-interface)
end function make-XMLSchemaCache30;


/* COM class: XSLTemplate version 0.0
 * GUID: {2933BF94-7B36-11D2-B20E-00C04F983E60}
 * Description: object for caching compiled XSL stylesheets
 */
define constant $XSLTemplate-class-id = as(<REFCLSID>, 
        "{2933BF94-7B36-11D2-B20E-00C04F983E60}");

define function make-XSLTemplate () => (default-interface :: 
        <IXSLTemplate>)
  let default-interface = make(<IXSLTemplate>, class-id: 
        $XSLTemplate-class-id);
  values(default-interface)
end function make-XSLTemplate;


/* COM class: XSLTemplate26 version 0.0
 * GUID: {F5078F21-C551-11D3-89B9-0000F81FE221}
 * Description: object for caching compiled XSL stylesheets
 */
define constant $XSLTemplate26-class-id = as(<REFCLSID>, 
        "{F5078F21-C551-11D3-89B9-0000F81FE221}");

define function make-XSLTemplate26 () => (default-interface :: 
        <IXSLTemplate>)
  let default-interface = make(<IXSLTemplate>, class-id: 
        $XSLTemplate26-class-id);
  values(default-interface)
end function make-XSLTemplate26;


/* COM class: XSLTemplate30 version 0.0
 * GUID: {F5078F36-C551-11D3-89B9-0000F81FE221}
 * Description: object for caching compiled XSL stylesheets
 */
define constant $XSLTemplate30-class-id = as(<REFCLSID>, 
        "{F5078F36-C551-11D3-89B9-0000F81FE221}");

define function make-XSLTemplate30 () => (default-interface :: 
        <IXSLTemplate>)
  let default-interface = make(<IXSLTemplate>, class-id: 
        $XSLTemplate30-class-id);
  values(default-interface)
end function make-XSLTemplate30;


/* COM class: DSOControl version 0.0
 * GUID: {F6D90F14-9C73-11D3-B32E-00C04F990BB4}
 * Description: XML Data Source Object
 */
define constant $DSOControl-class-id = as(<REFCLSID>, 
        "{F6D90F14-9C73-11D3-B32E-00C04F990BB4}");

define function make-DSOControl () => (default-interface :: <IDSOControl>)
  let default-interface = make(<IDSOControl>, class-id: 
        $DSOControl-class-id);
  values(default-interface)
end function make-DSOControl;


/* COM class: DSOControl26 version 0.0
 * GUID: {F5078F1F-C551-11D3-89B9-0000F81FE221}
 * Description: XML Data Source Object
 */
define constant $DSOControl26-class-id = as(<REFCLSID>, 
        "{F5078F1F-C551-11D3-89B9-0000F81FE221}");

define function make-DSOControl26 () => (default-interface :: 
        <IDSOControl>)
  let default-interface = make(<IDSOControl>, class-id: 
        $DSOControl26-class-id);
  values(default-interface)
end function make-DSOControl26;


/* COM class: DSOControl30 version 0.0
 * GUID: {F5078F39-C551-11D3-89B9-0000F81FE221}
 * Description: XML Data Source Object
 */
define constant $DSOControl30-class-id = as(<REFCLSID>, 
        "{F5078F39-C551-11D3-89B9-0000F81FE221}");

define function make-DSOControl30 () => (default-interface :: 
        <IDSOControl>)
  let default-interface = make(<IDSOControl>, class-id: 
        $DSOControl30-class-id);
  values(default-interface)
end function make-DSOControl30;


/* COM class: XMLHTTP version 0.0
 * GUID: {F6D90F16-9C73-11D3-B32E-00C04F990BB4}
 * Description: XML HTTP Request class.
 */
define constant $XMLHTTP-class-id = as(<REFCLSID>, 
        "{F6D90F16-9C73-11D3-B32E-00C04F990BB4}");

define function make-XMLHTTP () => (default-interface :: <IXMLHTTPRequest>)
  let default-interface = make(<IXMLHTTPRequest>, class-id: 
        $XMLHTTP-class-id);
  values(default-interface)
end function make-XMLHTTP;


/* COM class: XMLHTTP26 version 0.0
 * GUID: {F5078F1E-C551-11D3-89B9-0000F81FE221}
 * Description: XML HTTP Request class.
 */
define constant $XMLHTTP26-class-id = as(<REFCLSID>, 
        "{F5078F1E-C551-11D3-89B9-0000F81FE221}");

define function make-XMLHTTP26 () => (default-interface :: 
        <IXMLHTTPRequest>)
  let default-interface = make(<IXMLHTTPRequest>, class-id: 
        $XMLHTTP26-class-id);
  values(default-interface)
end function make-XMLHTTP26;


/* COM class: XMLHTTP30 version 0.0
 * GUID: {F5078F35-C551-11D3-89B9-0000F81FE221}
 * Description: XML HTTP Request class.
 */
define constant $XMLHTTP30-class-id = as(<REFCLSID>, 
        "{F5078F35-C551-11D3-89B9-0000F81FE221}");

define function make-XMLHTTP30 () => (default-interface :: 
        <IXMLHTTPRequest>)
  let default-interface = make(<IXMLHTTPRequest>, class-id: 
        $XMLHTTP30-class-id);
  values(default-interface)
end function make-XMLHTTP30;


/* COM class: ServerXMLHTTP version 0.0
 * GUID: {AFBA6B42-5692-48EA-8141-DC517DCF0EF1}
 * Description: Server XML HTTP Request class.
 */
define constant $ServerXMLHTTP-class-id = as(<REFCLSID>, 
        "{AFBA6B42-5692-48EA-8141-DC517DCF0EF1}");

define function make-ServerXMLHTTP () => (default-interface :: 
        <IServerXMLHTTPRequest>)
  let default-interface = make(<IServerXMLHTTPRequest>, class-id: 
        $ServerXMLHTTP-class-id);
  values(default-interface)
end function make-ServerXMLHTTP;


/* COM class: ServerXMLHTTP30 version 0.0
 * GUID: {AFB40FFD-B609-40A3-9828-F88BBE11E4E3}
 * Description: Server XML HTTP Request class.
 */
define constant $ServerXMLHTTP30-class-id = as(<REFCLSID>, 
        "{AFB40FFD-B609-40A3-9828-F88BBE11E4E3}");

define function make-ServerXMLHTTP30 () => (default-interface :: 
        <IServerXMLHTTPRequest>)
  let default-interface = make(<IServerXMLHTTPRequest>, class-id: 
        $ServerXMLHTTP30-class-id);
  values(default-interface)
end function make-ServerXMLHTTP30;


/* Pointer definitions: */
define C-pointer-type <C-unsigned-short*> => <C-unsigned-short>; 
        ignorable(<C-unsigned-short*>);
define C-pointer-type <BSTR*> => <BSTR>; ignorable(<BSTR*>);
define C-pointer-type <_xml-error*> => <_xml-error>; 
        ignorable(<_xml-error*>);
