Module: type-library-module
Creator: created from "E:\projects\dylanlibs\dylanlibs\win32\adobe-svg-dispatch\type-library.spec" at 0: 2 2001- 6-23 New Zealand Standard Time.


/* Type library: SVGVIEWLib version 2.0
 * Description: Adobe SVG Viewer Type Library 2.0
 * GUID: {B2ADAF70-94C9-11D4-9064-00C04F78ACF9}
 */


/* COM class: SVGCtl version 0.0
 * GUID: {EBF9B040-94C9-11D4-9064-00C04F78ACF9}
 * Description: SVGCtl Class
 */
define constant $SVGCtl-class-id = as(<REFCLSID>, 
        "{EBF9B040-94C9-11D4-9064-00C04F78ACF9}");

define function make-SVGCtl () => (default-interface :: <ISVGCtl>)
  let default-interface = make(<ISVGCtl>, class-id: $SVGCtl-class-id);
  values(default-interface)
end function make-SVGCtl;


/* Dispatch interface: ISVGCtl version 0.0
 * GUID: {E1B4EB10-B9A3-11D4-9E52-0050BAA8920E}
 * Description: ISVGCtl Interface
 */
define dispatch-client <ISVGCtl>
  uuid "{E1B4EB10-B9A3-11D4-9E52-0050BAA8920E}";

  /* property SRC */
  property ISVGCtl/SRC :: <string>, name: "SRC", disp-id: 1;

  /* gets the src */
  function ISVGCtl/getSrc () => (arg-result :: <string>), name: "getSrc", 
        disp-id: as(<machine-word>, #x60020002);

  /* sets the src */
  function ISVGCtl/setSrc (arg-newVal :: <string>) => (), name: "setSrc", 
        disp-id: as(<machine-word>, #x60020003);

  /* Stock property ReadyState */
  constant property ISVGCtl/ReadyState :: type-union(<integer>, 
        <machine-word>), name: "ReadyState", disp-id: -525;

  /* method reload */
  function ISVGCtl/reload () => (), name: "reload", disp-id: 101;

  /* property DefaultFontFamily */
  property ISVGCtl/DefaultFontFamily :: <string>, name: 
        "DefaultFontFamily", disp-id: 2;

  /* getDefaultFontFamily */
  function ISVGCtl/getDefaultFontFamily () => (arg-result :: <string>), 
        name: "getDefaultFontFamily", disp-id: as(<machine-word>, 
        #x60020008);

  /* setDefaultFontFamily */
  function ISVGCtl/setDefaultFontFamily (arg-newVal :: <string>) => (), 
        name: "setDefaultFontFamily", disp-id: as(<machine-word>, 
        #x60020009);

  /* property DefaultFontSize */
  property ISVGCtl/DefaultFontSize :: <single-float>, name: 
        "DefaultFontSize", disp-id: 3;

  /* getDefaultFontSize */
  function ISVGCtl/getDefaultFontSize () => (arg-result :: <single-float>), 
        name: "getDefaultFontSize", disp-id: as(<machine-word>, 
        #x6002000C);

  /* setDefaultFontSize */
  function ISVGCtl/setDefaultFontSize (arg-newVal :: <single-float>) => (), 
        name: "setDefaultFontSize", disp-id: as(<machine-word>, 
        #x6002000D);

  /* property DefaultAntialias */
  property ISVGCtl/DefaultAntialias :: type-union(<integer>, 
        <machine-word>), name: "DefaultAntialias", disp-id: 4;

  /* getDefaultAntialias */
  function ISVGCtl/getDefaultAntialias () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: 
        "getDefaultAntialias", disp-id: as(<machine-word>, #x60020010);

  /* setDefaultAntialias */
  function ISVGCtl/setDefaultAntialias (arg-newVal :: type-union(<integer>, 
        <machine-word>)) => (), name: "setDefaultAntialias", disp-id: 
        as(<machine-word>, #x60020011);

  /* property FULLSCREEN */
  property ISVGCtl/FULLSCREEN :: <string>, name: "FULLSCREEN", disp-id: 18;

  /* property USE_SVGZ */
  property ISVGCtl/USE-SVGZ :: <string>, name: "USE_SVGZ", disp-id: 19;

  /* method getSVGDocument */
  function ISVGCtl/getSVGDocument () => (arg-result :: <ISVGDocument>), 
        name: "getSVGDocument", disp-id: 107;

  /* method getSVGViewerVersion */
  function ISVGCtl/getSVGViewerVersion () => (arg-result :: <string>), 
        name: "getSVGViewerVersion", disp-id: 106;

  /* method disableAutoUpdate */
  function ISVGCtl/disableAutoUpdate () => (), name: "disableAutoUpdate", 
        disp-id: 108;
end dispatch-client <ISVGCtl>;


/* Dispatch interface: ISVGDocument version 0.0
 * GUID: {E86F6020-10E5-11D4-904B-00C04F78ACF9}
 * Description: ISVGDocument Interface
 */
define dispatch-client <ISVGDocument>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentElement. */
  uuid "{E86F6020-10E5-11D4-904B-00C04F78ACF9}";

  /* name of the node */
  constant property ISVGDocument/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property ISVGDocument/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property ISVGDocument/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ISVGDocument/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ISVGDocument/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ISVGDocument/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ISVGDocument/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property ISVGDocument/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ISVGDocument/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ISVGDocument/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function ISVGDocument/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ISVGDocument/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ISVGDocument/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ISVGDocument/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ISVGDocument/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ISVGDocument/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function ISVGDocument/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* node corresponding to the DOCTYPE */
  constant property ISVGDocument/doctype :: <IDOMDocumentType>, name: 
        "doctype", disp-id: 66;

  /* info on this DOM implementation */
  constant property ISVGDocument/implementation :: <IDOMImplementation>, 
        name: "implementation", disp-id: 67;

  /* the root of the tree */
  constant property ISVGDocument/documentElement :: <IDOMElement>, name: 
        "documentElement", disp-id: 68;

  /* create an Element node */
  function ISVGDocument/createElement (arg-tagName :: <string>) => 
        (arg-result :: <IDOMElement>), name: "createElement", disp-id: 69;

  /* create a DocumentFragment node */
  function ISVGDocument/createDocumentFragment () => (arg-result :: 
        <IDOMDocumentFragment>), name: "createDocumentFragment", disp-id: 
        70;

  /* create a text node */
  function ISVGDocument/createTextNode (arg-data :: <string>) => 
        (arg-result :: <IDOMText>), name: "createTextNode", disp-id: 71;

  /* create a comment node */
  function ISVGDocument/createComment (arg-data :: <string>) => (arg-result 
        :: <IDOMComment>), name: "createComment", disp-id: 72;

  /* create a CDATA section node */
  function ISVGDocument/createCDATASection (arg-data :: <string>) => 
        (arg-result :: <IDOMCDATASection>), name: "createCDATASection", 
        disp-id: 73;

  /* create a processing instruction node */
  function ISVGDocument/createProcessingInstruction (arg-target :: 
        <string>, arg-data :: <string>) => (arg-result :: 
        <IDOMProcessingInstruction>), name: "createProcessingInstruction", 
        disp-id: 74;

  /* create an attribute node */
  function ISVGDocument/createAttribute (arg-name :: <string>) => 
        (arg-result :: <IDOMAttribute>), name: "createAttribute", disp-id: 
        75;

  /* create an entity reference node */
  function ISVGDocument/createEntityReference (arg-name :: <string>) => 
        (arg-result :: <IDOMEntityReference>), name: 
        "createEntityReference", disp-id: 77;

  /* build a list of elements by name */
  function ISVGDocument/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 78;

  /* name of the node */
  function ISVGDocument/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function ISVGDocument/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function ISVGDocument/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function ISVGDocument/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function ISVGDocument/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function ISVGDocument/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function ISVGDocument/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function ISVGDocument/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function ISVGDocument/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function ISVGDocument/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function ISVGDocument/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function ISVGDocument/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function ISVGDocument/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function ISVGDocument/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function ISVGDocument/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property ISVGDocument/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function ISVGDocument/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property ISVGDocument/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function ISVGDocument/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function ISVGDocument/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property ISVGDocument/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function ISVGDocument/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function ISVGDocument/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* garbageCollect method */
  function ISVGDocument/garbageCollect () => (), name: "garbageCollect", 
        disp-id: as(<machine-word>, #x60040018);

  /* constant: Element node */
  constant property ISVGDocument/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Attribute node */
  constant property ISVGDocument/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: Text node */
  constant property ISVGDocument/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6004001B);

  /* constant: CDATASection node */
  constant property ISVGDocument/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: EntityReference node */
  constant property ISVGDocument/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: Entity node */
  constant property ISVGDocument/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: ProcessingInstruction node */
  constant property ISVGDocument/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001F);

  /* constant: Comment node */
  constant property ISVGDocument/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: Document node */
  constant property ISVGDocument/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentType node */
  constant property ISVGDocument/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040022);

  /* constant: DocumentFragment node */
  constant property ISVGDocument/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040023);

  /* constant: Notation node */
  constant property ISVGDocument/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040024);

  /* node corresponding to the DOCTYPE */
  function ISVGDocument/getDoctype () => (arg-result :: 
        <IDOMDocumentType>), name: "getDoctype", disp-id: 
        as(<machine-word>, #x60040025);

  /* info on this DOM implementation */
  function ISVGDocument/getImplementation () => (arg-result :: 
        <IDOMImplementation>), name: "getImplementation", disp-id: 
        as(<machine-word>, #x60040026);

  /* the root of the tree */
  function ISVGDocument/getDocumentElement () => (arg-result :: 
        <IDOMElement>), name: "getDocumentElement", disp-id: 
        as(<machine-word>, #x60040027);

  /* the root of the tree */
  function ISVGDocument/setDocumentElement (arg-DOMElement :: 
        <IDOMElement>) => (), name: "setDocumentElement", disp-id: 
        as(<machine-word>, #x60040028);

  /* imports a node */
  function ISVGDocument/importNode (arg-importedNode :: <IDOMNode>, 
        arg-deep :: <boolean>) => (arg-result :: <IDOMNode>), name: 
        "importNode", disp-id: as(<machine-word>, #x60040029);

  /* creates an element */
  function ISVGDocument/createElementNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>) => (arg-result :: <IDOMElement>), 
        name: "createElementNS", disp-id: as(<machine-word>, #x6004002A);

  /* creates an attribute */
  function ISVGDocument/createAttributeNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>) => (arg-result :: <IDOMAttribute>), 
        name: "createAttributeNS", disp-id: as(<machine-word>, #x6004002B);

  /* gets a list of elements by tag name */
  function ISVGDocument/getElementsByTagNameNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: 
        <IDOMNodeList>), name: "getElementsByTagNameNS", disp-id: 
        as(<machine-word>, #x6004002C);

  /* gets an element by its ID attribute */
  function ISVGDocument/getElementById (arg-elementId :: <string>) => 
        (arg-result :: <IDOMElement>), name: "getElementById", disp-id: 
        as(<machine-word>, #x6004002D);

  /* title of document */
  property ISVGDocument/title :: <string>, name: "title", disp-id: 150;

  /* referrer of document */
  constant property ISVGDocument/referrer :: <string>, name: "referrer", 
        disp-id: 151;

  /* domain of document */
  constant property ISVGDocument/domain :: <string>, name: "domain", 
        disp-id: 152;

  /* URL of document */
  constant property ISVGDocument/URL :: <string>, name: "URL", disp-id: 
        153;

  /* root element of document */
  constant property ISVGDocument/rootElement :: <ISVGSVGElement>, name: 
        "rootElement", disp-id: 162;

  /* method suspend_redraw */
  function ISVGDocument/suspend-redraw (arg-max_wait_milliseconds :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "suspend_redraw", disp-id: 159;

  /* method unsuspend_redraw */
  function ISVGDocument/unsuspend-redraw (arg-suspend_handle_id :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "unsuspend_redraw", disp-id: 160;

  /* method unsuspend_redraw_all */
  function ISVGDocument/unsuspend-redraw-all () => (), name: 
        "unsuspend_redraw_all", disp-id: 161;

  /* title of document */
  function ISVGDocument/getTitle () => (arg-result :: <string>), name: 
        "getTitle", disp-id: as(<machine-word>, #x60050009);

  /* title of document */
  function ISVGDocument/setTitle (arg-title :: <string>) => (), name: 
        "setTitle", disp-id: as(<machine-word>, #x6005000A);

  /* referrer of document */
  function ISVGDocument/getReferrer () => (arg-result :: <string>), name: 
        "getReferrer", disp-id: as(<machine-word>, #x6005000B);

  /* domain of document */
  function ISVGDocument/getDomain () => (arg-result :: <string>), name: 
        "getDomain", disp-id: as(<machine-word>, #x6005000C);

  /* URL of document */
  function ISVGDocument/getURL () => (arg-result :: <string>), name: 
        "getURL", disp-id: as(<machine-word>, #x6005000D);

  /* root element of document */
  function ISVGDocument/getRootElement () => (arg-result :: 
        <ISVGSVGElement>), name: "getRootElement", disp-id: 
        as(<machine-word>, #x6005000E);
end dispatch-client <ISVGDocument>;


/* Dispatch interface: IDocument version 0.0
 * GUID: {887DCBD0-F7DC-11D4-9076-00C04F78ACF9}
 * Description: IDocument Interface
 */
define dispatch-client <IDocument>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentElement. */
  uuid "{887DCBD0-F7DC-11D4-9076-00C04F78ACF9}";

  /* name of the node */
  constant property IDocument/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDocument/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IDocument/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IDocument/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IDocument/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDocument/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IDocument/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDocument/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDocument/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDocument/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDocument/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IDocument/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IDocument/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDocument/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDocument/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDocument/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDocument/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* node corresponding to the DOCTYPE */
  constant property IDocument/doctype :: <IDOMDocumentType>, name: 
        "doctype", disp-id: 66;

  /* info on this DOM implementation */
  constant property IDocument/implementation :: <IDOMImplementation>, name: 
        "implementation", disp-id: 67;

  /* the root of the tree */
  constant property IDocument/documentElement :: <IDOMElement>, name: 
        "documentElement", disp-id: 68;

  /* create an Element node */
  function IDocument/createElement (arg-tagName :: <string>) => (arg-result 
        :: <IDOMElement>), name: "createElement", disp-id: 69;

  /* create a DocumentFragment node */
  function IDocument/createDocumentFragment () => (arg-result :: 
        <IDOMDocumentFragment>), name: "createDocumentFragment", disp-id: 
        70;

  /* create a text node */
  function IDocument/createTextNode (arg-data :: <string>) => (arg-result 
        :: <IDOMText>), name: "createTextNode", disp-id: 71;

  /* create a comment node */
  function IDocument/createComment (arg-data :: <string>) => (arg-result :: 
        <IDOMComment>), name: "createComment", disp-id: 72;

  /* create a CDATA section node */
  function IDocument/createCDATASection (arg-data :: <string>) => 
        (arg-result :: <IDOMCDATASection>), name: "createCDATASection", 
        disp-id: 73;

  /* create a processing instruction node */
  function IDocument/createProcessingInstruction (arg-target :: <string>, 
        arg-data :: <string>) => (arg-result :: 
        <IDOMProcessingInstruction>), name: "createProcessingInstruction", 
        disp-id: 74;

  /* create an attribute node */
  function IDocument/createAttribute (arg-name :: <string>) => (arg-result 
        :: <IDOMAttribute>), name: "createAttribute", disp-id: 75;

  /* create an entity reference node */
  function IDocument/createEntityReference (arg-name :: <string>) => 
        (arg-result :: <IDOMEntityReference>), name: 
        "createEntityReference", disp-id: 77;

  /* build a list of elements by name */
  function IDocument/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 78;

  /* name of the node */
  function IDocument/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IDocument/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IDocument/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IDocument/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function IDocument/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IDocument/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function IDocument/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IDocument/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IDocument/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60040008);

  /* right sibling of the node */
  function IDocument/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function IDocument/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IDocument/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IDocument/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IDocument/supports (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "supports", 
        disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IDocument/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IDocument/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IDocument/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property IDocument/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IDocument/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IDocument/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IDocument/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IDocument/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IDocument/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* garbageCollect method */
  function IDocument/garbageCollect () => (), name: "garbageCollect", 
        disp-id: as(<machine-word>, #x60040018);

  /* constant: Element node */
  constant property IDocument/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Attribute node */
  constant property IDocument/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: Text node */
  constant property IDocument/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6004001B);

  /* constant: CDATASection node */
  constant property IDocument/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: EntityReference node */
  constant property IDocument/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: Entity node */
  constant property IDocument/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: ProcessingInstruction node */
  constant property IDocument/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001F);

  /* constant: Comment node */
  constant property IDocument/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: Document node */
  constant property IDocument/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentType node */
  constant property IDocument/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040022);

  /* constant: DocumentFragment node */
  constant property IDocument/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040023);

  /* constant: Notation node */
  constant property IDocument/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040024);

  /* node corresponding to the DOCTYPE */
  function IDocument/getDoctype () => (arg-result :: <IDOMDocumentType>), 
        name: "getDoctype", disp-id: as(<machine-word>, #x60040025);

  /* info on this DOM implementation */
  function IDocument/getImplementation () => (arg-result :: 
        <IDOMImplementation>), name: "getImplementation", disp-id: 
        as(<machine-word>, #x60040026);

  /* the root of the tree */
  function IDocument/getDocumentElement () => (arg-result :: 
        <IDOMElement>), name: "getDocumentElement", disp-id: 
        as(<machine-word>, #x60040027);

  /* the root of the tree */
  function IDocument/setDocumentElement (arg-DOMElement :: <IDOMElement>) 
        => (), name: "setDocumentElement", disp-id: as(<machine-word>, 
        #x60040028);

  /* imports a node */
  function IDocument/importNode (arg-importedNode :: <IDOMNode>, arg-deep 
        :: <boolean>) => (arg-result :: <IDOMNode>), name: "importNode", 
        disp-id: as(<machine-word>, #x60040029);

  /* creates an element */
  function IDocument/createElementNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>) => (arg-result :: <IDOMElement>), 
        name: "createElementNS", disp-id: as(<machine-word>, #x6004002A);

  /* creates an attribute */
  function IDocument/createAttributeNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>) => (arg-result :: <IDOMAttribute>), 
        name: "createAttributeNS", disp-id: as(<machine-word>, #x6004002B);

  /* gets a list of elements by tag name */
  function IDocument/getElementsByTagNameNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMNodeList>), name: 
        "getElementsByTagNameNS", disp-id: as(<machine-word>, #x6004002C);

  /* gets an element by its ID attribute */
  function IDocument/getElementById (arg-elementId :: <string>) => 
        (arg-result :: <IDOMElement>), name: "getElementById", disp-id: 
        as(<machine-word>, #x6004002D);
end dispatch-client <IDocument>;


/* Dispatch interface: IDOMDocument version 0.0
 * GUID: {3EFAA414-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMDocument>
  /* Translation error: Cannot translate PROPERTYPUTREF method 
        documentElement. */
  uuid "{3EFAA414-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMDocument/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMDocument/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDOMDocument/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMDocument/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMDocument/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMDocument/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMDocument/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMDocument/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMDocument/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMDocument/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMDocument/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMDocument/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMDocument/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMDocument/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMDocument/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMDocument/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMDocument/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* node corresponding to the DOCTYPE */
  constant property IDOMDocument/doctype :: <IDOMDocumentType>, name: 
        "doctype", disp-id: 66;

  /* info on this DOM implementation */
  constant property IDOMDocument/implementation :: <IDOMImplementation>, 
        name: "implementation", disp-id: 67;

  /* the root of the tree */
  constant property IDOMDocument/documentElement :: <IDOMElement>, name: 
        "documentElement", disp-id: 68;

  /* create an Element node */
  function IDOMDocument/createElement (arg-tagName :: <string>) => 
        (arg-result :: <IDOMElement>), name: "createElement", disp-id: 69;

  /* create a DocumentFragment node */
  function IDOMDocument/createDocumentFragment () => (arg-result :: 
        <IDOMDocumentFragment>), name: "createDocumentFragment", disp-id: 
        70;

  /* create a text node */
  function IDOMDocument/createTextNode (arg-data :: <string>) => 
        (arg-result :: <IDOMText>), name: "createTextNode", disp-id: 71;

  /* create a comment node */
  function IDOMDocument/createComment (arg-data :: <string>) => (arg-result 
        :: <IDOMComment>), name: "createComment", disp-id: 72;

  /* create a CDATA section node */
  function IDOMDocument/createCDATASection (arg-data :: <string>) => 
        (arg-result :: <IDOMCDATASection>), name: "createCDATASection", 
        disp-id: 73;

  /* create a processing instruction node */
  function IDOMDocument/createProcessingInstruction (arg-target :: 
        <string>, arg-data :: <string>) => (arg-result :: 
        <IDOMProcessingInstruction>), name: "createProcessingInstruction", 
        disp-id: 74;

  /* create an attribute node */
  function IDOMDocument/createAttribute (arg-name :: <string>) => 
        (arg-result :: <IDOMAttribute>), name: "createAttribute", disp-id: 
        75;

  /* create an entity reference node */
  function IDOMDocument/createEntityReference (arg-name :: <string>) => 
        (arg-result :: <IDOMEntityReference>), name: 
        "createEntityReference", disp-id: 77;

  /* build a list of elements by name */
  function IDOMDocument/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 78;
end dispatch-client <IDOMDocument>;


/* Dispatch interface: IDOMNode version 0.0
 * GUID: {3EFAA411-272F-11D2-836F-0000F87A7782}
 * Description: Core DOM node interface
 */
define dispatch-client <IDOMNode>
  uuid "{3EFAA411-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMNode/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMNode/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IDOMNode/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IDOMNode/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMNode/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMNode/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IDOMNode/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDOMNode/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMNode/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMNode/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMNode/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IDOMNode/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IDOMNode/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMNode/appendChild (arg-newChild :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMNode/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMNode/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMNode/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;
end dispatch-client <IDOMNode>;


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


/* Dispatch interface: IDOMNodeList version 0.0
 * GUID: {3EFAA416-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMNodeList>
  uuid "{3EFAA416-272F-11D2-836F-0000F87A7782}";

  /* collection of nodes */
  element constant property IDOMNodeList/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IDOMNode>, name: "item", 
        disp-id: 0;

  /* number of nodes in the collection */
  constant property IDOMNodeList/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 387;
end dispatch-client <IDOMNodeList>;


/* Dispatch interface: IDOMNamedNodeMap version 0.0
 * GUID: {3EFAA418-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMNamedNodeMap>
  uuid "{3EFAA418-272F-11D2-836F-0000F87A7782}";

  /* lookup item by name */
  function IDOMNamedNodeMap/getNamedItem (arg-name :: <string>) => 
        (arg-result :: <IDOMNode>), name: "getNamedItem", disp-id: 420;

  /* set item by name */
  function IDOMNamedNodeMap/setNamedItem (arg-newItem :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "setNamedItem", disp-id: 421;

  /* remove item by name */
  function IDOMNamedNodeMap/removeNamedItem (arg-name :: <string>) => 
        (arg-result :: <IDOMNode>), name: "removeNamedItem", disp-id: 422;

  /* collection of nodes */
  element constant property IDOMNamedNodeMap/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IDOMNode>, name: "item", 
        disp-id: 0;

  /* number of nodes in the collection */
  constant property IDOMNamedNodeMap/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 387;
end dispatch-client <IDOMNamedNodeMap>;


/* Dispatch interface: IDOMDocumentType version 0.0
 * GUID: {3EFAA421-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMDocumentType>
  uuid "{3EFAA421-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMDocumentType/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMDocumentType/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDOMDocumentType/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMDocumentType/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMDocumentType/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMDocumentType/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMDocumentType/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMDocumentType/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMDocumentType/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMDocumentType/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMDocumentType/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMDocumentType/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMDocumentType/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMDocumentType/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMDocumentType/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMDocumentType/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMDocumentType/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* name of the document type (root of the tree) */
  constant property IDOMDocumentType/name :: <string>, name: "name", 
        disp-id: 258;

  /* a list of entities in the document */
  constant property IDOMDocumentType/entities :: <IDOMNamedNodeMap>, name: 
        "entities", disp-id: 259;

  /* a list of notations in the document */
  constant property IDOMDocumentType/notations :: <IDOMNamedNodeMap>, name: 
        "notations", disp-id: 260;
end dispatch-client <IDOMDocumentType>;


/* Dispatch interface: IDOMImplementation version 0.0
 * GUID: {3EFAA410-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMImplementation>
  uuid "{3EFAA410-272F-11D2-836F-0000F87A7782}";

  function IDOMImplementation/hasFeature (arg-feature :: <string>, 
        arg-version :: <string>) => (arg-result :: <boolean>), name: 
        "hasFeature", disp-id: 450;
end dispatch-client <IDOMImplementation>;


/* Dispatch interface: IDOMElement version 0.0
 * GUID: {3EFAA41C-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMElement>
  uuid "{3EFAA41C-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMElement/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMElement/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDOMElement/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMElement/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMElement/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMElement/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMElement/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDOMElement/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMElement/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMElement/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMElement/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMElement/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMElement/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMElement/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMElement/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMElement/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMElement/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get the tagName of the element */
  constant property IDOMElement/tagName :: <string>, name: "tagName", 
        disp-id: 98;

  /* look up the string value of an attribute by name */
  function IDOMElement/getAttribute (arg-name :: <string>) => (arg-result 
        :: <object>), name: "getAttribute", disp-id: 100;

  /* set the string value of an attribute by name */
  function IDOMElement/setAttribute (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setAttribute", disp-id: 101;

  /* remove an attribute by name */
  function IDOMElement/removeAttribute (arg-name :: <string>) => (), name: 
        "removeAttribute", disp-id: 102;

  /* look up the attribute node by name */
  function IDOMElement/getAttributeNode (arg-name :: <string>) => 
        (arg-result :: <IDOMAttribute>), name: "getAttributeNode", disp-id: 
        103;

  /* set the specified attribute on the element */
  function IDOMElement/setAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNode", disp-id: 104;

  /* remove the specified attribute */
  function IDOMElement/removeAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 105;

  /* build a list of elements by name */
  function IDOMElement/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 106;

  /* collapse all adjacent text nodes in sub-tree */
  function IDOMElement/normalize () => (), name: "normalize", disp-id: 107;
end dispatch-client <IDOMElement>;


/* Dispatch interface: IDOMAttribute version 0.0
 * GUID: {3EFAA41B-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMAttribute>
  uuid "{3EFAA41B-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMAttribute/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMAttribute/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDOMAttribute/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMAttribute/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMAttribute/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMAttribute/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMAttribute/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMAttribute/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMAttribute/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMAttribute/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMAttribute/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMAttribute/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMAttribute/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMAttribute/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMAttribute/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMAttribute/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMAttribute/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get name of the attribute */
  constant property IDOMAttribute/name :: <string>, name: "name", disp-id: 
        162;

  /* indicates whether attribute has a default value */
  constant property IDOMAttribute/specified :: <boolean>, name: 
        "specified", disp-id: 163;

  /* string value of the attribute */
  property IDOMAttribute/value :: <object>, name: "value", disp-id: 0;
end dispatch-client <IDOMAttribute>;


/* Dispatch interface: IDOMDocumentFragment version 0.0
 * GUID: {3EFAA413-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMDocumentFragment>
  uuid "{3EFAA413-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMDocumentFragment/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMDocumentFragment/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDOMDocumentFragment/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMDocumentFragment/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMDocumentFragment/childNodes :: <IDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMDocumentFragment/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMDocumentFragment/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMDocumentFragment/previousSibling :: <IDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMDocumentFragment/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMDocumentFragment/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMDocumentFragment/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMDocumentFragment/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMDocumentFragment/removeChild (arg-childNode :: <IDOMNode>) 
        => (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMDocumentFragment/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMDocumentFragment/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMDocumentFragment/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IDOMDocumentFragment/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;
end dispatch-client <IDOMDocumentFragment>;


/* Dispatch interface: IDOMText version 0.0
 * GUID: {9CAFC72D-272E-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMText>
  uuid "{9CAFC72D-272E-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMText/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMText/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IDOMText/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IDOMText/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMText/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMText/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IDOMText/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDOMText/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMText/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMText/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMText/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IDOMText/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IDOMText/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMText/appendChild (arg-newChild :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMText/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMText/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMText/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IDOMText/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IDOMText/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IDOMText/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 131;

  /* append string to value */
  function IDOMText/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IDOMText/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function IDOMText/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function IDOMText/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;

  /* split the text node into two text nodes at the position specified */
  function IDOMText/splitText (arg-offset :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IDOMText>), name: "splitText", 
        disp-id: 194;
end dispatch-client <IDOMText>;


/* Dispatch interface: IDOMCharacterData version 0.0
 * GUID: {3EFAA41A-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMCharacterData>
  uuid "{3EFAA41A-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMCharacterData/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMCharacterData/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDOMCharacterData/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMCharacterData/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMCharacterData/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMCharacterData/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMCharacterData/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMCharacterData/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMCharacterData/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMCharacterData/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMCharacterData/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMCharacterData/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMCharacterData/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMCharacterData/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMCharacterData/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMCharacterData/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IDOMCharacterData/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IDOMCharacterData/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IDOMCharacterData/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IDOMCharacterData/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 131;

  /* append string to value */
  function IDOMCharacterData/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IDOMCharacterData/insertData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "insertData", disp-id: 133;

  /* delete string within the value */
  function IDOMCharacterData/deleteData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (), name: "deleteData", 
        disp-id: 134;

  /* replace string within the value */
  function IDOMCharacterData/replaceData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "replaceData", disp-id: 135;
end dispatch-client <IDOMCharacterData>;


/* Dispatch interface: IDOMComment version 0.0
 * GUID: {3EFAA41E-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMComment>
  uuid "{3EFAA41E-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMComment/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMComment/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDOMComment/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMComment/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMComment/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMComment/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMComment/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDOMComment/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMComment/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMComment/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMComment/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMComment/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMComment/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMComment/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMComment/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMComment/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMComment/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IDOMComment/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IDOMComment/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IDOMComment/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 131;

  /* append string to value */
  function IDOMComment/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IDOMComment/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function IDOMComment/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function IDOMComment/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;
end dispatch-client <IDOMComment>;


/* Dispatch interface: IDOMCDATASection version 0.0
 * GUID: {3EFAA420-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMCDATASection>
  uuid "{3EFAA420-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMCDATASection/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMCDATASection/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDOMCDATASection/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMCDATASection/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMCDATASection/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMCDATASection/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMCDATASection/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMCDATASection/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMCDATASection/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMCDATASection/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMCDATASection/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMCDATASection/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMCDATASection/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMCDATASection/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMCDATASection/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMCDATASection/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMCDATASection/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IDOMCDATASection/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IDOMCDATASection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IDOMCDATASection/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 131;

  /* append string to value */
  function IDOMCDATASection/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IDOMCDATASection/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function IDOMCDATASection/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function IDOMCDATASection/replaceData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>), arg-data :: <string>) => (), 
        name: "replaceData", disp-id: 135;

  /* split the text node into two text nodes at the position specified */
  function IDOMCDATASection/splitText (arg-offset :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IDOMText>), name: "splitText", 
        disp-id: 194;
end dispatch-client <IDOMCDATASection>;


/* Dispatch interface: IDOMProcessingInstruction version 0.0
 * GUID: {3EFAA41F-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMProcessingInstruction>
  uuid "{3EFAA41F-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMProcessingInstruction/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMProcessingInstruction/nodeValue :: <object>, name: 
        "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IDOMProcessingInstruction/nodeType :: <DOMNodeType>, 
        name: "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMProcessingInstruction/parentNode :: <IDOMNode>, 
        name: "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMProcessingInstruction/childNodes :: <IDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMProcessingInstruction/firstChild :: <IDOMNode>, 
        name: "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMProcessingInstruction/lastChild :: <IDOMNode>, 
        name: "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMProcessingInstruction/previousSibling :: 
        <IDOMNode>, name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMProcessingInstruction/nextSibling :: <IDOMNode>, 
        name: "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMProcessingInstruction/attributes :: 
        <IDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMProcessingInstruction/insertBefore (arg-newChild :: 
        <IDOMNode>, arg-refChild :: <object>) => (arg-result :: 
        <IDOMNode>), name: "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMProcessingInstruction/replaceChild (arg-newChild :: 
        <IDOMNode>, arg-oldChild :: <IDOMNode>) => (arg-result :: 
        <IDOMNode>), name: "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMProcessingInstruction/removeChild (arg-childNode :: 
        <IDOMNode>) => (arg-result :: <IDOMNode>), name: "removeChild", 
        disp-id: 15;

  /* append a child node */
  function IDOMProcessingInstruction/appendChild (arg-newChild :: 
        <IDOMNode>) => (arg-result :: <IDOMNode>), name: "appendChild", 
        disp-id: 16;

  function IDOMProcessingInstruction/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMProcessingInstruction/ownerDocument :: 
        <IDOMDocument>, name: "ownerDocument", disp-id: 18;

  function IDOMProcessingInstruction/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the target */
  constant property IDOMProcessingInstruction/target :: <string>, name: 
        "target", disp-id: 226;

  /* the data */
  property IDOMProcessingInstruction/data :: <string>, name: "data", 
        disp-id: 0;
end dispatch-client <IDOMProcessingInstruction>;


/* Dispatch interface: IDOMEntityReference version 0.0
 * GUID: {3EFAA424-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMEntityReference>
  uuid "{3EFAA424-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMEntityReference/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDOMEntityReference/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDOMEntityReference/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMEntityReference/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMEntityReference/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMEntityReference/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMEntityReference/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMEntityReference/previousSibling :: <IDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMEntityReference/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMEntityReference/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMEntityReference/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMEntityReference/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMEntityReference/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMEntityReference/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMEntityReference/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMEntityReference/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IDOMEntityReference/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;
end dispatch-client <IDOMEntityReference>;


/* Dispatch interface: ISVGSVGElement version 0.0
 * GUID: {CE1E8FA0-B506-11D4-906B-00C04F78ACF9}
 * Description: ISVGSVGElement Interface
 */
define dispatch-client <ISVGSVGElement>
  uuid "{CE1E8FA0-B506-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property ISVGSVGElement/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property ISVGSVGElement/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property ISVGSVGElement/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ISVGSVGElement/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ISVGSVGElement/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ISVGSVGElement/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ISVGSVGElement/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property ISVGSVGElement/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ISVGSVGElement/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ISVGSVGElement/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function ISVGSVGElement/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ISVGSVGElement/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ISVGSVGElement/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ISVGSVGElement/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ISVGSVGElement/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ISVGSVGElement/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function ISVGSVGElement/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get the tagName of the element */
  constant property ISVGSVGElement/tagName :: <string>, name: "tagName", 
        disp-id: 98;

  /* look up the string value of an attribute by name */
  function ISVGSVGElement/getAttribute (arg-name :: <string>) => 
        (arg-result :: <object>), name: "getAttribute", disp-id: 100;

  /* set the string value of an attribute by name */
  function ISVGSVGElement/setAttribute (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setAttribute", disp-id: 101;

  /* remove an attribute by name */
  function ISVGSVGElement/removeAttribute (arg-name :: <string>) => (), 
        name: "removeAttribute", disp-id: 102;

  /* look up the attribute node by name */
  function ISVGSVGElement/getAttributeNode (arg-name :: <string>) => 
        (arg-result :: <IDOMAttribute>), name: "getAttributeNode", disp-id: 
        103;

  /* set the specified attribute on the element */
  function ISVGSVGElement/setAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNode", disp-id: 104;

  /* remove the specified attribute */
  function ISVGSVGElement/removeAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 105;

  /* build a list of elements by name */
  function ISVGSVGElement/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 106;

  /* collapse all adjacent text nodes in sub-tree */
  function ISVGSVGElement/normalize () => (), name: "normalize", disp-id: 
        107;

  /* name of the node */
  function ISVGSVGElement/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function ISVGSVGElement/getNodeValue () => (arg-result :: <object>), 
        name: "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function ISVGSVGElement/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function ISVGSVGElement/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function ISVGSVGElement/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function ISVGSVGElement/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function ISVGSVGElement/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function ISVGSVGElement/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function ISVGSVGElement/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function ISVGSVGElement/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function ISVGSVGElement/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function ISVGSVGElement/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* queries support of given feature */
  function ISVGSVGElement/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function ISVGSVGElement/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000D);

  /* value of the namespaceURI */
  constant property ISVGSVGElement/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000E);

  /* gets URI of node's namespace */
  function ISVGSVGElement/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* value of the prefix */
  property ISVGSVGElement/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040010);

  /* gets node's prefix */
  function ISVGSVGElement/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040012);

  /* sets node's prefix */
  function ISVGSVGElement/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040013);

  /* value of the localName */
  constant property ISVGSVGElement/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040014);

  /* gets local name of the node */
  function ISVGSVGElement/getLocalName () => (arg-result :: <string>), 
        name: "getLocalName", disp-id: as(<machine-word>, #x60040015);

  /* queries if node has attributes */
  function ISVGSVGElement/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040016);

  /* constant: Element node */
  constant property ISVGSVGElement/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040017);

  /* constant: Attribute node */
  constant property ISVGSVGElement/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Text node */
  constant property ISVGSVGElement/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: CDATASection node */
  constant property ISVGSVGElement/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: EntityReference node */
  constant property ISVGSVGElement/ENTITY-REFERENCE-NODE :: <integer>, 
        name: "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, 
        #x6004001B);

  /* constant: Entity node */
  constant property ISVGSVGElement/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: ProcessingInstruction node */
  constant property ISVGSVGElement/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001D);

  /* constant: Comment node */
  constant property ISVGSVGElement/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: Document node */
  constant property ISVGSVGElement/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: DocumentType node */
  constant property ISVGSVGElement/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentFragment node */
  constant property ISVGSVGElement/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60040021);

  /* constant: Notation node */
  constant property ISVGSVGElement/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040022);

  /* get the tagName of the element */
  function ISVGSVGElement/getTagName () => (arg-result :: <string>), name: 
        "getTagName", disp-id: as(<machine-word>, #x60040023);

  /* add an EventListener */
  function ISVGSVGElement/addEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x60040024);

  /* remove an EventListener */
  function ISVGSVGElement/removeEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "removeEventListener", disp-id: as(<machine-word>, #x60040025);

  /* dispatch an event */
  function ISVGSVGElement/dispatchEvent (arg-evt :: <IEvent>) => 
        (arg-result :: <boolean>), name: "dispatchEvent", disp-id: 
        as(<machine-word>, #x60040026);

  /* gets an attribute */
  function ISVGSVGElement/getAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <string>), name: 
        "getAttributeNS", disp-id: as(<machine-word>, #x60040027);

  /* sets an attribute */
  function ISVGSVGElement/setAttributeNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>, arg-value :: <string>) => (), name: 
        "setAttributeNS", disp-id: as(<machine-word>, #x60040028);

  /* remove an attribute */
  function ISVGSVGElement/removeAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (), name: "removeAttributeNS", 
        disp-id: as(<machine-word>, #x60040029);

  /* get an attribute node */
  function ISVGSVGElement/getAttributeNodeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMAttribute>), 
        name: "getAttributeNodeNS", disp-id: as(<machine-word>, 
        #x6004002A);

  /* set an attribute node */
  function ISVGSVGElement/setAttributeNodeNS (arg-newAttr :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNodeNS", disp-id: as(<machine-word>, #x6004002B);

  /* get elements by tag name */
  function ISVGSVGElement/getElementsByTagNameNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: 
        <IDOMNodeList>), name: "getElementsByTagNameNS", disp-id: 
        as(<machine-word>, #x6004002C);

  /* does element have the attribute? */
  function ISVGSVGElement/hasAttribute (arg-name :: <string>) => 
        (arg-result :: <boolean>), name: "hasAttribute", disp-id: 
        as(<machine-word>, #x6004002D);

  /* does element have the attribute? */
  function ISVGSVGElement/hasAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <boolean>), name: 
        "hasAttributeNS", disp-id: as(<machine-word>, #x6004002E);

  /* method getStyle */
  function ISVGSVGElement/getStyle () => (arg-result :: 
        <ICSSStyleDeclaration>), name: "getStyle", disp-id: 121;

  /* property style */
  constant property ISVGSVGElement/style :: <ICSSStyleDeclaration>, name: 
        "style", disp-id: 123;

  /* property id */
  property ISVGSVGElement/id :: <string>, name: "id", disp-id: 125;

  /* method getId */
  function ISVGSVGElement/getId () => (arg-result :: <string>), name: 
        "getId", disp-id: as(<machine-word>, #x60050004);

  /* method setId */
  function ISVGSVGElement/setId (arg-id :: <string>) => (), name: "setId", 
        disp-id: as(<machine-word>, #x60050005);

  /* property ownerSVGElement */
  constant property ISVGSVGElement/ownerSVGElement :: <ISVGSVGElement>, 
        name: "ownerSVGElement", disp-id: 126;

  /* method getOwnerSVGElement */
  function ISVGSVGElement/getOwnerSVGElement () => (arg-result :: 
        <ISVGSVGElement>), name: "getOwnerSVGElement", disp-id: 
        as(<machine-word>, #x60050007);

  /* property viewportElement */
  constant property ISVGSVGElement/viewportElement :: <ISVGElement>, name: 
        "viewportElement", disp-id: 127;

  /* method getViewportElement */
  function ISVGSVGElement/getViewportElement () => (arg-result :: 
        <ISVGElement>), name: "getViewportElement", disp-id: 
        as(<machine-word>, #x60050009);

  /* property currentScale */
  property ISVGSVGElement/currentScale :: <single-float>, name: 
        "currentScale", disp-id: 250;

  /* property currentTranslate */
  constant property ISVGSVGElement/currentTranslate :: <ISVGPoint>, name: 
        "currentTranslate", disp-id: 251;

  /* method getCurrentScale */
  function ISVGSVGElement/getCurrentScale () => (arg-result :: 
        <single-float>), name: "getCurrentScale", disp-id: 
        as(<machine-word>, #x60060003);

  /* method setCurrentScale */
  function ISVGSVGElement/setCurrentScale (arg-curScale :: <single-float>) 
        => (), name: "setCurrentScale", disp-id: as(<machine-word>, 
        #x60060004);

  /* method getCurrentTranslate */
  function ISVGSVGElement/getCurrentTranslate () => (arg-result :: 
        <ISVGPoint>), name: "getCurrentTranslate", disp-id: 
        as(<machine-word>, #x60060005);
end dispatch-client <ISVGSVGElement>;


/* Dispatch interface: ISVGElement version 0.0
 * GUID: {2064FF50-93F9-11D4-9063-00C04F78ACF9}
 * Description: ISVGElement Interface
 */
define dispatch-client <ISVGElement>
  uuid "{2064FF50-93F9-11D4-9063-00C04F78ACF9}";

  /* name of the node */
  constant property ISVGElement/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property ISVGElement/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property ISVGElement/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ISVGElement/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ISVGElement/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ISVGElement/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ISVGElement/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property ISVGElement/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ISVGElement/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ISVGElement/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function ISVGElement/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ISVGElement/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ISVGElement/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ISVGElement/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ISVGElement/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ISVGElement/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function ISVGElement/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get the tagName of the element */
  constant property ISVGElement/tagName :: <string>, name: "tagName", 
        disp-id: 98;

  /* look up the string value of an attribute by name */
  function ISVGElement/getAttribute (arg-name :: <string>) => (arg-result 
        :: <object>), name: "getAttribute", disp-id: 100;

  /* set the string value of an attribute by name */
  function ISVGElement/setAttribute (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setAttribute", disp-id: 101;

  /* remove an attribute by name */
  function ISVGElement/removeAttribute (arg-name :: <string>) => (), name: 
        "removeAttribute", disp-id: 102;

  /* look up the attribute node by name */
  function ISVGElement/getAttributeNode (arg-name :: <string>) => 
        (arg-result :: <IDOMAttribute>), name: "getAttributeNode", disp-id: 
        103;

  /* set the specified attribute on the element */
  function ISVGElement/setAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNode", disp-id: 104;

  /* remove the specified attribute */
  function ISVGElement/removeAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 105;

  /* build a list of elements by name */
  function ISVGElement/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 106;

  /* collapse all adjacent text nodes in sub-tree */
  function ISVGElement/normalize () => (), name: "normalize", disp-id: 107;

  /* name of the node */
  function ISVGElement/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function ISVGElement/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function ISVGElement/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function ISVGElement/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function ISVGElement/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function ISVGElement/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function ISVGElement/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function ISVGElement/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function ISVGElement/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60040008);

  /* right sibling of the node */
  function ISVGElement/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function ISVGElement/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function ISVGElement/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* queries support of given feature */
  function ISVGElement/supports (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "supports", 
        disp-id: as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function ISVGElement/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000D);

  /* value of the namespaceURI */
  constant property ISVGElement/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000E);

  /* gets URI of node's namespace */
  function ISVGElement/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* value of the prefix */
  property ISVGElement/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040010);

  /* gets node's prefix */
  function ISVGElement/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040012);

  /* sets node's prefix */
  function ISVGElement/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040013);

  /* value of the localName */
  constant property ISVGElement/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040014);

  /* gets local name of the node */
  function ISVGElement/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040015);

  /* queries if node has attributes */
  function ISVGElement/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040016);

  /* constant: Element node */
  constant property ISVGElement/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040017);

  /* constant: Attribute node */
  constant property ISVGElement/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Text node */
  constant property ISVGElement/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x60040019);

  /* constant: CDATASection node */
  constant property ISVGElement/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: EntityReference node */
  constant property ISVGElement/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: Entity node */
  constant property ISVGElement/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: ProcessingInstruction node */
  constant property ISVGElement/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001D);

  /* constant: Comment node */
  constant property ISVGElement/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: Document node */
  constant property ISVGElement/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: DocumentType node */
  constant property ISVGElement/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentFragment node */
  constant property ISVGElement/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: Notation node */
  constant property ISVGElement/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040022);

  /* get the tagName of the element */
  function ISVGElement/getTagName () => (arg-result :: <string>), name: 
        "getTagName", disp-id: as(<machine-word>, #x60040023);

  /* add an EventListener */
  function ISVGElement/addEventListener (arg-type :: <string>, arg-listener 
        :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x60040024);

  /* remove an EventListener */
  function ISVGElement/removeEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "removeEventListener", disp-id: as(<machine-word>, #x60040025);

  /* dispatch an event */
  function ISVGElement/dispatchEvent (arg-evt :: <IEvent>) => (arg-result 
        :: <boolean>), name: "dispatchEvent", disp-id: as(<machine-word>, 
        #x60040026);

  /* gets an attribute */
  function ISVGElement/getAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <string>), name: 
        "getAttributeNS", disp-id: as(<machine-word>, #x60040027);

  /* sets an attribute */
  function ISVGElement/setAttributeNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>, arg-value :: <string>) => (), name: 
        "setAttributeNS", disp-id: as(<machine-word>, #x60040028);

  /* remove an attribute */
  function ISVGElement/removeAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (), name: "removeAttributeNS", 
        disp-id: as(<machine-word>, #x60040029);

  /* get an attribute node */
  function ISVGElement/getAttributeNodeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMAttribute>), 
        name: "getAttributeNodeNS", disp-id: as(<machine-word>, 
        #x6004002A);

  /* set an attribute node */
  function ISVGElement/setAttributeNodeNS (arg-newAttr :: <IDOMAttribute>) 
        => (arg-result :: <IDOMAttribute>), name: "setAttributeNodeNS", 
        disp-id: as(<machine-word>, #x6004002B);

  /* get elements by tag name */
  function ISVGElement/getElementsByTagNameNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: 
        <IDOMNodeList>), name: "getElementsByTagNameNS", disp-id: 
        as(<machine-word>, #x6004002C);

  /* does element have the attribute? */
  function ISVGElement/hasAttribute (arg-name :: <string>) => (arg-result 
        :: <boolean>), name: "hasAttribute", disp-id: as(<machine-word>, 
        #x6004002D);

  /* does element have the attribute? */
  function ISVGElement/hasAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <boolean>), name: 
        "hasAttributeNS", disp-id: as(<machine-word>, #x6004002E);

  /* method getStyle */
  function ISVGElement/getStyle () => (arg-result :: 
        <ICSSStyleDeclaration>), name: "getStyle", disp-id: 121;

  /* property style */
  constant property ISVGElement/style :: <ICSSStyleDeclaration>, name: 
        "style", disp-id: 123;

  /* property id */
  property ISVGElement/id :: <string>, name: "id", disp-id: 125;

  /* method getId */
  function ISVGElement/getId () => (arg-result :: <string>), name: "getId", 
        disp-id: as(<machine-word>, #x60050004);

  /* method setId */
  function ISVGElement/setId (arg-id :: <string>) => (), name: "setId", 
        disp-id: as(<machine-word>, #x60050005);

  /* property ownerSVGElement */
  constant property ISVGElement/ownerSVGElement :: <ISVGSVGElement>, name: 
        "ownerSVGElement", disp-id: 126;

  /* method getOwnerSVGElement */
  function ISVGElement/getOwnerSVGElement () => (arg-result :: 
        <ISVGSVGElement>), name: "getOwnerSVGElement", disp-id: 
        as(<machine-word>, #x60050007);

  /* property viewportElement */
  constant property ISVGElement/viewportElement :: <ISVGElement>, name: 
        "viewportElement", disp-id: 127;

  /* method getViewportElement */
  function ISVGElement/getViewportElement () => (arg-result :: 
        <ISVGElement>), name: "getViewportElement", disp-id: 
        as(<machine-word>, #x60050009);
end dispatch-client <ISVGElement>;


/* Dispatch interface: IElement version 0.0
 * GUID: {DC3697B0-F7DC-11D4-9076-00C04F78ACF9}
 * Description: IElement Interface
 */
define dispatch-client <IElement>
  uuid "{DC3697B0-F7DC-11D4-9076-00C04F78ACF9}";

  /* name of the node */
  constant property IElement/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IElement/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IElement/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IElement/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IElement/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IElement/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IElement/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IElement/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IElement/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IElement/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IElement/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IElement/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IElement/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IElement/appendChild (arg-newChild :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IElement/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IElement/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IElement/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get the tagName of the element */
  constant property IElement/tagName :: <string>, name: "tagName", disp-id: 
        98;

  /* look up the string value of an attribute by name */
  function IElement/getAttribute (arg-name :: <string>) => (arg-result :: 
        <object>), name: "getAttribute", disp-id: 100;

  /* set the string value of an attribute by name */
  function IElement/setAttribute (arg-name :: <string>, arg-value :: 
        <object>) => (), name: "setAttribute", disp-id: 101;

  /* remove an attribute by name */
  function IElement/removeAttribute (arg-name :: <string>) => (), name: 
        "removeAttribute", disp-id: 102;

  /* look up the attribute node by name */
  function IElement/getAttributeNode (arg-name :: <string>) => (arg-result 
        :: <IDOMAttribute>), name: "getAttributeNode", disp-id: 103;

  /* set the specified attribute on the element */
  function IElement/setAttributeNode (arg-DOMAttribute :: <IDOMAttribute>) 
        => (arg-result :: <IDOMAttribute>), name: "setAttributeNode", 
        disp-id: 104;

  /* remove the specified attribute */
  function IElement/removeAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 105;

  /* build a list of elements by name */
  function IElement/getElementsByTagName (arg-tagName :: <string>) => 
        (arg-result :: <IDOMNodeList>), name: "getElementsByTagName", 
        disp-id: 106;

  /* collapse all adjacent text nodes in sub-tree */
  function IElement/normalize () => (), name: "normalize", disp-id: 107;

  /* name of the node */
  function IElement/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IElement/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IElement/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IElement/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function IElement/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IElement/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function IElement/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IElement/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IElement/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60040008);

  /* right sibling of the node */
  function IElement/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function IElement/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IElement/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6004000B);

  /* queries support of given feature */
  function IElement/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IElement/isSupported (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x6004000D);

  /* value of the namespaceURI */
  constant property IElement/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000E);

  /* gets URI of node's namespace */
  function IElement/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* value of the prefix */
  property IElement/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040010);

  /* gets node's prefix */
  function IElement/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040012);

  /* sets node's prefix */
  function IElement/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040013);

  /* value of the localName */
  constant property IElement/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040014);

  /* gets local name of the node */
  function IElement/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040015);

  /* queries if node has attributes */
  function IElement/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040016);

  /* constant: Element node */
  constant property IElement/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040017);

  /* constant: Attribute node */
  constant property IElement/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Text node */
  constant property IElement/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x60040019);

  /* constant: CDATASection node */
  constant property IElement/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: EntityReference node */
  constant property IElement/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: Entity node */
  constant property IElement/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x6004001C);

  /* constant: ProcessingInstruction node */
  constant property IElement/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001D);

  /* constant: Comment node */
  constant property IElement/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: Document node */
  constant property IElement/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: DocumentType node */
  constant property IElement/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentFragment node */
  constant property IElement/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: Notation node */
  constant property IElement/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040022);

  /* get the tagName of the element */
  function IElement/getTagName () => (arg-result :: <string>), name: 
        "getTagName", disp-id: as(<machine-word>, #x60040023);

  /* add an EventListener */
  function IElement/addEventListener (arg-type :: <string>, arg-listener :: 
        <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x60040024);

  /* remove an EventListener */
  function IElement/removeEventListener (arg-type :: <string>, arg-listener 
        :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "removeEventListener", disp-id: as(<machine-word>, #x60040025);

  /* dispatch an event */
  function IElement/dispatchEvent (arg-evt :: <IEvent>) => (arg-result :: 
        <boolean>), name: "dispatchEvent", disp-id: as(<machine-word>, 
        #x60040026);

  /* gets an attribute */
  function IElement/getAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <string>), name: 
        "getAttributeNS", disp-id: as(<machine-word>, #x60040027);

  /* sets an attribute */
  function IElement/setAttributeNS (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>, arg-value :: <string>) => (), name: 
        "setAttributeNS", disp-id: as(<machine-word>, #x60040028);

  /* remove an attribute */
  function IElement/removeAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (), name: "removeAttributeNS", 
        disp-id: as(<machine-word>, #x60040029);

  /* get an attribute node */
  function IElement/getAttributeNodeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMAttribute>), 
        name: "getAttributeNodeNS", disp-id: as(<machine-word>, 
        #x6004002A);

  /* set an attribute node */
  function IElement/setAttributeNodeNS (arg-newAttr :: <IDOMAttribute>) => 
        (arg-result :: <IDOMAttribute>), name: "setAttributeNodeNS", 
        disp-id: as(<machine-word>, #x6004002B);

  /* get elements by tag name */
  function IElement/getElementsByTagNameNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMNodeList>), name: 
        "getElementsByTagNameNS", disp-id: as(<machine-word>, #x6004002C);

  /* does element have the attribute? */
  function IElement/hasAttribute (arg-name :: <string>) => (arg-result :: 
        <boolean>), name: "hasAttribute", disp-id: as(<machine-word>, 
        #x6004002D);

  /* does element have the attribute? */
  function IElement/hasAttributeNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <boolean>), name: 
        "hasAttributeNS", disp-id: as(<machine-word>, #x6004002E);
end dispatch-client <IElement>;


/* Dispatch interface: IEvent version 0.0
 * GUID: {15842560-88E3-11D4-9060-00C04F78ACF9}
 * Description: IEvent Interface
 */
define dispatch-client <IEvent>
  uuid "{15842560-88E3-11D4-9060-00C04F78ACF9}";

  /* constant: capturing phase */
  constant property IEvent/CAPTURING-PHASE :: <integer>, name: 
        "CAPTURING_PHASE", disp-id: as(<machine-word>, #x60020000);

  /* constant: at target */
  constant property IEvent/AT-TARGET :: <integer>, name: "AT_TARGET", 
        disp-id: as(<machine-word>, #x60020001);

  /* constant: bubbling phase */
  constant property IEvent/BUBBLING-PHASE :: <integer>, name: 
        "BUBBLING_PHASE", disp-id: as(<machine-word>, #x60020002);

  /* event type */
  constant property IEvent/type :: <string>, name: "type", disp-id: 21;

  /* event target */
  constant property IEvent/target :: <IDOMNode>, name: "target", disp-id: 
        22;

  /* current node of event */
  constant property IEvent/currentNode :: <IDOMNode>, name: "currentNode", 
        disp-id: 23;

  /* current EventTarget of event */
  constant property IEvent/currentTarget :: <IDOMEventTarget>, name: 
        "currentTarget", disp-id: 20;

  /* event phase */
  constant property IEvent/eventPhase :: <integer>, name: "eventPhase", 
        disp-id: 24;

  /* event bubbles */
  constant property IEvent/bubbles :: <boolean>, name: "bubbles", disp-id: 
        25;

  /* event is cancelable */
  constant property IEvent/cancelable :: <boolean>, name: "cancelable", 
        disp-id: 26;

  /* timestamp */
  constant property IEvent/timeStamp :: <object>, name: "timeStamp", 
        disp-id: 27;

  /* get the type */
  function IEvent/getType () => (arg-result :: <string>), name: "getType", 
        disp-id: as(<machine-word>, #x6002000B);

  /* get the target */
  function IEvent/getTarget () => (arg-result :: <IDOMNode>), name: 
        "getTarget", disp-id: as(<machine-word>, #x6002000C);

  /* get the current node */
  function IEvent/getCurrentNode () => (arg-result :: <IDOMNode>), name: 
        "getCurrentNode", disp-id: as(<machine-word>, #x6002000D);

  /* get the current EventTarget */
  function IEvent/getCurrentTarget () => (arg-result :: <IDOMEventTarget>), 
        name: "getCurrentTarget", disp-id: as(<machine-word>, #x6002000E);

  /* get the event phase */
  function IEvent/getEventPhase () => (arg-result :: <integer>), name: 
        "getEventPhase", disp-id: as(<machine-word>, #x6002000F);

  /* does the event bubble? */
  function IEvent/getBubbles () => (arg-result :: <boolean>), name: 
        "getBubbles", disp-id: as(<machine-word>, #x60020010);

  /* is the event cancelable? */
  function IEvent/getCancelable () => (arg-result :: <boolean>), name: 
        "getCancelable", disp-id: as(<machine-word>, #x60020011);

  /* prevents further propagation of event */
  function IEvent/stopPropagation () => (), name: "stopPropagation", 
        disp-id: as(<machine-word>, #x60020012);

  /* prevents default action for event */
  function IEvent/preventDefault () => (), name: "preventDefault", disp-id: 
        as(<machine-word>, #x60020013);

  /* initializes an event */
  function IEvent/initEvent (arg-eventTypeArg :: <string>, arg-canBubbleArg 
        :: <boolean>, arg-cancelableArg :: <boolean>) => (), name: 
        "initEvent", disp-id: as(<machine-word>, #x60020014);

  /* get the timestamp */
  function IEvent/getTimeStamp () => (arg-result :: <object>), name: 
        "getTimeStamp", disp-id: as(<machine-word>, #x60020015);
end dispatch-client <IEvent>;


/* Dispatch interface: IDOMEventTarget version 0.0
 * GUID: {F7805760-8851-11D4-9060-00C04F78ACF9}
 * Description: IDOMEventTarget Interface
 */
define dispatch-client <IDOMEventTarget>
  uuid "{F7805760-8851-11D4-9060-00C04F78ACF9}";

  /* add an EventListener */
  function IDOMEventTarget/addEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x60010000);

  /* remove an EventListener */
  function IDOMEventTarget/removeEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "removeEventListener", disp-id: as(<machine-word>, #x60010001);

  /* dispatch an event */
  function IDOMEventTarget/dispatchEvent (arg-evt :: <IEvent>) => 
        (arg-result :: <boolean>), name: "dispatchEvent", disp-id: 
        as(<machine-word>, #x60010002);
end dispatch-client <IDOMEventTarget>;


/* Dispatch interface: ICSSStyleDeclaration version 0.0
 * GUID: {AA6F6570-0398-11D5-9078-00C04F78ACF9}
 * Description: ICSSStyleDeclaration Interface
 */
define dispatch-client <ICSSStyleDeclaration>
  uuid "{AA6F6570-0398-11D5-9078-00C04F78ACF9}";

  /* method setProperty */
  function ICSSStyleDeclaration/setPropertyDeprecated (arg-propertyName :: 
        <string>, arg-newVal :: <string>) => (), name: 
        "setPropertyDeprecated", disp-id: 140;
end dispatch-client <ICSSStyleDeclaration>;


/* Dispatch interface: ISVGPoint version 0.0
 * GUID: {1746CD70-98AE-11D4-9064-00C04F78ACF9}
 * Description: ISVGPoint Interface
 */
define dispatch-client <ISVGPoint>
  uuid "{1746CD70-98AE-11D4-9064-00C04F78ACF9}";

  /* property x */
  property ISVGPoint/x :: <single-float>, name: "x", disp-id: 210;

  /* property y */
  property ISVGPoint/y :: <single-float>, name: "y", disp-id: 211;

  /* method getX */
  function ISVGPoint/getX () => (arg-result :: <single-float>), name: 
        "getX", disp-id: as(<machine-word>, #x60020004);

  /* method setX */
  function ISVGPoint/setX (arg-x :: <single-float>) => (), name: "setX", 
        disp-id: as(<machine-word>, #x60020005);

  /* method getY */
  function ISVGPoint/getY () => (arg-result :: <single-float>), name: 
        "getY", disp-id: as(<machine-word>, #x60020006);

  /* method setY */
  function ISVGPoint/setY (arg-y :: <single-float>) => (), name: "setY", 
        disp-id: as(<machine-word>, #x60020007);

  /* method matrixTransform */
  function ISVGPoint/matrixTransform (arg-pMatrix :: <ISVGMatrix>) => 
        (arg-result :: <ISVGPoint>), name: "matrixTransform", disp-id: 
        as(<machine-word>, #x60020008);
end dispatch-client <ISVGPoint>;


/* Dispatch interface: ISVGMatrix version 0.0
 * GUID: {05BD5090-98AE-11D4-9064-00C04F78ACF9}
 * Description: ISVGMatrix Interface
 */
define dispatch-client <ISVGMatrix>
  uuid "{05BD5090-98AE-11D4-9064-00C04F78ACF9}";
end dispatch-client <ISVGMatrix>;


/* COM class: DOMImplementation version 0.0
 * GUID: {256FDDA0-94D1-11D4-9064-00C04F78ACF9}
 * Description: DOMImplementation Class
 */
define constant $DOMImplementation-class-id = as(<REFCLSID>, 
        "{256FDDA0-94D1-11D4-9064-00C04F78ACF9}");

define function make-DOMImplementation () => (default-interface :: 
        <IImplementation>, interface-2 :: <IDOMImplementation>, interface-3 
        :: <IInternalRef>)
  let default-interface = make(<IImplementation>, class-id: 
        $DOMImplementation-class-id);
  values(default-interface,
         make(<IDOMImplementation>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-DOMImplementation;


/* Dispatch interface: IImplementation version 0.0
 * GUID: {27D1FEA0-F7D4-11D4-9075-00C04F78ACF9}
 * Description: IImplementation Interface
 */
define dispatch-client <IImplementation>
  uuid "{27D1FEA0-F7D4-11D4-9075-00C04F78ACF9}";

  function IImplementation/hasFeature (arg-feature :: <string>, arg-version 
        :: <string>) => (arg-result :: <boolean>), name: "hasFeature", 
        disp-id: 450;

  /* creates a DocumentType object */
  function IImplementation/createDocumentType (arg-qualifiedName :: 
        <string>, arg-publicId :: <string>, arg-systemId :: <string>) => 
        (arg-result :: <IDOMDocumentType>), name: "createDocumentType", 
        disp-id: as(<machine-word>, #x60030000);

  /* creates a Document object */
  function IImplementation/createDocument (arg-namespaceURI :: <object>, 
        arg-qualifiedName :: <string>, arg-doctype :: <IDOMDocumentType>) 
        => (arg-result :: <IDOMDocument>), name: "createDocument", disp-id: 
        as(<machine-word>, #x60030001);
end dispatch-client <IImplementation>;


/* COM class: DocumentFragment version 0.0
 * GUID: {4AF6E170-94D1-11D4-9064-00C04F78ACF9}
 * Description: DocumentFragment Class
 */
define constant $DocumentFragment-class-id = as(<REFCLSID>, 
        "{4AF6E170-94D1-11D4-9064-00C04F78ACF9}");

define function make-DocumentFragment () => (default-interface :: 
        <IDocumentFragment>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMDocumentFragment>, interface-4 :: <IInternalRef>, interface-5 
        :: <IContainer>)
  let default-interface = make(<IDocumentFragment>, class-id: 
        $DocumentFragment-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMDocumentFragment>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-DocumentFragment;


/* Dispatch interface: IDocumentFragment version 0.0
 * GUID: {CE619A10-B43E-11D4-906B-00C04F78ACF9}
 * Description: IDocumentFragment Interface
 */
define dispatch-client <IDocumentFragment>
  uuid "{CE619A10-B43E-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IDocumentFragment/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IDocumentFragment/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IDocumentFragment/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDocumentFragment/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDocumentFragment/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDocumentFragment/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDocumentFragment/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDocumentFragment/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDocumentFragment/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDocumentFragment/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IDocumentFragment/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDocumentFragment/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDocumentFragment/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDocumentFragment/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDocumentFragment/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDocumentFragment/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IDocumentFragment/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* name of the node */
  function IDocumentFragment/getNodeName () => (arg-result :: <string>), 
        name: "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IDocumentFragment/getNodeValue () => (arg-result :: <object>), 
        name: "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IDocumentFragment/setNodeValue (arg-value :: <object>) => (), 
        name: "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IDocumentFragment/getNodeType () => (arg-result :: 
        <DOMNodeType>), name: "getNodeType", disp-id: as(<machine-word>, 
        #x60040003);

  /* parent of the node */
  function IDocumentFragment/getParentNode () => (arg-result :: 
        <IDOMNode>), name: "getParentNode", disp-id: as(<machine-word>, 
        #x60040004);

  /* the collection of the node's children */
  function IDocumentFragment/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function IDocumentFragment/getFirstChild () => (arg-result :: 
        <IDOMNode>), name: "getFirstChild", disp-id: as(<machine-word>, 
        #x60040006);

  /* last child of the node */
  function IDocumentFragment/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IDocumentFragment/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function IDocumentFragment/getNextSibling () => (arg-result :: 
        <IDOMNode>), name: "getNextSibling", disp-id: as(<machine-word>, 
        #x60040009);

  /* the collection of the node's attributes */
  function IDocumentFragment/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IDocumentFragment/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IDocumentFragment/normalize () => (), name: "normalize", 
        disp-id: as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IDocumentFragment/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IDocumentFragment/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IDocumentFragment/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IDocumentFragment/getNamespaceURI () => (arg-result :: 
        <string>), name: "getNamespaceURI", disp-id: as(<machine-word>, 
        #x60040010);

  /* value of the prefix */
  property IDocumentFragment/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IDocumentFragment/getPrefix () => (arg-result :: <string>), 
        name: "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IDocumentFragment/setPrefix (arg-prefix :: <string>) => (), 
        name: "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IDocumentFragment/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IDocumentFragment/getLocalName () => (arg-result :: <string>), 
        name: "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IDocumentFragment/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property IDocumentFragment/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IDocumentFragment/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IDocumentFragment/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IDocumentFragment/CDATA-SECTION-NODE :: <integer>, 
        name: "CDATA_SECTION_NODE", disp-id: as(<machine-word>, 
        #x6004001B);

  /* constant: EntityReference node */
  constant property IDocumentFragment/ENTITY-REFERENCE-NODE :: <integer>, 
        name: "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, 
        #x6004001C);

  /* constant: Entity node */
  constant property IDocumentFragment/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IDocumentFragment/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001E);

  /* constant: Comment node */
  constant property IDocumentFragment/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IDocumentFragment/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IDocumentFragment/DOCUMENT-TYPE-NODE :: <integer>, 
        name: "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, 
        #x60040021);

  /* constant: DocumentFragment node */
  constant property IDocumentFragment/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60040022);

  /* constant: Notation node */
  constant property IDocumentFragment/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);
end dispatch-client <IDocumentFragment>;


/* COM class: document version 0.0
 * GUID: {63CBAD70-94D1-11D4-9064-00C04F78ACF9}
 * Description: Document Class
 */
define constant $document-class-id = as(<REFCLSID>, 
        "{63CBAD70-94D1-11D4-9064-00C04F78ACF9}");

define function make-document () => (default-interface :: <IDocument>, 
        interface-2 :: <IDOMNode>, interface-3 :: <IDOMDocument>, 
        interface-4 :: <IInternalRef>, interface-5 :: <IContainer>)
  let default-interface = make(<IDocument>, class-id: $document-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMDocument>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-document;


/* COM class: Node version 0.0
 * GUID: {0820D410-996B-11D4-9064-00C04F78ACF9}
 * Description: Node Class
 */
define constant $Node-class-id = as(<REFCLSID>, 
        "{0820D410-996B-11D4-9064-00C04F78ACF9}");

define function make-Node () => (default-interface :: <INode>, interface-2 
        :: <IDOMEventTarget>, interface-3 :: <IDOMNode>, interface-4 :: 
        <IInternalRef>)
  let default-interface = make(<INode>, class-id: $Node-class-id);
  values(default-interface,
         make(<IDOMEventTarget>, disp-interface: default-interface),
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-Node;


/* Dispatch interface: INode version 0.0
 * GUID: {426FFCA0-B43F-11D4-906B-00C04F78ACF9}
 * Description: INode Interface
 */
define dispatch-client <INode>
  uuid "{426FFCA0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property INode/nodeName :: <string>, name: "nodeName", disp-id: 
        2;

  /* value stored in the node */
  property INode/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property INode/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property INode/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property INode/childNodes :: <IDOMNodeList>, name: "childNodes", 
        disp-id: 7;

  /* first child of the node */
  constant property INode/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property INode/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property INode/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property INode/nextSibling :: <IDOMNode>, name: "nextSibling", 
        disp-id: 11;

  /* the collection of the node's attributes */
  constant property INode/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function INode/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild :: 
        <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function INode/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild :: 
        <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function INode/removeChild (arg-childNode :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function INode/appendChild (arg-newChild :: <IDOMNode>) => (arg-result :: 
        <IDOMNode>), name: "appendChild", disp-id: 16;

  function INode/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property INode/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function INode/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* name of the node */
  function INode/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60030000);

  /* value stored in the node */
  function INode/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60030001);

  /* value stored in the node */
  function INode/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60030002);

  /* the node's type */
  function INode/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60030003);

  /* parent of the node */
  function INode/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60030004);

  /* the collection of the node's children */
  function INode/getChildNodes () => (arg-result :: <IDOMNodeList>), name: 
        "getChildNodes", disp-id: as(<machine-word>, #x60030005);

  /* first child of the node */
  function INode/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60030006);

  /* last child of the node */
  function INode/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60030007);

  /* left sibling of the node */
  function INode/getPreviousSibling () => (arg-result :: <IDOMNode>), name: 
        "getPreviousSibling", disp-id: as(<machine-word>, #x60030008);

  /* right sibling of the node */
  function INode/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60030009);

  /* the collection of the node's attributes */
  function INode/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6003000A);

  /* document that contains the node */
  function INode/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6003000B);

  /* add an EventListener */
  function INode/addEventListener (arg-type :: <string>, arg-listener :: 
        <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x6003000C);

  /* remove an EventListener */
  function INode/removeEventListener (arg-type :: <string>, arg-listener :: 
        <object>, arg-useCapture :: <boolean>) => (), name: 
        "removeEventListener", disp-id: as(<machine-word>, #x6003000D);

  /* dispatch an event */
  function INode/dispatchEvent (arg-evt :: <IEvent>) => (arg-result :: 
        <boolean>), name: "dispatchEvent", disp-id: as(<machine-word>, 
        #x6003000E);

  /* normalize Text nodes */
  function INode/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6003000F);

  /* queries support of given feature */
  function INode/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x60030010);

  /* queries support of given feature */
  function INode/isSupported (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x60030011);

  /* value of the namespaceURI */
  constant property INode/namespaceURI :: <string>, name: "namespaceURI", 
        disp-id: as(<machine-word>, #x60030012);

  /* gets URI of node's namespace */
  function INode/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60030013);

  /* value of the prefix */
  property INode/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60030014);

  /* gets node's prefix */
  function INode/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60030016);

  /* sets node's prefix */
  function INode/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60030017);

  /* value of the localName */
  constant property INode/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60030018);

  /* gets local name of the node */
  function INode/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60030019);

  /* queries if node has attributes */
  function INode/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x6003001A);

  /* constant: Element node */
  constant property INode/ELEMENT-NODE :: <integer>, name: "ELEMENT_NODE", 
        disp-id: as(<machine-word>, #x6003001B);

  /* constant: Attribute node */
  constant property INode/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x6003001C);

  /* constant: Text node */
  constant property INode/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6003001D);

  /* constant: CDATASection node */
  constant property INode/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6003001E);

  /* constant: EntityReference node */
  constant property INode/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6003001F);

  /* constant: Entity node */
  constant property INode/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x60030020);

  /* constant: ProcessingInstruction node */
  constant property INode/PROCESSING-INSTRUCTION-NODE :: <integer>, name: 
        "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x60030021);

  /* constant: Comment node */
  constant property INode/COMMENT-NODE :: <integer>, name: "COMMENT_NODE", 
        disp-id: as(<machine-word>, #x60030022);

  /* constant: Document node */
  constant property INode/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60030023);

  /* constant: DocumentType node */
  constant property INode/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60030024);

  /* constant: DocumentFragment node */
  constant property INode/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60030025);

  /* constant: Notation node */
  constant property INode/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60030026);
end dispatch-client <INode>;


/* COM class: NodeList version 0.0
 * GUID: {C8D0D6B0-94D1-11D4-9064-00C04F78ACF9}
 * Description: NodeList Class
 */
define constant $NodeList-class-id = as(<REFCLSID>, 
        "{C8D0D6B0-94D1-11D4-9064-00C04F78ACF9}");

define function make-NodeList () => (default-interface :: <INodeList>, 
        interface-2 :: <IDOMNodeList>, interface-3 :: <IInternalRef>)
  let default-interface = make(<INodeList>, class-id: $NodeList-class-id);
  values(default-interface,
         make(<IDOMNodeList>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-NodeList;


/* Dispatch interface: INodeList version 0.0
 * GUID: {BFF27190-1D48-11D4-904D-00C04F78ACF9}
 * Description: INodeList Interface
 */
define dispatch-client <INodeList>
  uuid "{BFF27190-1D48-11D4-904D-00C04F78ACF9}";

  /* collection of nodes */
  element constant property INodeList/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IDOMNode>, name: "item", 
        disp-id: 0;

  /* number of nodes in the collection */
  constant property INodeList/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 387;

  /* number of nodes in the collection */
  function INodeList/getLength () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getLength", disp-id: as(<machine-word>, 
        #x60030000);
end dispatch-client <INodeList>;


/* COM class: NamedNodeMap version 0.0
 * GUID: {E6213880-94D1-11D4-9064-00C04F78ACF9}
 * Description: NamedNodeMap Class
 */
define constant $NamedNodeMap-class-id = as(<REFCLSID>, 
        "{E6213880-94D1-11D4-9064-00C04F78ACF9}");

define function make-NamedNodeMap () => (default-interface :: 
        <INamedNodeMap>, interface-2 :: <IDOMNamedNodeMap>, interface-3 :: 
        <IInternalRef>)
  let default-interface = make(<INamedNodeMap>, class-id: 
        $NamedNodeMap-class-id);
  values(default-interface,
         make(<IDOMNamedNodeMap>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-NamedNodeMap;


/* Dispatch interface: INamedNodeMap version 0.0
 * GUID: {AD645CF0-F7DC-11D4-9076-00C04F78ACF9}
 * Description: INamedNodeMap Interface
 */
define dispatch-client <INamedNodeMap>
  uuid "{AD645CF0-F7DC-11D4-9076-00C04F78ACF9}";

  /* lookup item by name */
  function INamedNodeMap/getNamedItem (arg-name :: <string>) => (arg-result 
        :: <IDOMNode>), name: "getNamedItem", disp-id: 420;

  /* set item by name */
  function INamedNodeMap/setNamedItem (arg-newItem :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "setNamedItem", disp-id: 421;

  /* remove item by name */
  function INamedNodeMap/removeNamedItem (arg-name :: <string>) => 
        (arg-result :: <IDOMNode>), name: "removeNamedItem", disp-id: 422;

  /* collection of nodes */
  element constant property INamedNodeMap/item (arg-index :: 
        type-union(<integer>, <machine-word>)) :: <IDOMNode>, name: "item", 
        disp-id: 0;

  /* number of nodes in the collection */
  constant property INamedNodeMap/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 387;

  /* number of nodes in the collection */
  function INamedNodeMap/getLength () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getLength", disp-id: 
        as(<machine-word>, #x60030000);

  /* gets named item */
  function INamedNodeMap/getNamedItemNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMNode>), name: 
        "getNamedItemNS", disp-id: as(<machine-word>, #x60030001);

  /* sets named item */
  function INamedNodeMap/setNamedItemNS (arg-arg :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "setNamedItemNS", disp-id: 
        as(<machine-word>, #x60030002);

  /* removes named item */
  function INamedNodeMap/removeNamedItemNS (arg-namespaceURI :: <object>, 
        arg-localName :: <string>) => (arg-result :: <IDOMNode>), name: 
        "removeNamedItemNS", disp-id: as(<machine-word>, #x60030003);
end dispatch-client <INamedNodeMap>;


/* COM class: Attr version 0.0
 * GUID: {013C36F0-94D2-11D4-9064-00C04F78ACF9}
 * Description: Attr Class
 */
define constant $Attr-class-id = as(<REFCLSID>, 
        "{013C36F0-94D2-11D4-9064-00C04F78ACF9}");

define function make-Attr () => (default-interface :: <IAttr>, interface-2 
        :: <IDOMNode>, interface-3 :: <IDOMAttribute>, interface-4 :: 
        <IInternalRef>, interface-5 :: <IContainer>)
  let default-interface = make(<IAttr>, class-id: $Attr-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMAttribute>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-Attr;


/* Dispatch interface: IAttr version 0.0
 * GUID: {51CEC870-B43F-11D4-906B-00C04F78ACF9}
 * Description: IAttr Interface
 */
define dispatch-client <IAttr>
  uuid "{51CEC870-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IAttr/nodeName :: <string>, name: "nodeName", disp-id: 
        2;

  /* value stored in the node */
  property IAttr/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IAttr/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IAttr/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IAttr/childNodes :: <IDOMNodeList>, name: "childNodes", 
        disp-id: 7;

  /* first child of the node */
  constant property IAttr/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IAttr/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IAttr/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IAttr/nextSibling :: <IDOMNode>, name: "nextSibling", 
        disp-id: 11;

  /* the collection of the node's attributes */
  constant property IAttr/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IAttr/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild :: 
        <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IAttr/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild :: 
        <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IAttr/removeChild (arg-childNode :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IAttr/appendChild (arg-newChild :: <IDOMNode>) => (arg-result :: 
        <IDOMNode>), name: "appendChild", disp-id: 16;

  function IAttr/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IAttr/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IAttr/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get name of the attribute */
  constant property IAttr/name :: <string>, name: "name", disp-id: 162;

  /* indicates whether attribute has a default value */
  constant property IAttr/specified :: <boolean>, name: "specified", 
        disp-id: 163;

  /* string value of the attribute */
  property IAttr/value :: <object>, name: "value", disp-id: 0;

  /* name of the node */
  function IAttr/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IAttr/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IAttr/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IAttr/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function IAttr/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IAttr/getChildNodes () => (arg-result :: <IDOMNodeList>), name: 
        "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function IAttr/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IAttr/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IAttr/getPreviousSibling () => (arg-result :: <IDOMNode>), name: 
        "getPreviousSibling", disp-id: as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function IAttr/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function IAttr/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IAttr/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IAttr/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IAttr/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IAttr/isSupported (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IAttr/namespaceURI :: <string>, name: "namespaceURI", 
        disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IAttr/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property IAttr/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IAttr/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IAttr/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IAttr/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IAttr/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IAttr/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property IAttr/ELEMENT-NODE :: <integer>, name: "ELEMENT_NODE", 
        disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IAttr/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IAttr/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IAttr/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property IAttr/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: Entity node */
  constant property IAttr/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IAttr/PROCESSING-INSTRUCTION-NODE :: <integer>, name: 
        "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001E);

  /* constant: Comment node */
  constant property IAttr/COMMENT-NODE :: <integer>, name: "COMMENT_NODE", 
        disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IAttr/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IAttr/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property IAttr/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040022);

  /* constant: Notation node */
  constant property IAttr/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* get name of the attribute */
  function IAttr/getName () => (arg-result :: <string>), name: "getName", 
        disp-id: as(<machine-word>, #x60040024);

  /* indicates whether attribute has a default value */
  function IAttr/getSpecified () => (arg-result :: <boolean>), name: 
        "getSpecified", disp-id: as(<machine-word>, #x60040025);

  /* string value of the attribute */
  function IAttr/getValue () => (arg-result :: <object>), name: "getValue", 
        disp-id: as(<machine-word>, #x60040026);

  /* string value of the attribute */
  function IAttr/setValue (arg-attributeValue :: <object>) => (), name: 
        "setValue", disp-id: as(<machine-word>, #x60040027);

  /* element that owns the attribute */
  constant property IAttr/ownerElement :: <IDOMElement>, name: 
        "ownerElement", disp-id: 300;

  /* element that owns the attribute */
  function IAttr/getOwnerElement () => (arg-result :: <IDOMElement>), name: 
        "getOwnerElement", disp-id: as(<machine-word>, #x60040029);
end dispatch-client <IAttr>;


/* COM class: element version 0.0
 * GUID: {25D61040-996B-11D4-9064-00C04F78ACF9}
 * Description: Element Class
 */
define constant $element-class-id = as(<REFCLSID>, 
        "{25D61040-996B-11D4-9064-00C04F78ACF9}");

define function make-element () => (default-interface :: <IElement>, 
        interface-2 :: <IDOMEventTarget>, interface-3 :: <IDOMNode>, 
        interface-4 :: <IDOMElement>, interface-5 :: <IInternalRef>, 
        interface-6 :: <IContainer>)
  let default-interface = make(<IElement>, class-id: $element-class-id);
  values(default-interface,
         make(<IDOMEventTarget>, disp-interface: default-interface),
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMElement>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-element;


/* COM class: CharacterData version 0.0
 * GUID: {3759C0D0-94D2-11D4-9064-00C04F78ACF9}
 * Description: CharacterData Class
 */
define constant $CharacterData-class-id = as(<REFCLSID>, 
        "{3759C0D0-94D2-11D4-9064-00C04F78ACF9}");

define function make-CharacterData () => (default-interface :: 
        <ICharacterData>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMCharacterData>, interface-4 :: <IInternalRef>, interface-5 :: 
        <IContainer>)
  let default-interface = make(<ICharacterData>, class-id: 
        $CharacterData-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMCharacterData>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-CharacterData;


/* Dispatch interface: ICharacterData version 0.0
 * GUID: {788769C0-B43F-11D4-906B-00C04F78ACF9}
 * Description: ICharacterData Interface
 */
define dispatch-client <ICharacterData>
  uuid "{788769C0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property ICharacterData/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property ICharacterData/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property ICharacterData/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ICharacterData/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ICharacterData/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ICharacterData/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ICharacterData/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property ICharacterData/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ICharacterData/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ICharacterData/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function ICharacterData/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ICharacterData/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ICharacterData/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ICharacterData/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ICharacterData/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ICharacterData/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function ICharacterData/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property ICharacterData/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property ICharacterData/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function ICharacterData/substringData (arg-offset :: 
        type-union(<integer>, <machine-word>), arg-count :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: <string>), 
        name: "substringData", disp-id: 131;

  /* append string to value */
  function ICharacterData/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function ICharacterData/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function ICharacterData/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function ICharacterData/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;

  /* name of the node */
  function ICharacterData/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function ICharacterData/getNodeValue () => (arg-result :: <object>), 
        name: "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function ICharacterData/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function ICharacterData/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function ICharacterData/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function ICharacterData/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function ICharacterData/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function ICharacterData/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function ICharacterData/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function ICharacterData/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function ICharacterData/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function ICharacterData/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function ICharacterData/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function ICharacterData/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function ICharacterData/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property ICharacterData/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function ICharacterData/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property ICharacterData/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function ICharacterData/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function ICharacterData/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property ICharacterData/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function ICharacterData/getLocalName () => (arg-result :: <string>), 
        name: "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function ICharacterData/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property ICharacterData/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property ICharacterData/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property ICharacterData/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property ICharacterData/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property ICharacterData/ENTITY-REFERENCE-NODE :: <integer>, 
        name: "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, 
        #x6004001C);

  /* constant: Entity node */
  constant property ICharacterData/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property ICharacterData/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001E);

  /* constant: Comment node */
  constant property ICharacterData/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property ICharacterData/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property ICharacterData/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property ICharacterData/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60040022);

  /* constant: Notation node */
  constant property ICharacterData/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* get value of the node */
  function ICharacterData/getData () => (arg-result :: <string>), name: 
        "getData", disp-id: as(<machine-word>, #x60040024);

  /* set value of the node */
  function ICharacterData/setData (arg-data :: <string>) => (), name: 
        "setData", disp-id: as(<machine-word>, #x60040025);

  /* number of characters in value */
  function ICharacterData/getLength () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getLength", disp-id: 
        as(<machine-word>, #x60040026);
end dispatch-client <ICharacterData>;


/* COM class: text version 0.0
 * GUID: {4CDB6960-94D2-11D4-9064-00C04F78ACF9}
 * Description: Text Class
 */
define constant $text-class-id = as(<REFCLSID>, 
        "{4CDB6960-94D2-11D4-9064-00C04F78ACF9}");

define function make-text () => (default-interface :: <IText>, interface-2 
        :: <IDOMNode>, interface-3 :: <IDOMCharacterData>, interface-4 :: 
        <IDOMText>, interface-5 :: <IInternalRef>, interface-6 :: 
        <IContainer>)
  let default-interface = make(<IText>, class-id: $text-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMCharacterData>, disp-interface: default-interface),
         make(<IDOMText>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-text;


/* Dispatch interface: IText version 0.0
 * GUID: {87BB6C50-B43F-11D4-906B-00C04F78ACF9}
 * Description: IText Interface
 */
define dispatch-client <IText>
  uuid "{87BB6C50-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IText/nodeName :: <string>, name: "nodeName", disp-id: 
        2;

  /* value stored in the node */
  property IText/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IText/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IText/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IText/childNodes :: <IDOMNodeList>, name: "childNodes", 
        disp-id: 7;

  /* first child of the node */
  constant property IText/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IText/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IText/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IText/nextSibling :: <IDOMNode>, name: "nextSibling", 
        disp-id: 11;

  /* the collection of the node's attributes */
  constant property IText/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IText/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild :: 
        <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IText/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild :: 
        <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IText/removeChild (arg-childNode :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IText/appendChild (arg-newChild :: <IDOMNode>) => (arg-result :: 
        <IDOMNode>), name: "appendChild", disp-id: 16;

  function IText/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IText/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IText/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IText/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IText/length :: type-union(<integer>, <machine-word>), 
        name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IText/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 131;

  /* append string to value */
  function IText/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IText/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function IText/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function IText/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;

  /* split the text node into two text nodes at the position specified */
  function IText/splitText (arg-offset :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IDOMText>), name: "splitText", 
        disp-id: 194;

  /* name of the node */
  function IText/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60050000);

  /* value stored in the node */
  function IText/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60050001);

  /* value stored in the node */
  function IText/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60050002);

  /* the node's type */
  function IText/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60050003);

  /* parent of the node */
  function IText/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60050004);

  /* the collection of the node's children */
  function IText/getChildNodes () => (arg-result :: <IDOMNodeList>), name: 
        "getChildNodes", disp-id: as(<machine-word>, #x60050005);

  /* first child of the node */
  function IText/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60050006);

  /* last child of the node */
  function IText/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60050007);

  /* left sibling of the node */
  function IText/getPreviousSibling () => (arg-result :: <IDOMNode>), name: 
        "getPreviousSibling", disp-id: as(<machine-word>, #x60050008);

  /* right sibling of the node */
  function IText/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60050009);

  /* the collection of the node's attributes */
  function IText/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6005000A);

  /* document that contains the node */
  function IText/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6005000B);

  /* normalize Text nodes */
  function IText/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6005000C);

  /* queries support of given feature */
  function IText/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x6005000D);

  /* queries support of given feature */
  function IText/isSupported (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x6005000E);

  /* value of the namespaceURI */
  constant property IText/namespaceURI :: <string>, name: "namespaceURI", 
        disp-id: as(<machine-word>, #x6005000F);

  /* gets URI of node's namespace */
  function IText/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60050010);

  /* value of the prefix */
  property IText/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60050011);

  /* gets node's prefix */
  function IText/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60050013);

  /* sets node's prefix */
  function IText/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60050014);

  /* value of the localName */
  constant property IText/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60050015);

  /* gets local name of the node */
  function IText/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60050016);

  /* queries if node has attributes */
  function IText/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60050017);

  /* constant: Element node */
  constant property IText/ELEMENT-NODE :: <integer>, name: "ELEMENT_NODE", 
        disp-id: as(<machine-word>, #x60050018);

  /* constant: Attribute node */
  constant property IText/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60050019);

  /* constant: Text node */
  constant property IText/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6005001A);

  /* constant: CDATASection node */
  constant property IText/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6005001B);

  /* constant: EntityReference node */
  constant property IText/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6005001C);

  /* constant: Entity node */
  constant property IText/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x6005001D);

  /* constant: ProcessingInstruction node */
  constant property IText/PROCESSING-INSTRUCTION-NODE :: <integer>, name: 
        "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6005001E);

  /* constant: Comment node */
  constant property IText/COMMENT-NODE :: <integer>, name: "COMMENT_NODE", 
        disp-id: as(<machine-word>, #x6005001F);

  /* constant: Document node */
  constant property IText/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60050020);

  /* constant: DocumentType node */
  constant property IText/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60050021);

  /* constant: DocumentFragment node */
  constant property IText/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60050022);

  /* constant: Notation node */
  constant property IText/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60050023);

  /* get value of the node */
  function IText/getData () => (arg-result :: <string>), name: "getData", 
        disp-id: as(<machine-word>, #x60050024);

  /* set value of the node */
  function IText/setData (arg-data :: <string>) => (), name: "setData", 
        disp-id: as(<machine-word>, #x60050025);

  /* number of characters in value */
  function IText/getLength () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getLength", disp-id: as(<machine-word>, 
        #x60050026);
end dispatch-client <IText>;


/* COM class: comment version 0.0
 * GUID: {648DDFC0-94D2-11D4-9064-00C04F78ACF9}
 * Description: Comment Class
 */
define constant $comment-class-id = as(<REFCLSID>, 
        "{648DDFC0-94D2-11D4-9064-00C04F78ACF9}");

define function make-comment () => (default-interface :: <IComment>, 
        interface-2 :: <IDOMNode>, interface-3 :: <IDOMCharacterData>, 
        interface-4 :: <IDOMComment>, interface-5 :: <IInternalRef>, 
        interface-6 :: <IContainer>)
  let default-interface = make(<IComment>, class-id: $comment-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMCharacterData>, disp-interface: default-interface),
         make(<IDOMComment>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-comment;


/* Dispatch interface: IComment version 0.0
 * GUID: {9462F1D0-B43F-11D4-906B-00C04F78ACF9}
 * Description: IComment Interface
 */
define dispatch-client <IComment>
  uuid "{9462F1D0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IComment/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IComment/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IComment/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IComment/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IComment/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IComment/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IComment/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IComment/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IComment/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IComment/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IComment/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IComment/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IComment/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IComment/appendChild (arg-newChild :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IComment/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IComment/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IComment/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property IComment/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property IComment/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function IComment/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 131;

  /* append string to value */
  function IComment/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function IComment/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function IComment/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function IComment/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;

  /* name of the node */
  function IComment/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60050000);

  /* value stored in the node */
  function IComment/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60050001);

  /* value stored in the node */
  function IComment/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60050002);

  /* the node's type */
  function IComment/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60050003);

  /* parent of the node */
  function IComment/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60050004);

  /* the collection of the node's children */
  function IComment/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60050005);

  /* first child of the node */
  function IComment/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60050006);

  /* last child of the node */
  function IComment/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60050007);

  /* left sibling of the node */
  function IComment/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60050008);

  /* right sibling of the node */
  function IComment/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60050009);

  /* the collection of the node's attributes */
  function IComment/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6005000A);

  /* document that contains the node */
  function IComment/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6005000B);

  /* normalize Text nodes */
  function IComment/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6005000C);

  /* queries support of given feature */
  function IComment/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x6005000D);

  /* queries support of given feature */
  function IComment/isSupported (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x6005000E);

  /* value of the namespaceURI */
  constant property IComment/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6005000F);

  /* gets URI of node's namespace */
  function IComment/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60050010);

  /* value of the prefix */
  property IComment/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60050011);

  /* gets node's prefix */
  function IComment/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60050013);

  /* sets node's prefix */
  function IComment/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60050014);

  /* value of the localName */
  constant property IComment/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60050015);

  /* gets local name of the node */
  function IComment/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60050016);

  /* queries if node has attributes */
  function IComment/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60050017);

  /* constant: Element node */
  constant property IComment/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60050018);

  /* constant: Attribute node */
  constant property IComment/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60050019);

  /* constant: Text node */
  constant property IComment/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6005001A);

  /* constant: CDATASection node */
  constant property IComment/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6005001B);

  /* constant: EntityReference node */
  constant property IComment/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6005001C);

  /* constant: Entity node */
  constant property IComment/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x6005001D);

  /* constant: ProcessingInstruction node */
  constant property IComment/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6005001E);

  /* constant: Comment node */
  constant property IComment/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6005001F);

  /* constant: Document node */
  constant property IComment/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60050020);

  /* constant: DocumentType node */
  constant property IComment/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60050021);

  /* constant: DocumentFragment node */
  constant property IComment/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60050022);

  /* constant: Notation node */
  constant property IComment/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60050023);

  /* get value of the node */
  function IComment/getData () => (arg-result :: <string>), name: 
        "getData", disp-id: as(<machine-word>, #x60050024);

  /* set value of the node */
  function IComment/setData (arg-data :: <string>) => (), name: "setData", 
        disp-id: as(<machine-word>, #x60050025);

  /* number of characters in value */
  function IComment/getLength () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getLength", disp-id: as(<machine-word>, 
        #x60050026);
end dispatch-client <IComment>;


/* COM class: CDATASection version 0.0
 * GUID: {7B103FD0-94D2-11D4-9064-00C04F78ACF9}
 * Description: CDATASection Class
 */
define constant $CDATASection-class-id = as(<REFCLSID>, 
        "{7B103FD0-94D2-11D4-9064-00C04F78ACF9}");

define function make-CDATASection () => (default-interface :: 
        <ICDATASection>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMCharacterData>, interface-4 :: <IDOMText>, interface-5 :: 
        <IDOMCDATASection>, interface-6 :: <IInternalRef>, interface-7 :: 
        <IContainer>)
  let default-interface = make(<ICDATASection>, class-id: 
        $CDATASection-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMCharacterData>, disp-interface: default-interface),
         make(<IDOMText>, disp-interface: default-interface),
         make(<IDOMCDATASection>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-CDATASection;


/* Dispatch interface: ICDATASection version 0.0
 * GUID: {A02E6C50-B43F-11D4-906B-00C04F78ACF9}
 * Description: ICDATASection Interface
 */
define dispatch-client <ICDATASection>
  uuid "{A02E6C50-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property ICDATASection/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property ICDATASection/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property ICDATASection/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ICDATASection/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ICDATASection/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ICDATASection/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ICDATASection/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property ICDATASection/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ICDATASection/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ICDATASection/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function ICDATASection/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ICDATASection/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ICDATASection/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ICDATASection/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ICDATASection/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ICDATASection/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function ICDATASection/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* value of the node */
  property ICDATASection/data :: <string>, name: "data", disp-id: 0;

  /* number of characters in value */
  constant property ICDATASection/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: 130;

  /* retrieve substring of value */
  function ICDATASection/substringData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: 
        "substringData", disp-id: 131;

  /* append string to value */
  function ICDATASection/appendData (arg-data :: <string>) => (), name: 
        "appendData", disp-id: 132;

  /* insert string into value */
  function ICDATASection/insertData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "insertData", 
        disp-id: 133;

  /* delete string within the value */
  function ICDATASection/deleteData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>)) => (), name: "deleteData", disp-id: 134;

  /* replace string within the value */
  function ICDATASection/replaceData (arg-offset :: type-union(<integer>, 
        <machine-word>), arg-count :: type-union(<integer>, 
        <machine-word>), arg-data :: <string>) => (), name: "replaceData", 
        disp-id: 135;

  /* split the text node into two text nodes at the position specified */
  function ICDATASection/splitText (arg-offset :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <IDOMText>), name: "splitText", 
        disp-id: 194;

  /* name of the node */
  function ICDATASection/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60060000);

  /* value stored in the node */
  function ICDATASection/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60060001);

  /* value stored in the node */
  function ICDATASection/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60060002);

  /* the node's type */
  function ICDATASection/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60060003);

  /* parent of the node */
  function ICDATASection/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60060004);

  /* the collection of the node's children */
  function ICDATASection/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60060005);

  /* first child of the node */
  function ICDATASection/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60060006);

  /* last child of the node */
  function ICDATASection/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60060007);

  /* left sibling of the node */
  function ICDATASection/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60060008);

  /* right sibling of the node */
  function ICDATASection/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60060009);

  /* the collection of the node's attributes */
  function ICDATASection/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6006000A);

  /* document that contains the node */
  function ICDATASection/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6006000B);

  /* normalize Text nodes */
  function ICDATASection/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6006000C);

  /* queries support of given feature */
  function ICDATASection/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6006000D);

  /* queries support of given feature */
  function ICDATASection/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6006000E);

  /* value of the namespaceURI */
  constant property ICDATASection/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6006000F);

  /* gets URI of node's namespace */
  function ICDATASection/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x60060010);

  /* value of the prefix */
  property ICDATASection/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60060011);

  /* gets node's prefix */
  function ICDATASection/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60060013);

  /* sets node's prefix */
  function ICDATASection/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60060014);

  /* value of the localName */
  constant property ICDATASection/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60060015);

  /* gets local name of the node */
  function ICDATASection/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60060016);

  /* queries if node has attributes */
  function ICDATASection/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60060017);

  /* constant: Element node */
  constant property ICDATASection/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60060018);

  /* constant: Attribute node */
  constant property ICDATASection/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60060019);

  /* constant: Text node */
  constant property ICDATASection/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6006001A);

  /* constant: CDATASection node */
  constant property ICDATASection/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6006001B);

  /* constant: EntityReference node */
  constant property ICDATASection/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6006001C);

  /* constant: Entity node */
  constant property ICDATASection/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6006001D);

  /* constant: ProcessingInstruction node */
  constant property ICDATASection/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6006001E);

  /* constant: Comment node */
  constant property ICDATASection/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6006001F);

  /* constant: Document node */
  constant property ICDATASection/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60060020);

  /* constant: DocumentType node */
  constant property ICDATASection/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60060021);

  /* constant: DocumentFragment node */
  constant property ICDATASection/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60060022);

  /* constant: Notation node */
  constant property ICDATASection/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60060023);

  /* get value of the node */
  function ICDATASection/getData () => (arg-result :: <string>), name: 
        "getData", disp-id: as(<machine-word>, #x60060024);

  /* set value of the node */
  function ICDATASection/setData (arg-data :: <string>) => (), name: 
        "setData", disp-id: as(<machine-word>, #x60060025);

  /* number of characters in value */
  function ICDATASection/getLength () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getLength", disp-id: 
        as(<machine-word>, #x60060026);
end dispatch-client <ICDATASection>;


/* COM class: documentType version 0.0
 * GUID: {979A1CC0-94D3-11D4-9064-00C04F78ACF9}
 * Description: DocumentType Class
 */
define constant $documentType-class-id = as(<REFCLSID>, 
        "{979A1CC0-94D3-11D4-9064-00C04F78ACF9}");

define function make-documentType () => (default-interface :: 
        <IDocumentType>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMDocumentType>, interface-4 :: <IInternalRef>, interface-5 :: 
        <IContainer>)
  let default-interface = make(<IDocumentType>, class-id: 
        $documentType-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMDocumentType>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-documentType;


/* Dispatch interface: IDocumentType version 0.0
 * GUID: {AEEE4CA0-B43F-11D4-906B-00C04F78ACF9}
 * Description: IDocumentType Interface
 */
define dispatch-client <IDocumentType>
  uuid "{AEEE4CA0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IDocumentType/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDocumentType/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDocumentType/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDocumentType/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDocumentType/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDocumentType/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDocumentType/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDocumentType/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDocumentType/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDocumentType/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDocumentType/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDocumentType/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDocumentType/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDocumentType/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDocumentType/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDocumentType/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDocumentType/cloneNode (arg-deep :: <boolean>) => (arg-result 
        :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* name of the document type (root of the tree) */
  constant property IDocumentType/name :: <string>, name: "name", disp-id: 
        258;

  /* a list of entities in the document */
  constant property IDocumentType/entities :: <IDOMNamedNodeMap>, name: 
        "entities", disp-id: 259;

  /* a list of notations in the document */
  constant property IDocumentType/notations :: <IDOMNamedNodeMap>, name: 
        "notations", disp-id: 260;

  /* name of the node */
  function IDocumentType/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IDocumentType/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IDocumentType/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IDocumentType/getNodeType () => (arg-result :: <DOMNodeType>), 
        name: "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function IDocumentType/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IDocumentType/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function IDocumentType/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IDocumentType/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IDocumentType/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function IDocumentType/getNextSibling () => (arg-result :: <IDOMNode>), 
        name: "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function IDocumentType/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IDocumentType/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IDocumentType/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IDocumentType/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IDocumentType/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IDocumentType/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IDocumentType/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property IDocumentType/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IDocumentType/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IDocumentType/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IDocumentType/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IDocumentType/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IDocumentType/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property IDocumentType/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IDocumentType/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IDocumentType/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IDocumentType/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property IDocumentType/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: Entity node */
  constant property IDocumentType/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IDocumentType/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001E);

  /* constant: Comment node */
  constant property IDocumentType/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IDocumentType/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IDocumentType/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property IDocumentType/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60040022);

  /* constant: Notation node */
  constant property IDocumentType/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* name of the document type (root of the tree) */
  function IDocumentType/getName () => (arg-result :: <string>), name: 
        "getName", disp-id: as(<machine-word>, #x60040024);

  /* a list of entities in the document */
  function IDocumentType/getEntities () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getEntities", disp-id: 
        as(<machine-word>, #x60040025);

  /* a list of notations in the document */
  function IDocumentType/getNotations () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getNotations", disp-id: 
        as(<machine-word>, #x60040026);

  /* publicId of this document's DOCTYPE */
  constant property IDocumentType/publicId :: <string>, name: "publicId", 
        disp-id: as(<machine-word>, #x60040027);

  /* publicId of this document's DOCTYPE */
  function IDocumentType/getPublicId () => (arg-result :: <string>), name: 
        "getPublicId", disp-id: as(<machine-word>, #x60040028);

  /* systemId of this document's DOCTYPE */
  constant property IDocumentType/systemId :: <string>, name: "systemId", 
        disp-id: as(<machine-word>, #x60040029);

  /* systemId of this document's DOCTYPE */
  function IDocumentType/getSystemId () => (arg-result :: <string>), name: 
        "getSystemId", disp-id: as(<machine-word>, #x6004002A);

  /* internal subset of this document's DOCTYPE */
  constant property IDocumentType/internalSubset :: <string>, name: 
        "internalSubset", disp-id: as(<machine-word>, #x6004002B);

  /* internal subset of this document's DOCTYPE */
  function IDocumentType/getInternalSubset () => (arg-result :: <string>), 
        name: "getInternalSubset", disp-id: as(<machine-word>, #x6004002C);
end dispatch-client <IDocumentType>;


/* COM class: Notation version 0.0
 * GUID: {BCB7B0A0-94D3-11D4-9064-00C04F78ACF9}
 * Description: Notation Class
 */
define constant $Notation-class-id = as(<REFCLSID>, 
        "{BCB7B0A0-94D3-11D4-9064-00C04F78ACF9}");

define function make-Notation () => (default-interface :: <INotation>, 
        interface-2 :: <IDOMNode>, interface-3 :: <IDOMNotation>, 
        interface-4 :: <IInternalRef>, interface-5 :: <IContainer>)
  let default-interface = make(<INotation>, class-id: $Notation-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMNotation>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-Notation;


/* Dispatch interface: IDOMNotation version 0.0
 * GUID: {3EFAA422-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMNotation>
  uuid "{3EFAA422-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMNotation/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMNotation/nodeValue :: <object>, name: "nodeValue", disp-id: 
        3;

  /* the node's type */
  constant property IDOMNotation/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IDOMNotation/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMNotation/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMNotation/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMNotation/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IDOMNotation/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMNotation/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMNotation/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMNotation/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMNotation/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMNotation/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMNotation/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMNotation/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMNotation/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMNotation/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the public ID */
  constant property IDOMNotation/publicId :: <object>, name: "publicId", 
        disp-id: 290;

  /* the system ID */
  constant property IDOMNotation/systemId :: <object>, name: "systemId", 
        disp-id: 291;
end dispatch-client <IDOMNotation>;


/* Dispatch interface: INotation version 0.0
 * GUID: {BC26FCF0-B43F-11D4-906B-00C04F78ACF9}
 * Description: INotation Interface
 */
define dispatch-client <INotation>
  uuid "{BC26FCF0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property INotation/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property INotation/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property INotation/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property INotation/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property INotation/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property INotation/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property INotation/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property INotation/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property INotation/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property INotation/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function INotation/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function INotation/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function INotation/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function INotation/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function INotation/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property INotation/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function INotation/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the public ID */
  constant property INotation/publicId :: <object>, name: "publicId", 
        disp-id: 290;

  /* the system ID */
  constant property INotation/systemId :: <object>, name: "systemId", 
        disp-id: 291;

  /* name of the node */
  function INotation/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function INotation/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function INotation/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function INotation/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function INotation/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function INotation/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function INotation/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function INotation/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function INotation/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60040008);

  /* right sibling of the node */
  function INotation/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function INotation/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function INotation/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function INotation/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function INotation/supports (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "supports", 
        disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function INotation/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property INotation/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function INotation/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property INotation/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function INotation/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function INotation/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property INotation/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function INotation/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function INotation/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property INotation/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property INotation/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property INotation/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property INotation/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property INotation/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: Entity node */
  constant property INotation/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property INotation/PROCESSING-INSTRUCTION-NODE :: <integer>, 
        name: "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001E);

  /* constant: Comment node */
  constant property INotation/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property INotation/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property INotation/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property INotation/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040022);

  /* constant: Notation node */
  constant property INotation/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* the public ID */
  function INotation/getPublicId () => (arg-result :: <object>), name: 
        "getPublicId", disp-id: as(<machine-word>, #x60040024);

  /* the system ID */
  function INotation/getSystemId () => (arg-result :: <object>), name: 
        "getSystemId", disp-id: as(<machine-word>, #x60040025);
end dispatch-client <INotation>;


/* COM class: Entity version 0.0
 * GUID: {DC0B08A0-94D3-11D4-9064-00C04F78ACF9}
 * Description: Entity Class
 */
define constant $Entity-class-id = as(<REFCLSID>, 
        "{DC0B08A0-94D3-11D4-9064-00C04F78ACF9}");

define function make-Entity () => (default-interface :: <IEntity>, 
        interface-2 :: <IDOMNode>, interface-3 :: <IDOMEntity>, interface-4 
        :: <IInternalRef>, interface-5 :: <IContainer>)
  let default-interface = make(<IEntity>, class-id: $Entity-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMEntity>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-Entity;


/* Dispatch interface: IDOMEntity version 0.0
 * GUID: {3EFAA423-272F-11D2-836F-0000F87A7782}
 */
define dispatch-client <IDOMEntity>
  uuid "{3EFAA423-272F-11D2-836F-0000F87A7782}";

  /* name of the node */
  constant property IDOMEntity/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IDOMEntity/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IDOMEntity/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IDOMEntity/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IDOMEntity/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IDOMEntity/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IDOMEntity/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IDOMEntity/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IDOMEntity/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IDOMEntity/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IDOMEntity/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IDOMEntity/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IDOMEntity/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IDOMEntity/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IDOMEntity/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IDOMEntity/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IDOMEntity/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the public ID */
  constant property IDOMEntity/publicId :: <object>, name: "publicId", 
        disp-id: 322;

  /* the system ID */
  constant property IDOMEntity/systemId :: <object>, name: "systemId", 
        disp-id: 323;

  /* the name of the notation */
  constant property IDOMEntity/notationName :: <string>, name: 
        "notationName", disp-id: 324;
end dispatch-client <IDOMEntity>;


/* Dispatch interface: IEntity version 0.0
 * GUID: {C7878050-B43F-11D4-906B-00C04F78ACF9}
 * Description: IEntity Interface
 */
define dispatch-client <IEntity>
  uuid "{C7878050-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IEntity/nodeName :: <string>, name: "nodeName", 
        disp-id: 2;

  /* value stored in the node */
  property IEntity/nodeValue :: <object>, name: "nodeValue", disp-id: 3;

  /* the node's type */
  constant property IEntity/nodeType :: <DOMNodeType>, name: "nodeType", 
        disp-id: 4;

  /* parent of the node */
  constant property IEntity/parentNode :: <IDOMNode>, name: "parentNode", 
        disp-id: 6;

  /* the collection of the node's children */
  constant property IEntity/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IEntity/firstChild :: <IDOMNode>, name: "firstChild", 
        disp-id: 8;

  /* first child of the node */
  constant property IEntity/lastChild :: <IDOMNode>, name: "lastChild", 
        disp-id: 9;

  /* left sibling of the node */
  constant property IEntity/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IEntity/nextSibling :: <IDOMNode>, name: "nextSibling", 
        disp-id: 11;

  /* the collection of the node's attributes */
  constant property IEntity/attributes :: <IDOMNamedNodeMap>, name: 
        "attributes", disp-id: 12;

  /* insert a child node */
  function IEntity/insertBefore (arg-newChild :: <IDOMNode>, arg-refChild 
        :: <object>) => (arg-result :: <IDOMNode>), name: "insertBefore", 
        disp-id: 13;

  /* replace a child node */
  function IEntity/replaceChild (arg-newChild :: <IDOMNode>, arg-oldChild 
        :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: "replaceChild", 
        disp-id: 14;

  /* remove a child node */
  function IEntity/removeChild (arg-childNode :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IEntity/appendChild (arg-newChild :: <IDOMNode>) => (arg-result 
        :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IEntity/hasChildNodes () => (arg-result :: <boolean>), name: 
        "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IEntity/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IEntity/cloneNode (arg-deep :: <boolean>) => (arg-result :: 
        <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the public ID */
  constant property IEntity/publicId :: <object>, name: "publicId", 
        disp-id: 322;

  /* the system ID */
  constant property IEntity/systemId :: <object>, name: "systemId", 
        disp-id: 323;

  /* the name of the notation */
  constant property IEntity/notationName :: <string>, name: "notationName", 
        disp-id: 324;

  /* name of the node */
  function IEntity/getNodeName () => (arg-result :: <string>), name: 
        "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IEntity/getNodeValue () => (arg-result :: <object>), name: 
        "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IEntity/setNodeValue (arg-value :: <object>) => (), name: 
        "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IEntity/getNodeType () => (arg-result :: <DOMNodeType>), name: 
        "getNodeType", disp-id: as(<machine-word>, #x60040003);

  /* parent of the node */
  function IEntity/getParentNode () => (arg-result :: <IDOMNode>), name: 
        "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IEntity/getChildNodes () => (arg-result :: <IDOMNodeList>), 
        name: "getChildNodes", disp-id: as(<machine-word>, #x60040005);

  /* first child of the node */
  function IEntity/getFirstChild () => (arg-result :: <IDOMNode>), name: 
        "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IEntity/getLastChild () => (arg-result :: <IDOMNode>), name: 
        "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IEntity/getPreviousSibling () => (arg-result :: <IDOMNode>), 
        name: "getPreviousSibling", disp-id: as(<machine-word>, 
        #x60040008);

  /* right sibling of the node */
  function IEntity/getNextSibling () => (arg-result :: <IDOMNode>), name: 
        "getNextSibling", disp-id: as(<machine-word>, #x60040009);

  /* the collection of the node's attributes */
  function IEntity/getAttributes () => (arg-result :: <IDOMNamedNodeMap>), 
        name: "getAttributes", disp-id: as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IEntity/getOwnerDocument () => (arg-result :: <IDOMDocument>), 
        name: "getOwnerDocument", disp-id: as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IEntity/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IEntity/supports (arg-featureStr :: <string>, arg-versionStr :: 
        <string>) => (arg-result :: <boolean>), name: "supports", disp-id: 
        as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IEntity/isSupported (arg-featureStr :: <string>, arg-versionStr 
        :: <string>) => (arg-result :: <boolean>), name: "isSupported", 
        disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IEntity/namespaceURI :: <string>, name: "namespaceURI", 
        disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IEntity/getNamespaceURI () => (arg-result :: <string>), name: 
        "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property IEntity/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IEntity/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IEntity/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IEntity/localName :: <string>, name: "localName", 
        disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IEntity/getLocalName () => (arg-result :: <string>), name: 
        "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IEntity/hasAttributes () => (arg-result :: <boolean>), name: 
        "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property IEntity/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IEntity/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IEntity/TEXT-NODE :: <integer>, name: "TEXT_NODE", 
        disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IEntity/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property IEntity/ENTITY-REFERENCE-NODE :: <integer>, name: 
        "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: Entity node */
  constant property IEntity/ENTITY-NODE :: <integer>, name: "ENTITY_NODE", 
        disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IEntity/PROCESSING-INSTRUCTION-NODE :: <integer>, name: 
        "PROCESSING_INSTRUCTION_NODE", disp-id: as(<machine-word>, 
        #x6004001E);

  /* constant: Comment node */
  constant property IEntity/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IEntity/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IEntity/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property IEntity/DOCUMENT-FRAGMENT-NODE :: <integer>, name: 
        "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, #x60040022);

  /* constant: Notation node */
  constant property IEntity/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* the public ID */
  function IEntity/getPublicId () => (arg-result :: <object>), name: 
        "getPublicId", disp-id: as(<machine-word>, #x60040024);

  /* the system ID */
  function IEntity/getSystemId () => (arg-result :: <object>), name: 
        "getSystemId", disp-id: as(<machine-word>, #x60040025);

  /* the name of the notation */
  function IEntity/getNotationName () => (arg-result :: <string>), name: 
        "getNotationName", disp-id: as(<machine-word>, #x60040026);
end dispatch-client <IEntity>;


/* COM class: EntityReference version 0.0
 * GUID: {F41498B0-94D3-11D4-9064-00C04F78ACF9}
 * Description: EntityReference Class
 */
define constant $EntityReference-class-id = as(<REFCLSID>, 
        "{F41498B0-94D3-11D4-9064-00C04F78ACF9}");

define function make-EntityReference () => (default-interface :: 
        <IEntityReference>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMEntityReference>, interface-4 :: <IInternalRef>, interface-5 
        :: <IContainer>)
  let default-interface = make(<IEntityReference>, class-id: 
        $EntityReference-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMEntityReference>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-EntityReference;


/* Dispatch interface: IEntityReference version 0.0
 * GUID: {D318E9B0-B43F-11D4-906B-00C04F78ACF9}
 * Description: IEntityReference Interface
 */
define dispatch-client <IEntityReference>
  uuid "{D318E9B0-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IEntityReference/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IEntityReference/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IEntityReference/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IEntityReference/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IEntityReference/childNodes :: <IDOMNodeList>, name: 
        "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IEntityReference/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IEntityReference/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IEntityReference/previousSibling :: <IDOMNode>, name: 
        "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IEntityReference/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IEntityReference/attributes :: <IDOMNamedNodeMap>, 
        name: "attributes", disp-id: 12;

  /* insert a child node */
  function IEntityReference/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IEntityReference/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IEntityReference/removeChild (arg-childNode :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IEntityReference/appendChild (arg-newChild :: <IDOMNode>) => 
        (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IEntityReference/hasChildNodes () => (arg-result :: <boolean>), 
        name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IEntityReference/ownerDocument :: <IDOMDocument>, name: 
        "ownerDocument", disp-id: 18;

  function IEntityReference/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* name of the node */
  function IEntityReference/getNodeName () => (arg-result :: <string>), 
        name: "getNodeName", disp-id: as(<machine-word>, #x60040000);

  /* value stored in the node */
  function IEntityReference/getNodeValue () => (arg-result :: <object>), 
        name: "getNodeValue", disp-id: as(<machine-word>, #x60040001);

  /* value stored in the node */
  function IEntityReference/setNodeValue (arg-value :: <object>) => (), 
        name: "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function IEntityReference/getNodeType () => (arg-result :: 
        <DOMNodeType>), name: "getNodeType", disp-id: as(<machine-word>, 
        #x60040003);

  /* parent of the node */
  function IEntityReference/getParentNode () => (arg-result :: <IDOMNode>), 
        name: "getParentNode", disp-id: as(<machine-word>, #x60040004);

  /* the collection of the node's children */
  function IEntityReference/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function IEntityReference/getFirstChild () => (arg-result :: <IDOMNode>), 
        name: "getFirstChild", disp-id: as(<machine-word>, #x60040006);

  /* last child of the node */
  function IEntityReference/getLastChild () => (arg-result :: <IDOMNode>), 
        name: "getLastChild", disp-id: as(<machine-word>, #x60040007);

  /* left sibling of the node */
  function IEntityReference/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function IEntityReference/getNextSibling () => (arg-result :: 
        <IDOMNode>), name: "getNextSibling", disp-id: as(<machine-word>, 
        #x60040009);

  /* the collection of the node's attributes */
  function IEntityReference/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function IEntityReference/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IEntityReference/normalize () => (), name: "normalize", disp-id: 
        as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IEntityReference/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IEntityReference/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IEntityReference/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IEntityReference/getNamespaceURI () => (arg-result :: <string>), 
        name: "getNamespaceURI", disp-id: as(<machine-word>, #x60040010);

  /* value of the prefix */
  property IEntityReference/prefix :: <string>, name: "prefix", disp-id: 
        as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IEntityReference/getPrefix () => (arg-result :: <string>), name: 
        "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IEntityReference/setPrefix (arg-prefix :: <string>) => (), name: 
        "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IEntityReference/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IEntityReference/getLocalName () => (arg-result :: <string>), 
        name: "getLocalName", disp-id: as(<machine-word>, #x60040016);

  /* queries if node has attributes */
  function IEntityReference/hasAttributes () => (arg-result :: <boolean>), 
        name: "hasAttributes", disp-id: as(<machine-word>, #x60040017);

  /* constant: Element node */
  constant property IEntityReference/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IEntityReference/ATTRIBUTE-NODE :: <integer>, name: 
        "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IEntityReference/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IEntityReference/CDATA-SECTION-NODE :: <integer>, name: 
        "CDATA_SECTION_NODE", disp-id: as(<machine-word>, #x6004001B);

  /* constant: EntityReference node */
  constant property IEntityReference/ENTITY-REFERENCE-NODE :: <integer>, 
        name: "ENTITY_REFERENCE_NODE", disp-id: as(<machine-word>, 
        #x6004001C);

  /* constant: Entity node */
  constant property IEntityReference/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IEntityReference/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001E);

  /* constant: Comment node */
  constant property IEntityReference/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IEntityReference/DOCUMENT-NODE :: <integer>, name: 
        "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IEntityReference/DOCUMENT-TYPE-NODE :: <integer>, name: 
        "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, #x60040021);

  /* constant: DocumentFragment node */
  constant property IEntityReference/DOCUMENT-FRAGMENT-NODE :: <integer>, 
        name: "DOCUMENT_FRAGMENT_NODE", disp-id: as(<machine-word>, 
        #x60040022);

  /* constant: Notation node */
  constant property IEntityReference/NOTATION-NODE :: <integer>, name: 
        "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);
end dispatch-client <IEntityReference>;


/* COM class: ProcessingInstruction version 0.0
 * GUID: {0A73D370-94D4-11D4-9064-00C04F78ACF9}
 * Description: ProcessingInstruction Class
 */
define constant $ProcessingInstruction-class-id = as(<REFCLSID>, 
        "{0A73D370-94D4-11D4-9064-00C04F78ACF9}");

define function make-ProcessingInstruction () => (default-interface :: 
        <IProcessingInstruction>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMProcessingInstruction>, interface-4 :: <IInternalRef>, 
        interface-5 :: <IContainer>)
  let default-interface = make(<IProcessingInstruction>, class-id: 
        $ProcessingInstruction-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMProcessingInstruction>, disp-interface: 
        default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-ProcessingInstruction;


/* Dispatch interface: IProcessingInstruction version 0.0
 * GUID: {E9C92160-B43F-11D4-906B-00C04F78ACF9}
 * Description: IProcessingInstruction Interface
 */
define dispatch-client <IProcessingInstruction>
  uuid "{E9C92160-B43F-11D4-906B-00C04F78ACF9}";

  /* name of the node */
  constant property IProcessingInstruction/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property IProcessingInstruction/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property IProcessingInstruction/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property IProcessingInstruction/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property IProcessingInstruction/childNodes :: <IDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property IProcessingInstruction/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property IProcessingInstruction/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property IProcessingInstruction/previousSibling :: <IDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property IProcessingInstruction/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property IProcessingInstruction/attributes :: 
        <IDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function IProcessingInstruction/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function IProcessingInstruction/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function IProcessingInstruction/removeChild (arg-childNode :: <IDOMNode>) 
        => (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function IProcessingInstruction/appendChild (arg-newChild :: <IDOMNode>) 
        => (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function IProcessingInstruction/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property IProcessingInstruction/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function IProcessingInstruction/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* the target */
  constant property IProcessingInstruction/target :: <string>, name: 
        "target", disp-id: 226;

  /* the data */
  property IProcessingInstruction/data :: <string>, name: "data", disp-id: 
        0;

  /* get name of the node */
  function IProcessingInstruction/getNodeName () => (arg-result :: 
        <string>), name: "getNodeName", disp-id: as(<machine-word>, 
        #x60040000);

  /* get value stored in the node */
  function IProcessingInstruction/getNodeValue () => (arg-result :: 
        <object>), name: "getNodeValue", disp-id: as(<machine-word>, 
        #x60040001);

  /* set value stored in the node */
  function IProcessingInstruction/setNodeValue (arg-value :: <object>) => 
        (), name: "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* get the node's type */
  function IProcessingInstruction/getNodeType () => (arg-result :: 
        <DOMNodeType>), name: "getNodeType", disp-id: as(<machine-word>, 
        #x60040003);

  /* get parent of the node */
  function IProcessingInstruction/getParentNode () => (arg-result :: 
        <IDOMNode>), name: "getParentNode", disp-id: as(<machine-word>, 
        #x60040004);

  /* get the collection of the node's children */
  function IProcessingInstruction/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* get first child of the node */
  function IProcessingInstruction/getFirstChild () => (arg-result :: 
        <IDOMNode>), name: "getFirstChild", disp-id: as(<machine-word>, 
        #x60040006);

  /* get last child of the node */
  function IProcessingInstruction/getLastChild () => (arg-result :: 
        <IDOMNode>), name: "getLastChild", disp-id: as(<machine-word>, 
        #x60040007);

  /* get left sibling of the node */
  function IProcessingInstruction/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* get right sibling of the node */
  function IProcessingInstruction/getNextSibling () => (arg-result :: 
        <IDOMNode>), name: "getNextSibling", disp-id: as(<machine-word>, 
        #x60040009);

  /* get the collection of the node's attributes */
  function IProcessingInstruction/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* get document that contains the node */
  function IProcessingInstruction/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* normalize Text nodes */
  function IProcessingInstruction/normalize () => (), name: "normalize", 
        disp-id: as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function IProcessingInstruction/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000D);

  /* queries support of given feature */
  function IProcessingInstruction/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000E);

  /* value of the namespaceURI */
  constant property IProcessingInstruction/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000F);

  /* gets URI of node's namespace */
  function IProcessingInstruction/getNamespaceURI () => (arg-result :: 
        <string>), name: "getNamespaceURI", disp-id: as(<machine-word>, 
        #x60040010);

  /* value of the prefix */
  property IProcessingInstruction/prefix :: <string>, name: "prefix", 
        disp-id: as(<machine-word>, #x60040011);

  /* gets node's prefix */
  function IProcessingInstruction/getPrefix () => (arg-result :: <string>), 
        name: "getPrefix", disp-id: as(<machine-word>, #x60040013);

  /* sets node's prefix */
  function IProcessingInstruction/setPrefix (arg-prefix :: <string>) => (), 
        name: "setPrefix", disp-id: as(<machine-word>, #x60040014);

  /* value of the localName */
  constant property IProcessingInstruction/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040015);

  /* gets local name of the node */
  function IProcessingInstruction/getLocalName () => (arg-result :: 
        <string>), name: "getLocalName", disp-id: as(<machine-word>, 
        #x60040016);

  /* queries if node has attributes */
  function IProcessingInstruction/hasAttributes () => (arg-result :: 
        <boolean>), name: "hasAttributes", disp-id: as(<machine-word>, 
        #x60040017);

  /* constant: Element node */
  constant property IProcessingInstruction/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Attribute node */
  constant property IProcessingInstruction/ATTRIBUTE-NODE :: <integer>, 
        name: "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: Text node */
  constant property IProcessingInstruction/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x6004001A);

  /* constant: CDATASection node */
  constant property IProcessingInstruction/CDATA-SECTION-NODE :: <integer>, 
        name: "CDATA_SECTION_NODE", disp-id: as(<machine-word>, 
        #x6004001B);

  /* constant: EntityReference node */
  constant property IProcessingInstruction/ENTITY-REFERENCE-NODE :: 
        <integer>, name: "ENTITY_REFERENCE_NODE", disp-id: 
        as(<machine-word>, #x6004001C);

  /* constant: Entity node */
  constant property IProcessingInstruction/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001D);

  /* constant: ProcessingInstruction node */
  constant property IProcessingInstruction/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001E);

  /* constant: Comment node */
  constant property IProcessingInstruction/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: Document node */
  constant property IProcessingInstruction/DOCUMENT-NODE :: <integer>, 
        name: "DOCUMENT_NODE", disp-id: as(<machine-word>, #x60040020);

  /* constant: DocumentType node */
  constant property IProcessingInstruction/DOCUMENT-TYPE-NODE :: <integer>, 
        name: "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, 
        #x60040021);

  /* constant: DocumentFragment node */
  constant property IProcessingInstruction/DOCUMENT-FRAGMENT-NODE :: 
        <integer>, name: "DOCUMENT_FRAGMENT_NODE", disp-id: 
        as(<machine-word>, #x60040022);

  /* constant: Notation node */
  constant property IProcessingInstruction/NOTATION-NODE :: <integer>, 
        name: "NOTATION_NODE", disp-id: as(<machine-word>, #x60040023);

  /* get the target */
  function IProcessingInstruction/getTarget () => (arg-result :: <string>), 
        name: "getTarget", disp-id: as(<machine-word>, #x60040024);

  /* get the data */
  function IProcessingInstruction/getData () => (arg-result :: <string>), 
        name: "getData", disp-id: as(<machine-word>, #x60040025);

  /* set the data */
  function IProcessingInstruction/setData (arg-value :: <string>) => (), 
        name: "setData", disp-id: as(<machine-word>, #x60040026);
end dispatch-client <IProcessingInstruction>;


/* COM class: Event version 0.0
 * GUID: {38FB6630-94CA-11D4-9064-00C04F78ACF9}
 * Description: Event Class
 */
define constant $Event-class-id = as(<REFCLSID>, 
        "{38FB6630-94CA-11D4-9064-00C04F78ACF9}");

define function make-Event () => (default-interface :: <IEvent>, 
        interface-2 :: <IInternalRef>)
  let default-interface = make(<IEvent>, class-id: $Event-class-id);
  values(default-interface,
         make(<IInternalRef>, disp-interface: default-interface))
end function make-Event;


/* COM class: UIEvent version 0.0
 * GUID: {3F70C710-94CA-11D4-9064-00C04F78ACF9}
 * Description: UIEvent Class
 */
define constant $UIEvent-class-id = as(<REFCLSID>, 
        "{3F70C710-94CA-11D4-9064-00C04F78ACF9}");

define function make-UIEvent () => (default-interface :: <IUIEvent>, 
        interface-2 :: <IEvent>, interface-3 :: <IInternalRef>)
  let default-interface = make(<IUIEvent>, class-id: $UIEvent-class-id);
  values(default-interface,
         make(<IEvent>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-UIEvent;


/* Dispatch interface: IUIEvent version 0.0
 * GUID: {D3B1E5E0-11B0-11D4-904B-00C04F78ACF9}
 * Description: IUIEvent Interface
 */
define dispatch-client <IUIEvent>
  uuid "{D3B1E5E0-11B0-11D4-904B-00C04F78ACF9}";

  /* constant: capturing phase */
  constant property IUIEvent/CAPTURING-PHASE :: <integer>, name: 
        "CAPTURING_PHASE", disp-id: as(<machine-word>, #x60020000);

  /* constant: at target */
  constant property IUIEvent/AT-TARGET :: <integer>, name: "AT_TARGET", 
        disp-id: as(<machine-word>, #x60020001);

  /* constant: bubbling phase */
  constant property IUIEvent/BUBBLING-PHASE :: <integer>, name: 
        "BUBBLING_PHASE", disp-id: as(<machine-word>, #x60020002);

  /* event type */
  constant property IUIEvent/type :: <string>, name: "type", disp-id: 21;

  /* event target */
  constant property IUIEvent/target :: <IDOMNode>, name: "target", disp-id: 
        22;

  /* current node of event */
  constant property IUIEvent/currentNode :: <IDOMNode>, name: 
        "currentNode", disp-id: 23;

  /* current EventTarget of event */
  constant property IUIEvent/currentTarget :: <IDOMEventTarget>, name: 
        "currentTarget", disp-id: 20;

  /* event phase */
  constant property IUIEvent/eventPhase :: <integer>, name: "eventPhase", 
        disp-id: 24;

  /* event bubbles */
  constant property IUIEvent/bubbles :: <boolean>, name: "bubbles", 
        disp-id: 25;

  /* event is cancelable */
  constant property IUIEvent/cancelable :: <boolean>, name: "cancelable", 
        disp-id: 26;

  /* timestamp */
  constant property IUIEvent/timeStamp :: <object>, name: "timeStamp", 
        disp-id: 27;

  /* get the type */
  function IUIEvent/getType () => (arg-result :: <string>), name: 
        "getType", disp-id: as(<machine-word>, #x6002000B);

  /* get the target */
  function IUIEvent/getTarget () => (arg-result :: <IDOMNode>), name: 
        "getTarget", disp-id: as(<machine-word>, #x6002000C);

  /* get the current node */
  function IUIEvent/getCurrentNode () => (arg-result :: <IDOMNode>), name: 
        "getCurrentNode", disp-id: as(<machine-word>, #x6002000D);

  /* get the current EventTarget */
  function IUIEvent/getCurrentTarget () => (arg-result :: 
        <IDOMEventTarget>), name: "getCurrentTarget", disp-id: 
        as(<machine-word>, #x6002000E);

  /* get the event phase */
  function IUIEvent/getEventPhase () => (arg-result :: <integer>), name: 
        "getEventPhase", disp-id: as(<machine-word>, #x6002000F);

  /* does the event bubble? */
  function IUIEvent/getBubbles () => (arg-result :: <boolean>), name: 
        "getBubbles", disp-id: as(<machine-word>, #x60020010);

  /* is the event cancelable? */
  function IUIEvent/getCancelable () => (arg-result :: <boolean>), name: 
        "getCancelable", disp-id: as(<machine-word>, #x60020011);

  /* prevents further propagation of event */
  function IUIEvent/stopPropagation () => (), name: "stopPropagation", 
        disp-id: as(<machine-word>, #x60020012);

  /* prevents default action for event */
  function IUIEvent/preventDefault () => (), name: "preventDefault", 
        disp-id: as(<machine-word>, #x60020013);

  /* initializes an event */
  function IUIEvent/initEvent (arg-eventTypeArg :: <string>, 
        arg-canBubbleArg :: <boolean>, arg-cancelableArg :: <boolean>) => 
        (), name: "initEvent", disp-id: as(<machine-word>, #x60020014);

  /* get the timestamp */
  function IUIEvent/getTimeStamp () => (arg-result :: <object>), name: 
        "getTimeStamp", disp-id: as(<machine-word>, #x60020015);

  constant property IUIEvent/view :: <IAbstractView>, name: "view", 
        disp-id: 28;

  function IUIEvent/getView () => (arg-result :: <IAbstractView>), name: 
        "getView", disp-id: as(<machine-word>, #x60030001);

  constant property IUIEvent/detail :: type-union(<integer>, 
        <machine-word>), name: "detail", disp-id: 29;

  function IUIEvent/getDetail () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getDetail", disp-id: as(<machine-word>, 
        #x60030003);

  function IUIEvent/initUIEvent (arg-typeArg :: <string>, arg-canBubbleArg 
        :: type-union(<integer>, <machine-word>), arg-cancelableArg :: 
        type-union(<integer>, <machine-word>), arg-viewArg :: 
        <IAbstractView>, arg-detailArg :: type-union(<integer>, 
        <machine-word>)) => (), name: "initUIEvent", disp-id: 
        as(<machine-word>, #x60030004);
end dispatch-client <IUIEvent>;


/* Dispatch interface: IAbstractView version 0.0
 * GUID: {48ED71E0-11B2-11D4-904B-00C04F78ACF9}
 * Description: IAbstractView Interface
 */
define dispatch-client <IAbstractView>
  uuid "{48ED71E0-11B2-11D4-904B-00C04F78ACF9}";

  constant property IAbstractView/document :: <IDocumentView>, name: 
        "document", disp-id: 80;
end dispatch-client <IAbstractView>;


/* Dispatch interface: IDocumentView version 0.0
 * GUID: {56FC1070-11B2-11D4-904B-00C04F78ACF9}
 * Description: IDocumentView Interface
 */
define dispatch-client <IDocumentView>
  uuid "{56FC1070-11B2-11D4-904B-00C04F78ACF9}";

  constant property IDocumentView/defaultView :: <IAbstractView>, name: 
        "defaultView", disp-id: 81;
end dispatch-client <IDocumentView>;


/* COM class: MouseEvent version 0.0
 * GUID: {47194CA0-94CA-11D4-9064-00C04F78ACF9}
 * Description: MouseEvent Class
 */
define constant $MouseEvent-class-id = as(<REFCLSID>, 
        "{47194CA0-94CA-11D4-9064-00C04F78ACF9}");

define function make-MouseEvent () => (default-interface :: <IMouseEvent>, 
        interface-2 :: <IEvent>, interface-3 :: <IUIEvent>, interface-4 :: 
        <IInternalRef>)
  let default-interface = make(<IMouseEvent>, class-id: 
        $MouseEvent-class-id);
  values(default-interface,
         make(<IEvent>, disp-interface: default-interface),
         make(<IUIEvent>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-MouseEvent;


/* Dispatch interface: IMouseEvent version 0.0
 * GUID: {8B4B3EE0-8850-11D4-9060-00C04F78ACF9}
 * Description: IMouseEvent Interface
 */
define dispatch-client <IMouseEvent>
  uuid "{8B4B3EE0-8850-11D4-9060-00C04F78ACF9}";

  /* constant: capturing phase */
  constant property IMouseEvent/CAPTURING-PHASE :: <integer>, name: 
        "CAPTURING_PHASE", disp-id: as(<machine-word>, #x60020000);

  /* constant: at target */
  constant property IMouseEvent/AT-TARGET :: <integer>, name: "AT_TARGET", 
        disp-id: as(<machine-word>, #x60020001);

  /* constant: bubbling phase */
  constant property IMouseEvent/BUBBLING-PHASE :: <integer>, name: 
        "BUBBLING_PHASE", disp-id: as(<machine-word>, #x60020002);

  /* event type */
  constant property IMouseEvent/type :: <string>, name: "type", disp-id: 
        21;

  /* event target */
  constant property IMouseEvent/target :: <IDOMNode>, name: "target", 
        disp-id: 22;

  /* current node of event */
  constant property IMouseEvent/currentNode :: <IDOMNode>, name: 
        "currentNode", disp-id: 23;

  /* current EventTarget of event */
  constant property IMouseEvent/currentTarget :: <IDOMEventTarget>, name: 
        "currentTarget", disp-id: 20;

  /* event phase */
  constant property IMouseEvent/eventPhase :: <integer>, name: 
        "eventPhase", disp-id: 24;

  /* event bubbles */
  constant property IMouseEvent/bubbles :: <boolean>, name: "bubbles", 
        disp-id: 25;

  /* event is cancelable */
  constant property IMouseEvent/cancelable :: <boolean>, name: 
        "cancelable", disp-id: 26;

  /* timestamp */
  constant property IMouseEvent/timeStamp :: <object>, name: "timeStamp", 
        disp-id: 27;

  /* get the type */
  function IMouseEvent/getType () => (arg-result :: <string>), name: 
        "getType", disp-id: as(<machine-word>, #x6002000B);

  /* get the target */
  function IMouseEvent/getTarget () => (arg-result :: <IDOMNode>), name: 
        "getTarget", disp-id: as(<machine-word>, #x6002000C);

  /* get the current node */
  function IMouseEvent/getCurrentNode () => (arg-result :: <IDOMNode>), 
        name: "getCurrentNode", disp-id: as(<machine-word>, #x6002000D);

  /* get the current EventTarget */
  function IMouseEvent/getCurrentTarget () => (arg-result :: 
        <IDOMEventTarget>), name: "getCurrentTarget", disp-id: 
        as(<machine-word>, #x6002000E);

  /* get the event phase */
  function IMouseEvent/getEventPhase () => (arg-result :: <integer>), name: 
        "getEventPhase", disp-id: as(<machine-word>, #x6002000F);

  /* does the event bubble? */
  function IMouseEvent/getBubbles () => (arg-result :: <boolean>), name: 
        "getBubbles", disp-id: as(<machine-word>, #x60020010);

  /* is the event cancelable? */
  function IMouseEvent/getCancelable () => (arg-result :: <boolean>), name: 
        "getCancelable", disp-id: as(<machine-word>, #x60020011);

  /* prevents further propagation of event */
  function IMouseEvent/stopPropagation () => (), name: "stopPropagation", 
        disp-id: as(<machine-word>, #x60020012);

  /* prevents default action for event */
  function IMouseEvent/preventDefault () => (), name: "preventDefault", 
        disp-id: as(<machine-word>, #x60020013);

  /* initializes an event */
  function IMouseEvent/initEvent (arg-eventTypeArg :: <string>, 
        arg-canBubbleArg :: <boolean>, arg-cancelableArg :: <boolean>) => 
        (), name: "initEvent", disp-id: as(<machine-word>, #x60020014);

  /* get the timestamp */
  function IMouseEvent/getTimeStamp () => (arg-result :: <object>), name: 
        "getTimeStamp", disp-id: as(<machine-word>, #x60020015);

  constant property IMouseEvent/view :: <IAbstractView>, name: "view", 
        disp-id: 28;

  function IMouseEvent/getView () => (arg-result :: <IAbstractView>), name: 
        "getView", disp-id: as(<machine-word>, #x60030001);

  constant property IMouseEvent/detail :: type-union(<integer>, 
        <machine-word>), name: "detail", disp-id: 29;

  function IMouseEvent/getDetail () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getDetail", disp-id: as(<machine-word>, 
        #x60030003);

  function IMouseEvent/initUIEvent (arg-typeArg :: <string>, 
        arg-canBubbleArg :: type-union(<integer>, <machine-word>), 
        arg-cancelableArg :: type-union(<integer>, <machine-word>), 
        arg-viewArg :: <IAbstractView>, arg-detailArg :: 
        type-union(<integer>, <machine-word>)) => (), name: "initUIEvent", 
        disp-id: as(<machine-word>, #x60030004);

  constant property IMouseEvent/screenX :: type-union(<integer>, 
        <machine-word>), name: "screenX", disp-id: 30;

  constant property IMouseEvent/screenY :: type-union(<integer>, 
        <machine-word>), name: "screenY", disp-id: 31;

  constant property IMouseEvent/clientX :: type-union(<integer>, 
        <machine-word>), name: "clientX", disp-id: 32;

  constant property IMouseEvent/clientY :: type-union(<integer>, 
        <machine-word>), name: "clientY", disp-id: 33;

  constant property IMouseEvent/ctrlKey :: type-union(<integer>, 
        <machine-word>), name: "ctrlKey", disp-id: 34;

  constant property IMouseEvent/shiftKey :: type-union(<integer>, 
        <machine-word>), name: "shiftKey", disp-id: 35;

  constant property IMouseEvent/altKey :: type-union(<integer>, 
        <machine-word>), name: "altKey", disp-id: 36;

  constant property IMouseEvent/metaKey :: type-union(<integer>, 
        <machine-word>), name: "metaKey", disp-id: 37;

  constant property IMouseEvent/button :: <integer>, name: "button", 
        disp-id: 40;

  constant property IMouseEvent/relatedTarget :: <IDOMEventTarget>, name: 
        "relatedTarget", disp-id: 41;

  function IMouseEvent/initMouseEvent (arg-typeArg :: <string>, 
        arg-canBubbleArg :: type-union(<integer>, <machine-word>), 
        arg-cancelableArg :: type-union(<integer>, <machine-word>), 
        arg-viewArg :: <IAbstractView>, arg-detailArg :: 
        type-union(<integer>, <machine-word>), arg-screenXArg :: 
        type-union(<integer>, <machine-word>), arg-screenYArg :: 
        type-union(<integer>, <machine-word>), arg-clientXArg :: 
        type-union(<integer>, <machine-word>), arg-clientYArg :: 
        type-union(<integer>, <machine-word>), arg-ctrlKeyArg :: 
        type-union(<integer>, <machine-word>), arg-altKeyArg :: 
        type-union(<integer>, <machine-word>), arg-shiftKeyArg :: 
        type-union(<integer>, <machine-word>), arg-metaKeyArg :: 
        type-union(<integer>, <machine-word>), arg-buttonArg :: <integer>, 
        arg-ppRelatedTargetArg :: <IDOMEventTarget*>) => (), name: 
        "initMouseEvent", disp-id: as(<machine-word>, #x6004000A);

  function IMouseEvent/getScreenX () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getScreenX", 
        disp-id: as(<machine-word>, #x6004000B);

  function IMouseEvent/getScreenY () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getScreenY", 
        disp-id: as(<machine-word>, #x6004000C);

  function IMouseEvent/getClientX () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getClientX", 
        disp-id: as(<machine-word>, #x6004000D);

  function IMouseEvent/getClientY () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getClientY", 
        disp-id: as(<machine-word>, #x6004000E);

  function IMouseEvent/getCtrlKey () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getCtrlKey", 
        disp-id: as(<machine-word>, #x6004000F);

  function IMouseEvent/getShiftKey () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getShiftKey", 
        disp-id: as(<machine-word>, #x60040010);

  function IMouseEvent/getAltKey () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getAltKey", disp-id: as(<machine-word>, 
        #x60040011);

  function IMouseEvent/getMetaKey () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getMetaKey", 
        disp-id: as(<machine-word>, #x60040012);

  function IMouseEvent/getButton () => (arg-result :: <integer>), name: 
        "getButton", disp-id: as(<machine-word>, #x60040013);

  function IMouseEvent/getRelatedTarget () => (arg-result :: 
        <IDOMEventTarget>), name: "getRelatedTarget", disp-id: 
        as(<machine-word>, #x60040014);
end dispatch-client <IMouseEvent>;


/* COM class: KeyEvent version 0.0
 * GUID: {4E1217A0-94CA-11D4-9064-00C04F78ACF9}
 * Description: KeyEvent Class
 */
define constant $KeyEvent-class-id = as(<REFCLSID>, 
        "{4E1217A0-94CA-11D4-9064-00C04F78ACF9}");

define function make-KeyEvent () => (default-interface :: <IKeyEvent>, 
        interface-2 :: <IEvent>, interface-3 :: <IUIEvent>, interface-4 :: 
        <IInternalRef>)
  let default-interface = make(<IKeyEvent>, class-id: $KeyEvent-class-id);
  values(default-interface,
         make(<IEvent>, disp-interface: default-interface),
         make(<IUIEvent>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-KeyEvent;


/* Dispatch interface: IKeyEvent version 0.0
 * GUID: {3EF1C230-11B8-11D4-904B-00C04F78ACF9}
 * Description: IKeyEvent Interface
 */
define dispatch-client <IKeyEvent>
  uuid "{3EF1C230-11B8-11D4-904B-00C04F78ACF9}";

  /* constant: capturing phase */
  constant property IKeyEvent/CAPTURING-PHASE :: <integer>, name: 
        "CAPTURING_PHASE", disp-id: as(<machine-word>, #x60020000);

  /* constant: at target */
  constant property IKeyEvent/AT-TARGET :: <integer>, name: "AT_TARGET", 
        disp-id: as(<machine-word>, #x60020001);

  /* constant: bubbling phase */
  constant property IKeyEvent/BUBBLING-PHASE :: <integer>, name: 
        "BUBBLING_PHASE", disp-id: as(<machine-word>, #x60020002);

  /* event type */
  constant property IKeyEvent/type :: <string>, name: "type", disp-id: 21;

  /* event target */
  constant property IKeyEvent/target :: <IDOMNode>, name: "target", 
        disp-id: 22;

  /* current node of event */
  constant property IKeyEvent/currentNode :: <IDOMNode>, name: 
        "currentNode", disp-id: 23;

  /* current EventTarget of event */
  constant property IKeyEvent/currentTarget :: <IDOMEventTarget>, name: 
        "currentTarget", disp-id: 20;

  /* event phase */
  constant property IKeyEvent/eventPhase :: <integer>, name: "eventPhase", 
        disp-id: 24;

  /* event bubbles */
  constant property IKeyEvent/bubbles :: <boolean>, name: "bubbles", 
        disp-id: 25;

  /* event is cancelable */
  constant property IKeyEvent/cancelable :: <boolean>, name: "cancelable", 
        disp-id: 26;

  /* timestamp */
  constant property IKeyEvent/timeStamp :: <object>, name: "timeStamp", 
        disp-id: 27;

  /* get the type */
  function IKeyEvent/getType () => (arg-result :: <string>), name: 
        "getType", disp-id: as(<machine-word>, #x6002000B);

  /* get the target */
  function IKeyEvent/getTarget () => (arg-result :: <IDOMNode>), name: 
        "getTarget", disp-id: as(<machine-word>, #x6002000C);

  /* get the current node */
  function IKeyEvent/getCurrentNode () => (arg-result :: <IDOMNode>), name: 
        "getCurrentNode", disp-id: as(<machine-word>, #x6002000D);

  /* get the current EventTarget */
  function IKeyEvent/getCurrentTarget () => (arg-result :: 
        <IDOMEventTarget>), name: "getCurrentTarget", disp-id: 
        as(<machine-word>, #x6002000E);

  /* get the event phase */
  function IKeyEvent/getEventPhase () => (arg-result :: <integer>), name: 
        "getEventPhase", disp-id: as(<machine-word>, #x6002000F);

  /* does the event bubble? */
  function IKeyEvent/getBubbles () => (arg-result :: <boolean>), name: 
        "getBubbles", disp-id: as(<machine-word>, #x60020010);

  /* is the event cancelable? */
  function IKeyEvent/getCancelable () => (arg-result :: <boolean>), name: 
        "getCancelable", disp-id: as(<machine-word>, #x60020011);

  /* prevents further propagation of event */
  function IKeyEvent/stopPropagation () => (), name: "stopPropagation", 
        disp-id: as(<machine-word>, #x60020012);

  /* prevents default action for event */
  function IKeyEvent/preventDefault () => (), name: "preventDefault", 
        disp-id: as(<machine-word>, #x60020013);

  /* initializes an event */
  function IKeyEvent/initEvent (arg-eventTypeArg :: <string>, 
        arg-canBubbleArg :: <boolean>, arg-cancelableArg :: <boolean>) => 
        (), name: "initEvent", disp-id: as(<machine-word>, #x60020014);

  /* get the timestamp */
  function IKeyEvent/getTimeStamp () => (arg-result :: <object>), name: 
        "getTimeStamp", disp-id: as(<machine-word>, #x60020015);

  constant property IKeyEvent/view :: <IAbstractView>, name: "view", 
        disp-id: 28;

  function IKeyEvent/getView () => (arg-result :: <IAbstractView>), name: 
        "getView", disp-id: as(<machine-word>, #x60030001);

  constant property IKeyEvent/detail :: type-union(<integer>, 
        <machine-word>), name: "detail", disp-id: 29;

  function IKeyEvent/getDetail () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getDetail", disp-id: as(<machine-word>, 
        #x60030003);

  function IKeyEvent/initUIEvent (arg-typeArg :: <string>, arg-canBubbleArg 
        :: type-union(<integer>, <machine-word>), arg-cancelableArg :: 
        type-union(<integer>, <machine-word>), arg-viewArg :: 
        <IAbstractView>, arg-detailArg :: type-union(<integer>, 
        <machine-word>)) => (), name: "initUIEvent", disp-id: 
        as(<machine-word>, #x60030004);

  constant property IKeyEvent/screenX :: type-union(<integer>, 
        <machine-word>), name: "screenX", disp-id: 30;

  constant property IKeyEvent/screenY :: type-union(<integer>, 
        <machine-word>), name: "screenY", disp-id: 31;

  constant property IKeyEvent/clientX :: type-union(<integer>, 
        <machine-word>), name: "clientX", disp-id: 32;

  constant property IKeyEvent/clientY :: type-union(<integer>, 
        <machine-word>), name: "clientY", disp-id: 33;

  constant property IKeyEvent/ctrlKey :: type-union(<integer>, 
        <machine-word>), name: "ctrlKey", disp-id: 34;

  constant property IKeyEvent/shiftKey :: type-union(<integer>, 
        <machine-word>), name: "shiftKey", disp-id: 35;

  constant property IKeyEvent/altKey :: type-union(<integer>, 
        <machine-word>), name: "altKey", disp-id: 36;

  constant property IKeyEvent/metaKey :: type-union(<integer>, 
        <machine-word>), name: "metaKey", disp-id: 37;

  constant property IKeyEvent/keyCode :: type-union(<integer>, 
        <machine-word>), name: "keyCode", disp-id: 38;

  constant property IKeyEvent/charCode :: type-union(<integer>, 
        <machine-word>), name: "charCode", disp-id: 39;

  function IKeyEvent/initKeyEvent (arg-typeArg :: <string>, 
        arg-canBubbleArg :: type-union(<integer>, <machine-word>), 
        arg-cancelableArg :: type-union(<integer>, <machine-word>), 
        arg-viewArg :: <IAbstractView>, arg-detailArg :: 
        type-union(<integer>, <machine-word>), arg-screenXArg :: 
        type-union(<integer>, <machine-word>), arg-screenYArg :: 
        type-union(<integer>, <machine-word>), arg-clientXArg :: 
        type-union(<integer>, <machine-word>), arg-clientYArg :: 
        type-union(<integer>, <machine-word>), arg-ctrlKeyArg :: 
        type-union(<integer>, <machine-word>), arg-altKeyArg :: 
        type-union(<integer>, <machine-word>), arg-shiftKeyArg :: 
        type-union(<integer>, <machine-word>), arg-metaKeyArg :: 
        type-union(<integer>, <machine-word>), arg-keyCode :: 
        type-union(<integer>, <machine-word>), arg-charCode :: 
        type-union(<integer>, <machine-word>)) => (), name: "initKeyEvent", 
        disp-id: as(<machine-word>, #x6004000A);

  function IKeyEvent/getScreenX () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getScreenX", disp-id: as(<machine-word>, 
        #x6004000B);

  function IKeyEvent/getScreenY () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getScreenY", disp-id: as(<machine-word>, 
        #x6004000C);

  function IKeyEvent/getClientX () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getClientX", disp-id: as(<machine-word>, 
        #x6004000D);

  function IKeyEvent/getClientY () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getClientY", disp-id: as(<machine-word>, 
        #x6004000E);

  function IKeyEvent/getCtrlKey () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getCtrlKey", disp-id: as(<machine-word>, 
        #x6004000F);

  function IKeyEvent/getShiftKey () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getShiftKey", disp-id: as(<machine-word>, 
        #x60040010);

  function IKeyEvent/getAltKey () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getAltKey", disp-id: as(<machine-word>, 
        #x60040011);

  function IKeyEvent/getMetaKey () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getMetaKey", disp-id: as(<machine-word>, 
        #x60040012);

  function IKeyEvent/getKeyCode () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getKeyCode", disp-id: as(<machine-word>, 
        #x60040013);

  function IKeyEvent/getCharCode () => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getCharCode", disp-id: as(<machine-word>, 
        #x60040014);
end dispatch-client <IKeyEvent>;


/* COM class: MutationEvent version 0.0
 * GUID: {5431E600-94CA-11D4-9064-00C04F78ACF9}
 * Description: MutationEvent Class
 */
define constant $MutationEvent-class-id = as(<REFCLSID>, 
        "{5431E600-94CA-11D4-9064-00C04F78ACF9}");

define function make-MutationEvent () => (default-interface :: 
        <IMutationEvent>, interface-2 :: <IEvent>, interface-3 :: 
        <IInternalRef>)
  let default-interface = make(<IMutationEvent>, class-id: 
        $MutationEvent-class-id);
  values(default-interface,
         make(<IEvent>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-MutationEvent;


/* Dispatch interface: IMutationEvent version 0.0
 * GUID: {0EE59290-B443-11D4-906B-00C04F78ACF9}
 * Description: IMutationEvent Interface
 */
define dispatch-client <IMutationEvent>
  uuid "{0EE59290-B443-11D4-906B-00C04F78ACF9}";

  /* constant: capturing phase */
  constant property IMutationEvent/CAPTURING-PHASE :: <integer>, name: 
        "CAPTURING_PHASE", disp-id: as(<machine-word>, #x60020000);

  /* constant: at target */
  constant property IMutationEvent/AT-TARGET :: <integer>, name: 
        "AT_TARGET", disp-id: as(<machine-word>, #x60020001);

  /* constant: bubbling phase */
  constant property IMutationEvent/BUBBLING-PHASE :: <integer>, name: 
        "BUBBLING_PHASE", disp-id: as(<machine-word>, #x60020002);

  /* event type */
  constant property IMutationEvent/type :: <string>, name: "type", disp-id: 
        21;

  /* event target */
  constant property IMutationEvent/target :: <IDOMNode>, name: "target", 
        disp-id: 22;

  /* current node of event */
  constant property IMutationEvent/currentNode :: <IDOMNode>, name: 
        "currentNode", disp-id: 23;

  /* current EventTarget of event */
  constant property IMutationEvent/currentTarget :: <IDOMEventTarget>, 
        name: "currentTarget", disp-id: 20;

  /* event phase */
  constant property IMutationEvent/eventPhase :: <integer>, name: 
        "eventPhase", disp-id: 24;

  /* event bubbles */
  constant property IMutationEvent/bubbles :: <boolean>, name: "bubbles", 
        disp-id: 25;

  /* event is cancelable */
  constant property IMutationEvent/cancelable :: <boolean>, name: 
        "cancelable", disp-id: 26;

  /* timestamp */
  constant property IMutationEvent/timeStamp :: <object>, name: 
        "timeStamp", disp-id: 27;

  /* get the type */
  function IMutationEvent/getType () => (arg-result :: <string>), name: 
        "getType", disp-id: as(<machine-word>, #x6002000B);

  /* get the target */
  function IMutationEvent/getTarget () => (arg-result :: <IDOMNode>), name: 
        "getTarget", disp-id: as(<machine-word>, #x6002000C);

  /* get the current node */
  function IMutationEvent/getCurrentNode () => (arg-result :: <IDOMNode>), 
        name: "getCurrentNode", disp-id: as(<machine-word>, #x6002000D);

  /* get the current EventTarget */
  function IMutationEvent/getCurrentTarget () => (arg-result :: 
        <IDOMEventTarget>), name: "getCurrentTarget", disp-id: 
        as(<machine-word>, #x6002000E);

  /* get the event phase */
  function IMutationEvent/getEventPhase () => (arg-result :: <integer>), 
        name: "getEventPhase", disp-id: as(<machine-word>, #x6002000F);

  /* does the event bubble? */
  function IMutationEvent/getBubbles () => (arg-result :: <boolean>), name: 
        "getBubbles", disp-id: as(<machine-word>, #x60020010);

  /* is the event cancelable? */
  function IMutationEvent/getCancelable () => (arg-result :: <boolean>), 
        name: "getCancelable", disp-id: as(<machine-word>, #x60020011);

  /* prevents further propagation of event */
  function IMutationEvent/stopPropagation () => (), name: 
        "stopPropagation", disp-id: as(<machine-word>, #x60020012);

  /* prevents default action for event */
  function IMutationEvent/preventDefault () => (), name: "preventDefault", 
        disp-id: as(<machine-word>, #x60020013);

  /* initializes an event */
  function IMutationEvent/initEvent (arg-eventTypeArg :: <string>, 
        arg-canBubbleArg :: <boolean>, arg-cancelableArg :: <boolean>) => 
        (), name: "initEvent", disp-id: as(<machine-word>, #x60020014);

  /* get the timestamp */
  function IMutationEvent/getTimeStamp () => (arg-result :: <object>), 
        name: "getTimeStamp", disp-id: as(<machine-word>, #x60020015);

  constant property IMutationEvent/relatedNode :: <IDOMNode>, name: 
        "relatedNode", disp-id: 60;

  constant property IMutationEvent/prevValue :: <string>, name: 
        "prevValue", disp-id: 61;

  constant property IMutationEvent/newValue :: <string>, name: "newValue", 
        disp-id: 62;

  constant property IMutationEvent/attrName :: <string>, name: "attrName", 
        disp-id: 63;

  constant property IMutationEvent/attrChange :: <AttrChangeType>, name: 
        "attrChange", disp-id: 64;

  function IMutationEvent/initMutationEvent (arg-typeArg :: <string>, 
        arg-canBubbleArg :: type-union(<integer>, <machine-word>), 
        arg-cancelableArg :: type-union(<integer>, <machine-word>), 
        arg-pRelatedNode :: <IDOMNode>, arg-prevValueArg :: <string>, 
        arg-newValueArg :: <string>, arg-attrNameArg :: <string>) => (), 
        name: "initMutationEvent", disp-id: as(<machine-word>, #x60030005);

  function IMutationEvent/getRelatedNode () => (arg-result :: <IDOMNode>), 
        name: "getRelatedNode", disp-id: as(<machine-word>, #x60030006);

  function IMutationEvent/getPrevValue () => (arg-result :: <string>), 
        name: "getPrevValue", disp-id: as(<machine-word>, #x60030007);

  function IMutationEvent/getNewValue () => (arg-result :: <string>), name: 
        "getNewValue", disp-id: as(<machine-word>, #x60030008);

  function IMutationEvent/getAttrName () => (arg-result :: <string>), name: 
        "getAttrName", disp-id: as(<machine-word>, #x60030009);

  function IMutationEvent/getAttrChange () => (arg-result :: 
        <AttrChangeType>), name: "getAttrChange", disp-id: 
        as(<machine-word>, #x6003000A);

  /* constant: attr modification */
  constant property IMutationEvent/MODIFICATION :: <integer>, name: 
        "MODIFICATION", disp-id: as(<machine-word>, #x6003000B);

  /* constant: attr addition */
  constant property IMutationEvent/ADDITION :: <integer>, name: "ADDITION", 
        disp-id: as(<machine-word>, #x6003000C);

  /* constant: attr removal */
  constant property IMutationEvent/REMOVAL :: <integer>, name: "REMOVAL", 
        disp-id: as(<machine-word>, #x6003000D);
end dispatch-client <IMutationEvent>;


/* Typedef: AttrChangeType
 * Description: Constants that define a node's type
 */
define constant <AttrChangeType> = <tagAttrChangeType>;


/* Enumeration: tagAttrChangeType
 * Description: Constants that define a node's type
 */
define constant <tagAttrChangeType> = type-union(<integer>, 
        <machine-word>);
define constant $ATTR-CHANGE-TYPE-INVALID = 0;
define constant $MODIFICATION = 1;
define constant $ADDITION = 2;
define constant $REMOVAL = 3;


/* COM class: SVGDocument version 0.0
 * GUID: {B54A34A3-C623-11D4-9E59-0050BAA8920E}
 * Description: SVGDocument Class
 */
define constant $SVGDocument-class-id = as(<REFCLSID>, 
        "{B54A34A3-C623-11D4-9E59-0050BAA8920E}");

define function make-SVGDocument () => (default-interface :: 
        <ISVGDocument>, interface-2 :: <IDOMNode>, interface-3 :: 
        <IDOMDocument>, interface-4 :: <IDocument>, interface-5 :: 
        <IInternalRef>, interface-6 :: <IContainer>, interface-7 :: 
        <IAnimationTiming>)
  let default-interface = make(<ISVGDocument>, class-id: 
        $SVGDocument-class-id);
  values(default-interface,
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMDocument>, disp-interface: default-interface),
         make(<IDocument>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface),
         make(<IAnimationTiming>, disp-interface: default-interface))
end function make-SVGDocument;


/* COM class: SVGElement version 0.0
 * GUID: {3F3F1570-996B-11D4-9064-00C04F78ACF9}
 * Description: SVGElement Class
 */
define constant $SVGElement-class-id = as(<REFCLSID>, 
        "{3F3F1570-996B-11D4-9064-00C04F78ACF9}");

define function make-SVGElement () => (default-interface :: <ISVGElement>, 
        interface-2 :: <IDOMEventTarget>, interface-3 :: <IDOMNode>, 
        interface-4 :: <IDOMElement>, interface-5 :: <IElement>, 
        interface-6 :: <IInternalRef>, interface-7 :: <IContainer>)
  let default-interface = make(<ISVGElement>, class-id: 
        $SVGElement-class-id);
  values(default-interface,
         make(<IDOMEventTarget>, disp-interface: default-interface),
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMElement>, disp-interface: default-interface),
         make(<IElement>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-SVGElement;


/* COM class: SVGRect version 0.0
 * GUID: {855FD130-98B7-11D4-9064-00C04F78ACF9}
 * Description: SVGRect Class
 */
define constant $SVGRect-class-id = as(<REFCLSID>, 
        "{855FD130-98B7-11D4-9064-00C04F78ACF9}");

define function make-SVGRect () => (default-interface :: <ISVGRect>)
  let default-interface = make(<ISVGRect>, class-id: $SVGRect-class-id);
  values(default-interface)
end function make-SVGRect;


/* Dispatch interface: ISVGRect version 0.0
 * GUID: {7DFA02F0-9890-11D4-9064-00C04F78ACF9}
 * Description: ISVGRect Interface
 */
define dispatch-client <ISVGRect>
  uuid "{7DFA02F0-9890-11D4-9064-00C04F78ACF9}";

  /* property x */
  property ISVGRect/x :: <single-float>, name: "x", disp-id: 200;

  /* property y */
  property ISVGRect/y :: <single-float>, name: "y", disp-id: 201;

  /* property width */
  property ISVGRect/width :: <single-float>, name: "width", disp-id: 202;

  /* property height */
  property ISVGRect/height :: <single-float>, name: "height", disp-id: 203;

  /* method getX */
  function ISVGRect/getX () => (arg-result :: <single-float>), name: 
        "getX", disp-id: as(<machine-word>, #x60020008);

  /* method setX */
  function ISVGRect/setX (arg-x :: <single-float>) => (), name: "setX", 
        disp-id: as(<machine-word>, #x60020009);

  /* method getY */
  function ISVGRect/getY () => (arg-result :: <single-float>), name: 
        "getY", disp-id: as(<machine-word>, #x6002000A);

  /* method setY */
  function ISVGRect/setY (arg-y :: <single-float>) => (), name: "setY", 
        disp-id: as(<machine-word>, #x6002000B);

  /* method getWidth */
  function ISVGRect/getWidth () => (arg-result :: <single-float>), name: 
        "getWidth", disp-id: as(<machine-word>, #x6002000C);

  /* method setWidth */
  function ISVGRect/setWidth (arg-width :: <single-float>) => (), name: 
        "setWidth", disp-id: as(<machine-word>, #x6002000D);

  /* method getHeight */
  function ISVGRect/getHeight () => (arg-result :: <single-float>), name: 
        "getHeight", disp-id: as(<machine-word>, #x6002000E);

  /* method setHeight */
  function ISVGRect/setHeight (arg-height :: <single-float>) => (), name: 
        "setHeight", disp-id: as(<machine-word>, #x6002000F);
end dispatch-client <ISVGRect>;


/* COM class: SVGPoint version 0.0
 * GUID: {AF764840-98B7-11D4-9064-00C04F78ACF9}
 * Description: SVGPoint Class
 */
define constant $SVGPoint-class-id = as(<REFCLSID>, 
        "{AF764840-98B7-11D4-9064-00C04F78ACF9}");

define function make-SVGPoint () => (default-interface :: <ISVGPoint>)
  let default-interface = make(<ISVGPoint>, class-id: $SVGPoint-class-id);
  values(default-interface)
end function make-SVGPoint;


/* COM class: SVGTextContentElement version 0.0
 * GUID: {C82F92E0-98B7-11D4-9064-00C04F78ACF9}
 * Description: SVGTextContentElement Class
 */
define constant $SVGTextContentElement-class-id = as(<REFCLSID>, 
        "{C82F92E0-98B7-11D4-9064-00C04F78ACF9}");

define function make-SVGTextContentElement () => (default-interface :: 
        <ISVGTextContentElement>, interface-2 :: <IDOMEventTarget>, 
        interface-3 :: <IDOMNode>, interface-4 :: <IDOMElement>, 
        interface-5 :: <IElement>, interface-6 :: <ISVGElement>, 
        interface-7 :: <IInternalRef>, interface-8 :: <IContainer>)
  let default-interface = make(<ISVGTextContentElement>, class-id: 
        $SVGTextContentElement-class-id);
  values(default-interface,
         make(<IDOMEventTarget>, disp-interface: default-interface),
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMElement>, disp-interface: default-interface),
         make(<IElement>, disp-interface: default-interface),
         make(<ISVGElement>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-SVGTextContentElement;


/* Dispatch interface: ISVGTextContentElement version 0.0
 * GUID: {C3370960-98AE-11D4-9064-00C04F78ACF9}
 * Description: ISVGTextContentElement Interface
 */
define dispatch-client <ISVGTextContentElement>
  uuid "{C3370960-98AE-11D4-9064-00C04F78ACF9}";

  /* name of the node */
  constant property ISVGTextContentElement/nodeName :: <string>, name: 
        "nodeName", disp-id: 2;

  /* value stored in the node */
  property ISVGTextContentElement/nodeValue :: <object>, name: "nodeValue", 
        disp-id: 3;

  /* the node's type */
  constant property ISVGTextContentElement/nodeType :: <DOMNodeType>, name: 
        "nodeType", disp-id: 4;

  /* parent of the node */
  constant property ISVGTextContentElement/parentNode :: <IDOMNode>, name: 
        "parentNode", disp-id: 6;

  /* the collection of the node's children */
  constant property ISVGTextContentElement/childNodes :: <IDOMNodeList>, 
        name: "childNodes", disp-id: 7;

  /* first child of the node */
  constant property ISVGTextContentElement/firstChild :: <IDOMNode>, name: 
        "firstChild", disp-id: 8;

  /* first child of the node */
  constant property ISVGTextContentElement/lastChild :: <IDOMNode>, name: 
        "lastChild", disp-id: 9;

  /* left sibling of the node */
  constant property ISVGTextContentElement/previousSibling :: <IDOMNode>, 
        name: "previousSibling", disp-id: 10;

  /* right sibling of the node */
  constant property ISVGTextContentElement/nextSibling :: <IDOMNode>, name: 
        "nextSibling", disp-id: 11;

  /* the collection of the node's attributes */
  constant property ISVGTextContentElement/attributes :: 
        <IDOMNamedNodeMap>, name: "attributes", disp-id: 12;

  /* insert a child node */
  function ISVGTextContentElement/insertBefore (arg-newChild :: <IDOMNode>, 
        arg-refChild :: <object>) => (arg-result :: <IDOMNode>), name: 
        "insertBefore", disp-id: 13;

  /* replace a child node */
  function ISVGTextContentElement/replaceChild (arg-newChild :: <IDOMNode>, 
        arg-oldChild :: <IDOMNode>) => (arg-result :: <IDOMNode>), name: 
        "replaceChild", disp-id: 14;

  /* remove a child node */
  function ISVGTextContentElement/removeChild (arg-childNode :: <IDOMNode>) 
        => (arg-result :: <IDOMNode>), name: "removeChild", disp-id: 15;

  /* append a child node */
  function ISVGTextContentElement/appendChild (arg-newChild :: <IDOMNode>) 
        => (arg-result :: <IDOMNode>), name: "appendChild", disp-id: 16;

  function ISVGTextContentElement/hasChildNodes () => (arg-result :: 
        <boolean>), name: "hasChildNodes", disp-id: 17;

  /* document that contains the node */
  constant property ISVGTextContentElement/ownerDocument :: <IDOMDocument>, 
        name: "ownerDocument", disp-id: 18;

  function ISVGTextContentElement/cloneNode (arg-deep :: <boolean>) => 
        (arg-result :: <IDOMNode>), name: "cloneNode", disp-id: 19;

  /* get the tagName of the element */
  constant property ISVGTextContentElement/tagName :: <string>, name: 
        "tagName", disp-id: 98;

  /* look up the string value of an attribute by name */
  function ISVGTextContentElement/getAttribute (arg-name :: <string>) => 
        (arg-result :: <object>), name: "getAttribute", disp-id: 100;

  /* set the string value of an attribute by name */
  function ISVGTextContentElement/setAttribute (arg-name :: <string>, 
        arg-value :: <object>) => (), name: "setAttribute", disp-id: 101;

  /* remove an attribute by name */
  function ISVGTextContentElement/removeAttribute (arg-name :: <string>) => 
        (), name: "removeAttribute", disp-id: 102;

  /* look up the attribute node by name */
  function ISVGTextContentElement/getAttributeNode (arg-name :: <string>) 
        => (arg-result :: <IDOMAttribute>), name: "getAttributeNode", 
        disp-id: 103;

  /* set the specified attribute on the element */
  function ISVGTextContentElement/setAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNode", disp-id: 104;

  /* remove the specified attribute */
  function ISVGTextContentElement/removeAttributeNode (arg-DOMAttribute :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "removeAttributeNode", disp-id: 105;

  /* build a list of elements by name */
  function ISVGTextContentElement/getElementsByTagName (arg-tagName :: 
        <string>) => (arg-result :: <IDOMNodeList>), name: 
        "getElementsByTagName", disp-id: 106;

  /* collapse all adjacent text nodes in sub-tree */
  function ISVGTextContentElement/normalize () => (), name: "normalize", 
        disp-id: 107;

  /* name of the node */
  function ISVGTextContentElement/getNodeName () => (arg-result :: 
        <string>), name: "getNodeName", disp-id: as(<machine-word>, 
        #x60040000);

  /* value stored in the node */
  function ISVGTextContentElement/getNodeValue () => (arg-result :: 
        <object>), name: "getNodeValue", disp-id: as(<machine-word>, 
        #x60040001);

  /* value stored in the node */
  function ISVGTextContentElement/setNodeValue (arg-value :: <object>) => 
        (), name: "setNodeValue", disp-id: as(<machine-word>, #x60040002);

  /* the node's type */
  function ISVGTextContentElement/getNodeType () => (arg-result :: 
        <DOMNodeType>), name: "getNodeType", disp-id: as(<machine-word>, 
        #x60040003);

  /* parent of the node */
  function ISVGTextContentElement/getParentNode () => (arg-result :: 
        <IDOMNode>), name: "getParentNode", disp-id: as(<machine-word>, 
        #x60040004);

  /* the collection of the node's children */
  function ISVGTextContentElement/getChildNodes () => (arg-result :: 
        <IDOMNodeList>), name: "getChildNodes", disp-id: as(<machine-word>, 
        #x60040005);

  /* first child of the node */
  function ISVGTextContentElement/getFirstChild () => (arg-result :: 
        <IDOMNode>), name: "getFirstChild", disp-id: as(<machine-word>, 
        #x60040006);

  /* last child of the node */
  function ISVGTextContentElement/getLastChild () => (arg-result :: 
        <IDOMNode>), name: "getLastChild", disp-id: as(<machine-word>, 
        #x60040007);

  /* left sibling of the node */
  function ISVGTextContentElement/getPreviousSibling () => (arg-result :: 
        <IDOMNode>), name: "getPreviousSibling", disp-id: 
        as(<machine-word>, #x60040008);

  /* right sibling of the node */
  function ISVGTextContentElement/getNextSibling () => (arg-result :: 
        <IDOMNode>), name: "getNextSibling", disp-id: as(<machine-word>, 
        #x60040009);

  /* the collection of the node's attributes */
  function ISVGTextContentElement/getAttributes () => (arg-result :: 
        <IDOMNamedNodeMap>), name: "getAttributes", disp-id: 
        as(<machine-word>, #x6004000A);

  /* document that contains the node */
  function ISVGTextContentElement/getOwnerDocument () => (arg-result :: 
        <IDOMDocument>), name: "getOwnerDocument", disp-id: 
        as(<machine-word>, #x6004000B);

  /* queries support of given feature */
  function ISVGTextContentElement/supports (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "supports", disp-id: as(<machine-word>, #x6004000C);

  /* queries support of given feature */
  function ISVGTextContentElement/isSupported (arg-featureStr :: <string>, 
        arg-versionStr :: <string>) => (arg-result :: <boolean>), name: 
        "isSupported", disp-id: as(<machine-word>, #x6004000D);

  /* value of the namespaceURI */
  constant property ISVGTextContentElement/namespaceURI :: <string>, name: 
        "namespaceURI", disp-id: as(<machine-word>, #x6004000E);

  /* gets URI of node's namespace */
  function ISVGTextContentElement/getNamespaceURI () => (arg-result :: 
        <string>), name: "getNamespaceURI", disp-id: as(<machine-word>, 
        #x6004000F);

  /* value of the prefix */
  property ISVGTextContentElement/prefix :: <string>, name: "prefix", 
        disp-id: as(<machine-word>, #x60040010);

  /* gets node's prefix */
  function ISVGTextContentElement/getPrefix () => (arg-result :: <string>), 
        name: "getPrefix", disp-id: as(<machine-word>, #x60040012);

  /* sets node's prefix */
  function ISVGTextContentElement/setPrefix (arg-prefix :: <string>) => (), 
        name: "setPrefix", disp-id: as(<machine-word>, #x60040013);

  /* value of the localName */
  constant property ISVGTextContentElement/localName :: <string>, name: 
        "localName", disp-id: as(<machine-word>, #x60040014);

  /* gets local name of the node */
  function ISVGTextContentElement/getLocalName () => (arg-result :: 
        <string>), name: "getLocalName", disp-id: as(<machine-word>, 
        #x60040015);

  /* queries if node has attributes */
  function ISVGTextContentElement/hasAttributes () => (arg-result :: 
        <boolean>), name: "hasAttributes", disp-id: as(<machine-word>, 
        #x60040016);

  /* constant: Element node */
  constant property ISVGTextContentElement/ELEMENT-NODE :: <integer>, name: 
        "ELEMENT_NODE", disp-id: as(<machine-word>, #x60040017);

  /* constant: Attribute node */
  constant property ISVGTextContentElement/ATTRIBUTE-NODE :: <integer>, 
        name: "ATTRIBUTE_NODE", disp-id: as(<machine-word>, #x60040018);

  /* constant: Text node */
  constant property ISVGTextContentElement/TEXT-NODE :: <integer>, name: 
        "TEXT_NODE", disp-id: as(<machine-word>, #x60040019);

  /* constant: CDATASection node */
  constant property ISVGTextContentElement/CDATA-SECTION-NODE :: <integer>, 
        name: "CDATA_SECTION_NODE", disp-id: as(<machine-word>, 
        #x6004001A);

  /* constant: EntityReference node */
  constant property ISVGTextContentElement/ENTITY-REFERENCE-NODE :: 
        <integer>, name: "ENTITY_REFERENCE_NODE", disp-id: 
        as(<machine-word>, #x6004001B);

  /* constant: Entity node */
  constant property ISVGTextContentElement/ENTITY-NODE :: <integer>, name: 
        "ENTITY_NODE", disp-id: as(<machine-word>, #x6004001C);

  /* constant: ProcessingInstruction node */
  constant property ISVGTextContentElement/PROCESSING-INSTRUCTION-NODE :: 
        <integer>, name: "PROCESSING_INSTRUCTION_NODE", disp-id: 
        as(<machine-word>, #x6004001D);

  /* constant: Comment node */
  constant property ISVGTextContentElement/COMMENT-NODE :: <integer>, name: 
        "COMMENT_NODE", disp-id: as(<machine-word>, #x6004001E);

  /* constant: Document node */
  constant property ISVGTextContentElement/DOCUMENT-NODE :: <integer>, 
        name: "DOCUMENT_NODE", disp-id: as(<machine-word>, #x6004001F);

  /* constant: DocumentType node */
  constant property ISVGTextContentElement/DOCUMENT-TYPE-NODE :: <integer>, 
        name: "DOCUMENT_TYPE_NODE", disp-id: as(<machine-word>, 
        #x60040020);

  /* constant: DocumentFragment node */
  constant property ISVGTextContentElement/DOCUMENT-FRAGMENT-NODE :: 
        <integer>, name: "DOCUMENT_FRAGMENT_NODE", disp-id: 
        as(<machine-word>, #x60040021);

  /* constant: Notation node */
  constant property ISVGTextContentElement/NOTATION-NODE :: <integer>, 
        name: "NOTATION_NODE", disp-id: as(<machine-word>, #x60040022);

  /* get the tagName of the element */
  function ISVGTextContentElement/getTagName () => (arg-result :: 
        <string>), name: "getTagName", disp-id: as(<machine-word>, 
        #x60040023);

  /* add an EventListener */
  function ISVGTextContentElement/addEventListener (arg-type :: <string>, 
        arg-listener :: <object>, arg-useCapture :: <boolean>) => (), name: 
        "addEventListener", disp-id: as(<machine-word>, #x60040024);

  /* remove an EventListener */
  function ISVGTextContentElement/removeEventListener (arg-type :: 
        <string>, arg-listener :: <object>, arg-useCapture :: <boolean>) => 
        (), name: "removeEventListener", disp-id: as(<machine-word>, 
        #x60040025);

  /* dispatch an event */
  function ISVGTextContentElement/dispatchEvent (arg-evt :: <IEvent>) => 
        (arg-result :: <boolean>), name: "dispatchEvent", disp-id: 
        as(<machine-word>, #x60040026);

  /* gets an attribute */
  function ISVGTextContentElement/getAttributeNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: <string>), 
        name: "getAttributeNS", disp-id: as(<machine-word>, #x60040027);

  /* sets an attribute */
  function ISVGTextContentElement/setAttributeNS (arg-namespaceURI :: 
        <object>, arg-qualifiedName :: <string>, arg-value :: <string>) => 
        (), name: "setAttributeNS", disp-id: as(<machine-word>, 
        #x60040028);

  /* remove an attribute */
  function ISVGTextContentElement/removeAttributeNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (), name: 
        "removeAttributeNS", disp-id: as(<machine-word>, #x60040029);

  /* get an attribute node */
  function ISVGTextContentElement/getAttributeNodeNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: 
        <IDOMAttribute>), name: "getAttributeNodeNS", disp-id: 
        as(<machine-word>, #x6004002A);

  /* set an attribute node */
  function ISVGTextContentElement/setAttributeNodeNS (arg-newAttr :: 
        <IDOMAttribute>) => (arg-result :: <IDOMAttribute>), name: 
        "setAttributeNodeNS", disp-id: as(<machine-word>, #x6004002B);

  /* get elements by tag name */
  function ISVGTextContentElement/getElementsByTagNameNS (arg-namespaceURI 
        :: <object>, arg-localName :: <string>) => (arg-result :: 
        <IDOMNodeList>), name: "getElementsByTagNameNS", disp-id: 
        as(<machine-word>, #x6004002C);

  /* does element have the attribute? */
  function ISVGTextContentElement/hasAttribute (arg-name :: <string>) => 
        (arg-result :: <boolean>), name: "hasAttribute", disp-id: 
        as(<machine-word>, #x6004002D);

  /* does element have the attribute? */
  function ISVGTextContentElement/hasAttributeNS (arg-namespaceURI :: 
        <object>, arg-localName :: <string>) => (arg-result :: <boolean>), 
        name: "hasAttributeNS", disp-id: as(<machine-word>, #x6004002E);

  /* method getStyle */
  function ISVGTextContentElement/getStyle () => (arg-result :: 
        <ICSSStyleDeclaration>), name: "getStyle", disp-id: 121;

  /* property style */
  constant property ISVGTextContentElement/style :: <ICSSStyleDeclaration>, 
        name: "style", disp-id: 123;

  /* property id */
  property ISVGTextContentElement/id :: <string>, name: "id", disp-id: 125;

  /* method getId */
  function ISVGTextContentElement/getId () => (arg-result :: <string>), 
        name: "getId", disp-id: as(<machine-word>, #x60050004);

  /* method setId */
  function ISVGTextContentElement/setId (arg-id :: <string>) => (), name: 
        "setId", disp-id: as(<machine-word>, #x60050005);

  /* property ownerSVGElement */
  constant property ISVGTextContentElement/ownerSVGElement :: 
        <ISVGSVGElement>, name: "ownerSVGElement", disp-id: 126;

  /* method getOwnerSVGElement */
  function ISVGTextContentElement/getOwnerSVGElement () => (arg-result :: 
        <ISVGSVGElement>), name: "getOwnerSVGElement", disp-id: 
        as(<machine-word>, #x60050007);

  /* property viewportElement */
  constant property ISVGTextContentElement/viewportElement :: 
        <ISVGElement>, name: "viewportElement", disp-id: 127;

  /* method getViewportElement */
  function ISVGTextContentElement/getViewportElement () => (arg-result :: 
        <ISVGElement>), name: "getViewportElement", disp-id: 
        as(<machine-word>, #x60050009);

  /* property textLength */
  constant property ISVGTextContentElement/textLength :: 
        <ISVGAnimatedLength>, name: "textLength", disp-id: 215;

  /* property lengthAdjust */
  constant property ISVGTextContentElement/lengthAdjust :: 
        <ISVGAnimatedEnumeration>, name: "lengthAdjust", disp-id: 216;

  /* method getNumberOfChars */
  function ISVGTextContentElement/getNumberOfChars () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getNumberOfChars", 
        disp-id: as(<machine-word>, #x60060002);

  /* method getComputedTextLength */
  function ISVGTextContentElement/getComputedTextLength () => (arg-result 
        :: <single-float>), name: "getComputedTextLength", disp-id: 
        as(<machine-word>, #x60060003);

  /* method getSubStringLength */
  function ISVGTextContentElement/getSubStringLength (arg-charnum :: 
        type-union(<integer>, <machine-word>), arg-nchars :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <single-float>), name: "getSubStringLength", disp-id: 
        as(<machine-word>, #x60060004);

  /* method getStartPositionOfChar */
  function ISVGTextContentElement/getStartPositionOfChar (arg-charnum :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <ISVGPoint>), name: "getStartPositionOfChar", disp-id: 
        as(<machine-word>, #x60060005);

  /* method getEndPositionOfChar */
  function ISVGTextContentElement/getEndPositionOfChar (arg-charnum :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <ISVGPoint>), name: "getEndPositionOfChar", disp-id: 
        as(<machine-word>, #x60060006);

  /* method getExtentOfChar */
  function ISVGTextContentElement/getExtentOfChar (arg-charnum :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <ISVGRect>), name: "getExtentOfChar", disp-id: as(<machine-word>, 
        #x60060007);

  /* method getRotationOfChar */
  function ISVGTextContentElement/getRotationOfChar (arg-charnum :: 
        type-union(<integer>, <machine-word>)) => (arg-result :: 
        <single-float>), name: "getRotationOfChar", disp-id: 
        as(<machine-word>, #x60060008);

  /* method getCharNumAtPosition */
  function ISVGTextContentElement/getCharNumAtPosition (arg-pPoint :: 
        <ISVGPoint>) => (arg-result :: type-union(<integer>, 
        <machine-word>)), name: "getCharNumAtPosition", disp-id: 
        as(<machine-word>, #x60060009);

  /* method selectSubString */
  function ISVGTextContentElement/selectSubString (arg-charnum :: 
        type-union(<integer>, <machine-word>), arg-nchars :: 
        type-union(<integer>, <machine-word>)) => (), name: 
        "selectSubString", disp-id: as(<machine-word>, #x6006000A);

  /* method getTextLength */
  function ISVGTextContentElement/getTextLength () => (arg-result :: 
        <ISVGAnimatedLength>), name: "getTextLength", disp-id: 
        as(<machine-word>, #x6006000B);

  /* method getLengthAdjust */
  function ISVGTextContentElement/getLengthAdjust () => (arg-result :: 
        <ISVGAnimatedEnumeration>), name: "getLengthAdjust", disp-id: 
        as(<machine-word>, #x6006000C);
end dispatch-client <ISVGTextContentElement>;


/* Dispatch interface: ISVGAnimatedLength version 0.0
 * GUID: {DB428520-98B1-11D4-9064-00C04F78ACF9}
 * Description: ISVGAnimatedLength Interface
 */
define dispatch-client <ISVGAnimatedLength>
  uuid "{DB428520-98B1-11D4-9064-00C04F78ACF9}";
end dispatch-client <ISVGAnimatedLength>;


/* Dispatch interface: ISVGAnimatedEnumeration version 0.0
 * GUID: {F212D3C0-98B1-11D4-9064-00C04F78ACF9}
 * Description: ISVGAnimatedEnumeration Interface
 */
define dispatch-client <ISVGAnimatedEnumeration>
  uuid "{F212D3C0-98B1-11D4-9064-00C04F78ACF9}";
end dispatch-client <ISVGAnimatedEnumeration>;


/* COM class: SVGSVGElement version 0.0
 * GUID: {41117E70-9AEF-11D4-9064-00C04F78ACF9}
 * Description: SVGSVGElement Class
 */
define constant $SVGSVGElement-class-id = as(<REFCLSID>, 
        "{41117E70-9AEF-11D4-9064-00C04F78ACF9}");

define function make-SVGSVGElement () => (default-interface :: 
        <ISVGSVGElement>, interface-2 :: <IDOMEventTarget>, interface-3 :: 
        <IDOMNode>, interface-4 :: <IDOMElement>, interface-5 :: 
        <IElement>, interface-6 :: <ISVGElement>, interface-7 :: 
        <IInternalRef>, interface-8 :: <IContainer>)
  let default-interface = make(<ISVGSVGElement>, class-id: 
        $SVGSVGElement-class-id);
  values(default-interface,
         make(<IDOMEventTarget>, disp-interface: default-interface),
         make(<IDOMNode>, disp-interface: default-interface),
         make(<IDOMElement>, disp-interface: default-interface),
         make(<IElement>, disp-interface: default-interface),
         make(<ISVGElement>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface),
         make(<IContainer>, disp-interface: default-interface))
end function make-SVGSVGElement;


/* COM class: CSSStyleDeclaration version 0.0
 * GUID: {50336560-94D4-11D4-9064-00C04F78ACF9}
 * Description: CSSStyleDeclaration Class
 */
define constant $CSSStyleDeclaration-class-id = as(<REFCLSID>, 
        "{50336560-94D4-11D4-9064-00C04F78ACF9}");

define function make-CSSStyleDeclaration () => (default-interface :: 
        <ICSSStyleDeclaration2>, interface-2 :: <ICSSStyleDeclaration>, 
        interface-3 :: <IInternalRef>)
  let default-interface = make(<ICSSStyleDeclaration2>, class-id: 
        $CSSStyleDeclaration-class-id);
  values(default-interface,
         make(<ICSSStyleDeclaration>, disp-interface: default-interface),
         make(<IInternalRef>, disp-interface: default-interface))
end function make-CSSStyleDeclaration;


/* Dispatch interface: ICSSStyleDeclaration2 version 0.0
 * GUID: {AA3BE370-910C-11D4-9062-00C04F78ACF9}
 * Description: ICSSStyleDeclaration2 Interface
 */
define dispatch-client <ICSSStyleDeclaration2>
  uuid "{AA3BE370-910C-11D4-9062-00C04F78ACF9}";

  /* method setProperty */
  function ICSSStyleDeclaration2/setPropertyDeprecated (arg-propertyName :: 
        <string>, arg-newVal :: <string>) => (), name: 
        "setPropertyDeprecated", disp-id: 140;

  /* cssText property */
  property ICSSStyleDeclaration2/cssText :: <string>, name: "cssText", 
        disp-id: as(<machine-word>, #x60030000);

  /* method getPropertyValue */
  function ICSSStyleDeclaration2/getPropertyValue (arg-propertyName :: 
        <string>) => (arg-result :: <string>), name: "getPropertyValue", 
        disp-id: as(<machine-word>, #x60030002);

  /* method getPropertyCSSValue */
  function ICSSStyleDeclaration2/getPropertyCSSValue (arg-propertyName :: 
        <string>) => (arg-result :: <ICSSValue>), name: 
        "getPropertyCSSValue", disp-id: as(<machine-word>, #x60030003);

  /* method removeProperty */
  function ICSSStyleDeclaration2/removeProperty (arg-propertyName :: 
        <string>) => (arg-result :: <string>), name: "removeProperty", 
        disp-id: as(<machine-word>, #x60030004);

  /* method getPropertyPriority */
  function ICSSStyleDeclaration2/getPropertyPriority (arg-propertyName :: 
        <string>) => (arg-result :: <string>), name: "getPropertyPriority", 
        disp-id: as(<machine-word>, #x60030005);

  /* method setProperty with priority */
  function ICSSStyleDeclaration2/setProperty (arg-propertyName :: <string>, 
        arg-newVal :: <string>, arg-priority :: <string>) => (), name: 
        "setProperty", disp-id: 141;

  /* length property */
  constant property ICSSStyleDeclaration2/length :: type-union(<integer>, 
        <machine-word>), name: "length", disp-id: as(<machine-word>, 
        #x60030007);

  /* method item */
  function ICSSStyleDeclaration2/item (arg-index :: type-union(<integer>, 
        <machine-word>)) => (arg-result :: <string>), name: "item", 
        disp-id: as(<machine-word>, #x60030008);

  /* parentRule property */
  constant property ICSSStyleDeclaration2/parentRule :: <ICSSRule>, name: 
        "parentRule", disp-id: as(<machine-word>, #x60030009);

  /* method getCSSText */
  function ICSSStyleDeclaration2/getCssText () => (arg-result :: <string>), 
        name: "getCssText", disp-id: as(<machine-word>, #x6003000A);

  /* method setCSSText */
  function ICSSStyleDeclaration2/setCssText (arg-cssText :: <string>) => 
        (), name: "setCssText", disp-id: as(<machine-word>, #x6003000B);

  /* method getLength */
  function ICSSStyleDeclaration2/getLength () => (arg-result :: 
        type-union(<integer>, <machine-word>)), name: "getLength", disp-id: 
        as(<machine-word>, #x6003000C);

  /* method getParentRule */
  function ICSSStyleDeclaration2/getParentRule () => (arg-result :: 
        <ICSSRule>), name: "getParentRule", disp-id: as(<machine-word>, 
        #x6003000D);
end dispatch-client <ICSSStyleDeclaration2>;


/* Dispatch interface: ICSSValue version 0.0
 * GUID: {EAACE020-8F5B-11D4-9062-00C04F78ACF9}
 * Description: ICSSValue Interface
 */
define dispatch-client <ICSSValue>
  uuid "{EAACE020-8F5B-11D4-9062-00C04F78ACF9}";
end dispatch-client <ICSSValue>;


/* Dispatch interface: ICSSRule version 0.0
 * GUID: {0C5EDD90-8F5C-11D4-9062-00C04F78ACF9}
 * Description: ICSSRule Interface
 */
define dispatch-client <ICSSRule>
  uuid "{0C5EDD90-8F5C-11D4-9062-00C04F78ACF9}";
end dispatch-client <ICSSRule>;


/* Pointer definitions: */
define C-pointer-type <IDOMEventTarget*> => <IDOMEventTarget>; 
        ignorable(<IDOMEventTarget*>);
