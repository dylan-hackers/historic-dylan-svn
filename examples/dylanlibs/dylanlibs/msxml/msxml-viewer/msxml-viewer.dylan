Module:    msxml-viewer
Synopsis:  An XML file viewer using the MSXML libraries.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define frame <msxml-viewer> (<simple-frame>)
  slot xml-document = #f;

  pane load-button (frame)
    make(<push-button>, label: "Load...", activate-callback: on-load);

  pane xpath-button (frame)
    make(<push-button>, label: "XPath...", activate-callback: on-xpath);

  pane xpath-pane (frame)
    make(<text-field>);

  pane xml-pane (frame)
    make(<text-editor>);

  pane display-pane (frame)
    make(<tree-control>,
         roots: #[],
         value-changed-callback: on-value-changed,
         label-key: node-label-generator,
         children-predicate: method(x) ~x.node-child-generator.empty? end,
         children-generator: node-child-generator);

  layout (frame)
    vertically()
      frame.load-button;
      horizontally()
        frame.xpath-pane;
        frame.xpath-button;
      end;
      frame.display-pane;
      frame.xml-pane;
    end;

  keyword width: = 300;
  keyword height: = 400;
  keyword title: = "XML Viewer";
end frame <msxml-viewer>;

define method on-load(g)
  let file = choose-file();

  when(file)
    let data =
      with-open-file(fs = file)
        fs.stream-contents
      end; 

    let document = make-document();
    document.document-async? := #f;
    if(document-load-xml(document, data))
      g.sheet-frame.xml-document := document;
      g.sheet-frame.display-pane.tree-control-roots := vector(document);
    else
      notify-user("Could not load XML document.");
    end if; 
  end when; 
end method on-load;

define method on-xpath(g)
  let frame = g.sheet-frame;
  if(frame.xml-document)
    let xpath = frame.xpath-pane.gadget-value;
    if(xpath.empty?)
      g.sheet-frame.display-pane.tree-control-roots := vector(frame.xml-document);
    else      
      let nodes = node-select-nodes(frame.xml-document, xpath);
      g.sheet-frame.display-pane.tree-control-roots := nodes;
    end if;
  else
    notify-user("No document loaded.");
  end if;
end method on-xpath;

define method on-value-changed(g)
  let item = g.gadget-value;
  g.sheet-frame.xml-pane.gadget-value := node-info(item);
end method on-value-changed;

define method node-label-generator(node)
  "Unknown Node"
end method node-label-generator;

define method node-label-generator( node :: <document> )
  "XML Document"  
end method node-label-generator;

define method node-label-generator( node :: <element> )
  concatenate("Element (", node.element-tag-name, ")");
end method node-label-generator;

define method node-label-generator( node :: <node> )
  concatenate("Node of type ", node.node-name, ")");
end method node-label-generator;

define method node-label-generator( node :: <text> )
  "Text"  
end method node-label-generator;

define method node-label-generator( node :: <string> )
  node  
end method node-label-generator;

define method node-child-generator(node)
  #[]
end method node-child-generator;

define method node-child-generator( node :: <document> )
  vector(node.document-element);  
end method node-child-generator;

define method node-child-generator( node :: <node> )
  node.node-child-nodes;  
end method node-child-generator;

define method node-child-generator( node :: <text> )
  vector(node.node-value);  
end method node-child-generator;

define method node-child-generator( node :: <string> )
  #[];  
end method node-child-generator;

define method node-info(node)
  "No Information Available"
end method node-info;

define method node-info( node :: <node>)
  node.node-xml
end method node-info;

define method main () => ()
  start-frame(make(<msxml-viewer>));
end method main;

begin
  msxml3-initialize();
  main();
end;
