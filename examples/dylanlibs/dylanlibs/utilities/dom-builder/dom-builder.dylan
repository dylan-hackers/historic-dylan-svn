Module:    dom-builder
Synopsis:  Utilities for generating Document Object Models for XML/HTML documents.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The with-dom-builder macro allows generating document object models
// using a lisp-style s-expression format. It is loosly based on
// HTMLGEN, the HTML generator from AllegroServe, a Common Lisp web server.
//
// Examples of use are:
//   with-dom-builder ()
//     (html
//       (head (title ["Test Page"])),
//       (body
//         (p ["The sum of 1 and 2 is "], [1 + 2])))
//
// Attributes can be included as well:
//
//   with-dom-builder ()
//       (html
//         (head (title ["Test Page"])),
//         (body
//           (p ["The sum of 1 and 2 is "], [1 + 2]),
//           (p 
//              ["Test this link: "],
//              ((a href: "http://www.double.co.nz") ["My website"]))))
//
// Nested invocations and using the macro with variables works fine. The
// following example is adapted from an AllegroServe HTMLGEN example:
//
// define method simple-table(count, border-width, backg-color, border-color)
//  with-dom-builder()
//    (html
//      (head (title ["Test Table"])),
//      (body
//        ((table border: border-width,
//                bordercolor: border-color,
//                bgcolor: backg-color,
//                cellpadding: 3)
//          (tr ((td bgcolor: "blue")
//               ((font color: "white", size: "+1")
//                ["Value"])),
//              ((td bgcolor: "blue")
//                ((font color: "white", size: "+1")
//                 ["Square"]))),
//          [for(n from 0 below count)
//             with-dom-builder(*current-dom-element*)
//               (tr (td [n]), (td [n * n]))             
//             end;
//           end for])))
//  end with-dom-builder;
// end method simple-table;   
//
//  with-open-file(fs = "d:\\test-table.html", direction: #"output")
//    print-html(simple-table(10, 3, "silver", "blue"), fs);
//  end;
//
// Basic Syntax
// ============
//
// XML tags are the first element of the s-expression. Immediately
// following this are the child elements and/or text elements that
// belong to that element. These can be nested indefinitely. Any 
// Dylan computation can be performed and the results of this computation
// will be included in the generated DOM as long as it is enclosed in 
// square brackets ('[' and ']'). Constant strings can be included as
// long as they too are enclosed in square brackets.
//
// Attributes of an element can be included. If an element has attributes
// then it should be enclosed in parenthesis as the first element of the
// s-expression with the attributes following it as keyword, argument pairs.
//
// Sibling element nodes should be seperated from each other with the comma
// seperator.
//
// Nested with-dom-builder macros can be done. If the nested macro DOM is
// required to be integrated with the outer macro DOM then the variable
// *current-dom-element* should be passed as an argument to the macro. This
// variable represents the parent element that the DOM of the inner macro
// will be integrated with.
//

// Contains the current element being processed so children
// know who there parent is when being added and allows
// nested with-dom-builder macros to work correctly.
define thread variable *current-dom-element* :: false-or(<element>) = #f;

define macro with-dom-builder
{ with-dom-builder ()
    ?single-form
  end }
 => { begin 
        dynamic-bind(*current-dom-element* = make(<document>))
          ?single-form;
          *current-dom-element*;
        end;	    
      end }

{ with-dom-builder (?parent:expression) 
    ?single-form
  end }
 => { begin 
        dynamic-bind(*current-dom-element* = ?parent)
          ?single-form;
          *current-dom-element*;
        end;	    
      end }

single-form:
  { } => { }
  { [ ?body:* ], ... } 
    => { begin 
           let result = begin ?body end;
           when(result)
             add-child(*current-dom-element*, 
                         make(<text-element>, 
                              text: format-to-string("%s", (result))));
           end when;
         end; ... }

  { (?tag-name:name), ... } 
    => { begin
           add-child(*current-dom-element*, 
                       make(<node-element>, 
                            tag: ?#"tag-name"));
         end; ... }

  { ((?tag-name:name ?attributes:*) ?multiple-forms:*), ... } 
   => { begin
          let element = add-child(*current-dom-element*, 
                                    make(<node-element>, 
                                         tag: ?#"tag-name"));
          dynamic-bind(*current-dom-element* = element)
            ?attributes;
            ?multiple-forms;
          end dynamic-bind;
        end; ... }

  { (?tag-name:name ?multiple-forms:*), ... } 
    => { begin
           let element = add-child(*current-dom-element*, 
                                     make(<node-element>, 
                                           tag: ?#"tag-name"));
           dynamic-bind(*current-dom-element* = element)
             ?multiple-forms;
           end dynamic-bind;
         end; ... }

multiple-forms:
  { } => { }
  { [ ?:body ], ... } 
    => { begin 
           let result = begin ?body end;
           when(result)
             add-child(*current-dom-element*, 
                         make(<text-element>, 
                              text: format-to-string("%s", result)));
           end when;
         end; ... }

  { (?tag-name:name), ... } 
    => { begin
           add-child(*current-dom-element*, 
                       make(<node-element>, 
                            tag: ?#"tag-name"));
         end }

  { ?single-form, ... } => { ?single-form; ... }

attributes:
  { } => { }
  { ?:symbol, ?:expression, ... } 
    => { add-attribute(*current-dom-element*, 
                       make(<attribute>, 
                            key: ?symbol, 
                            value: format-to-string("%s", ?expression))); ... }
  { ?:symbol ?:expression, ... }
    => { add-attribute(*current-dom-element*, 
                       make(<attribute>, 
                            key: ?symbol, 
                            value: format-to-string("%s", ?expression))); ... }
end macro with-dom-builder;


