Module:    interface
Synopsis:  Some macros to simplify creation of XML trees.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.


define constant $default-xml-parent = make(<document>, name: "default-parent");

define macro make-xml-element-children
  { make-xml-element-children (?result:expression) end }
  => { ?result }
  { make-xml-element-children (?accu:expression)
      attribute ?:name = ?value:expression;
      ?more:*
    end }
  => { make-xml-element-children (?accu)
         ?more
       end }
  { make-xml-element-children (?accu:expression)
      content ?value:expression;
      ?more:*
    end }
  => { make-xml-element-children (pair(?value, ?accu))
         ?more
       end }
  { make-xml-element-children (?accu:expression)
      element ?:name = ?subelement:expression;
      ?more:*
    end }
  => { make-xml-element-children
           (pair(make-xml-element(?name) content ?subelement end, ?accu))
         ?more
       end }
  { make-xml-element-children (?accu:expression)
      element (?:name) { ?subelements:* }
      ?more:*
    end }
  => { make-xml-element-children
           (pair(make-xml-element(?name) ?subelements end, ?accu))
         ?more
       end }
end macro make-xml-element-children;

define macro make-xml-element-attributes
  { make-xml-element-attributes (?result:expression) end }
  => { ?result }
  { make-xml-element-attributes (?accu:expression)
      attribute ?:name = ?value:expression;
      ?more:*
    end }
  => { make-xml-element-attributes
           (pair(make(<attribute>, 
                      name: ?"name", 
                      value: if (instance?(?value, <string>))
                               ?value
                             else
                               format-to-string("%=", ?value)
                             end),
                 ?accu))
         ?more
       end }
  { make-xml-element-attributes (?accu:expression)
      content ?:expression;
      ?more:*
    end }
  => { make-xml-element-attributes (?accu)
         ?more
       end }
  { make-xml-element-attributes (?accu:expression)
      element ?:name = ?subelement:expression;
      ?more:*
    end }
  => { make-xml-element-attributes (?accu)
         ?more
       end }
  { make-xml-element-attributes (?accu:expression)
      element (?:name) { ?subelements:* }
      ?more:*
    end }
  => { make-xml-element-attributes (?accu)
         ?more
       end }
end macro make-xml-element-attributes;

define macro make-xml-element
  { make-xml-element (?:name) ?content:* end }
  => { make(<element>, 
            name: ?"name",
            parent: $default-xml-parent,
            children: as(<stretchy-object-vector>,
                         make-xml-element-children (#()) ?content end),
            attributes: as(<stretchy-object-vector>,
                           make-xml-element-attributes (#()) ?content end)) }
end macro make-xml-element;
/*
define variable *elt* = 
  make-xml-element (foo)
    attribute a1 = 123;
    attribute a2 = 234;
    element (bar) {
      attribute b1 = "x";
      attribute b2 = 2;
      // content #(1, 2, 3);
      element (sub) {
        attribute x = 7;
        // content #"asdf";
      }
      attribute b3 = 7;
    }
    // content "x";
    // content #"y";
    attribute a3 = 123.3;
  end make-xml-element;

format-out("%S\n%=\n", *elt*, *elt*);
*/

