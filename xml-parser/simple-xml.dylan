module: simple-xml
author: Hannes Mehnert <hannes@mehnert.org>

/*
BUGS:
*with-xml:collect only works for elements, not for lists of elements
*passing around lists and elements is not the way to do it performant...
*comment elements are missing
*the following doesn't work (reference to undefined binding "collect" (but
 collect is defined unhygienic in with-xml macro, do-clause, any ideas?
 bug in functional-developer?)
 define macro add-form-helper
   { add-form-helper(?type:name) end }
     => { define method add-form (type == ?#"type")
            with-xml()
              form (action => "/edit", \method => "post")
              {
                div (class => "edit")
                {
                  do(for(slot in ?type.slot-descriptors)
                       let name = slot.slot-getter.debug-name;
                       collect(with-xml()
                                 text(name)
                               end);
                       collect(with-xml()
                                 input(type => "text",
                                       name => name)
                               end);
                       collect(with-xml() br end);
                     end;
                  input(type => "submit",
                        name => "add-button",
                        value => "Add")
                }
              }
            end;
          end; }
 end;


USAGE
=====

with-xml()
  html {
    head {
      title("foo")
    },
    body {
      div(id => "foobar",
          class => "narf") {
        a("here", href => "http://www.foo.com"),
        a(href => "http://www.ccc.de/"),
        text("foobar"),
        ul {
          li("foo"),
          br,
          li("bar"),
          br
        }
      }
    }
  }
end;

generates:

<html>
  <head>
    <title>foo</title>
  </head>
  <body>
    <div id="foobar" class="narf">
      <a href="http://www.foo.com">here</a>
      <a href="http://www.ccc.de/"/>
      foobar
      <ul>
        <li>foo</li>
        <br/>
        <li>bar</li>
        <br/>
      </ul>
    </div>
  </body>
</html>
*/

define method escape-xml (symbol :: <symbol>) => (res :: <symbol>)
  as(<symbol>, escape-xml(as(<string>, symbol)))
end;

define method escape-xml (string :: <string>) => (res :: <string>)
  let res = "";
  for (char in string)
    if (char = '>')
      res := concatenate(res, "&gt;");
    elseif (char = '<')
      res := concatenate(res, "&lt;");
    elseif (char = '&')
      res := concatenate(res, "&amp;");
    else
      res := add!(res, char)
    end;
  end;
  res;
end;

define macro with-xml-builder
  { with-xml-builder ()
      ?body:*
    end }
   => { begin
          let doc = make(<document>,
                         children: list(with-xml() ?body end));
          transform-document(doc, state: make(<add-parents>));
          doc;
        end; }
end;

define macro with-xml
  { with-xml () ?element end }
   => { begin
          ?element[0]
        end; }

  element:
   { ?:name } => { list(make(<element>, name: ?"name")) }
   { text ( ?value:expression ) } => { list(make(<char-string>,
                                                 text: escape-xml(?value))) }
   { !attribute(?attribute) }
    => { list(?attribute) }
   { do(?:body) }
    => { begin
           let res = make(<stretchy-vector>);
           local method ?=collect(element)
                   res := add!(res, element)
                 end;
           let body-res = ?body;
           if (res.size > 0)
             res;
           elseif (body-res)
             if (instance?(body-res, <sequence>))
               body-res;
             else
               list(body-res);
             end;
           else
             make(<list>)
           end;
         end }
   { ?:name { ?element-list } }
    => { list(make(<element>,
                   children: concatenate(?element-list),
                   name: ?"name")) }
   { ?:name ( ?attribute-list ) { ?element-list } }
    => { list(make(<element>,
                   children: concatenate(?element-list),
                   name: ?"name",
                   attributes: vector(?attribute-list))) }
   { ?:name ( ?value:expression ) }
    => { list(make(<element>,
                   children: list(make(<char-string>,
                                       text: escape-xml(?value))),
                   name: ?"name")) }
   { ?:name ( ?value:expression, ?attribute-list ) }
    => { list(make(<element>,
                   children: list(make(<char-string>,
                                        text: escape-xml(?value))),
                   name: ?"name",
                   attributes: vector(?attribute-list))) }
   { ?:name ( ?attribute-list ) }
    => { list(make(<element>,
                   name: ?"name",
                   attributes: vector(?attribute-list))) }
   //{ comment ( ?value:expression ) } =>  { make-comment(?value) }

  element-list:
   { } => { }
   { ?element, ... } => { ?element, ... }

  attribute-list:
   { } => { }
   { ?attribute, ... } => { ?attribute, ... }

  attribute:
   { ?key:name => ?value:expression }
    => { make(<attribute>, 
          name: ?"key",
          value: ?value) }
   { ?ns:name :: ?key:name => ?value:expression }
    => { make(<attribute>, 
          name: concatenate(?"ns" ## ":", ?"key"),
          value: ?value) }
end;

define method add-attribute (element :: <element>, attribute :: <attribute>) 
 => (res :: <element>)
  // prevent equal attribute names
  let existing-attribute = find-key(element.attributes, method (a :: <attribute>) 
                                                          a.name = attribute.name
                                                        end);
  if (existing-attribute)
    aref(element.attributes, existing-attribute) := attribute;
  else
    element.attributes := add(element.attributes, attribute);
  end if;
  element;
end method add-attribute;

define method remove-attribute (element :: <element>, attribute)
  element.attributes := remove(element.attributes, attribute, 
                          test: method (a :: <attribute>, b) 
                            a.name = as(<symbol>, b);
                          end);
end method remove-attribute;

define method attribute (element :: <element>, attribute-name)
   => (res :: false-or(<attribute>));
  let pos = find-key(element.attributes, method (a)
                                          a.name = as(<symbol>, attribute-name);
                                        end);
  if (pos)
    aref(element.attributes, pos);
  else
    #f;
  end if;
end method;

define open generic elements (element :: <element>, name :: <object>) => (res :: <sequence>);
define method elements (element :: <element>, element-name) 
 => (res :: <sequence>);
  choose(method (a)
            a.name = as(<symbol>, element-name)
          end, element.node-children);
end method elements;

define open generic add-element (element :: <element>, node :: <xml>);
define method add-element (element :: <element>, node :: <xml>) 
 => (res :: <element>);
  element.node-children := add(element.node-children, node);
  if (object-class(node) = <element>)
    node.element-parent := element;
  end if;
  element;
end method add-element;

define method remove-element (element :: <element>, node-name, #key count: element-count)
 => (res :: <element>);
  element.node-children := remove(element.node-children, node-name, count: element-count,
                          test: method (a :: <element>, b)            
                                  a.name = as(<symbol>, b);
                                end);
  element;
end method remove-element;
     
define open generic import-element (element :: <element>, node :: <element>);
define method import-element (element :: <element>, node :: <element>)
  for (child in node.node-children)
    add-element(element, child);
  end for;
  for (attribute in node.attributes)
    add-attribute(element, attribute);
  end for;
end method import-element;

define generic prefix (object :: <object>) => (res :: <string>);
define method prefix (element :: <element>)
 => (res :: <string>);
  prefix(element.name);
end method prefix;
  
define method prefix (name :: type-union(<string>, <symbol>))
  => (res :: <string>);
  split(as(<string>, name), ':')[0];
end method prefix;

define method prefix-setter (prefix :: <string>, element :: <element>) 
  if (~member?(':', as(<string>, element.name)))
    element.name := as(<symbol>, concatenate(prefix, ":", as(<string>, element.name)));
  end if;
  element;
end;

define generic real-name (object :: <object>) => (res :: <string>);
define method real-name (element :: <element>) 
 => (res :: <string>);
  real-name(element.name);
end method real-name;

define method real-name (name :: type-union(<string>, <symbol>))
 => (res :: <string>);
  split(as(<string>, name), ':')[1];
end method real-name;

define method namespace (element :: <element>)
 => (xmlns :: false-or(<string>));
  let xmlns = attribute(element, "xmlns");
  if (xmlns)
    xmlns.attribute-value;
  else
    #f;
  end if;
end method namespace;

define method add-namespace (element :: <element>, ns :: <string>)
  => (res :: <element>);
  add-attribute(element, make(<attribute>, name: "xmlns", value: ns));
  element;
end method add-namespace;

define method remove-namespace (element :: <element>)
 => (res :: <element>);
  remove-attribute(element, "xmlns");
  element;
end method remove-namespace;

define method replace-element-text (element :: <element>, node :: <string>, text :: <string>)
  let replace-element = #f;
  let replace-elements = elements(element, node);
  if (empty?(replace-elements))
    replace-element := make(<element>, name: node);
    add-element(element, replace-element);
  else
    replace-element := first(replace-elements);
  end if;
  replace-element.text := escape-xml(text);
end method replace-element-text;

define method start-tag (e :: <element>)
 => (tag :: <string>);
  let stream = make(<string-stream>, direction: #"output");
  print-opening(e, *printer-state*, stream);
  print-attributes(e.attributes, *printer-state*, stream);
  print-closing("", stream);
  stream-contents(stream);
end method start-tag; 
