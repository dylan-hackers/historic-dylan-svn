module: xml
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

define method escape-html (string :: <string>) => (res :: <string>)
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
                                                 text: escape-html(?value))) }
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
                                       text: escape-html(?value))),
                   name: ?"name")) }
   { ?:name ( ?value:expression, ?attribute-list ) }
    => { list(make(<element>,
                   children: list(make(<char-string>,
                                       text: escape-html(?value))),
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
   { ?key:name => ?value:expression }
                 => { make(<attribute>, name: ?"key", value: ?value) }
   { ?key:name => ?value:expression, ... }
                 => { make(<attribute>, name: ?"key", value: ?value), ... }

end;
