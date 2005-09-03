module: buddha

/*
TODO: do, collect, comments
with-xml-builder()
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
      res := add!(res, '&');
      res := add!(res, 'g');
      res := add!(res, 't');
      res := add!(res, ';');
    elseif (char = '<')
      res := add!(res, '&');
      res := add!(res, 'l');
      res := add!(res, 't');
      res := add!(res, ';');
    elseif (char = '&')
      res := add!(res, '&');
      res := add!(res, 'a');
      res := add!(res, 'm');
      res := add!(res, 'p');
      res := add!(res, ';');
    else
      res := add!(res, char)
    end;
  end;
  res;
end;

define macro with-xml-builder
  { with-xml-builder ()
      ?element
    end }
   => { begin
          let doc = make(<document>,
                         children: list(?element));
          transform-document(doc, state: make(<add-parents>));
          doc;
        end; }

  element:
   { ?:name } => { make(<element>, name: ?"name") }
   { text ( ?value:expression ) } => { make(<char-string>,
                                            text: escape-html(?value)) }
   { ?:name { ?element-list } }
    => { make(<element>, children: list(?element-list), name: ?"name") }
   { ?:name ( ?attribute-list ) { ?element-list } }
    => { make(<element>,
              children: list(?element-list),
              name: ?"name",
              attributes: vector(?attribute-list)) }
   { ?:name ( ?value:expression ) }
    => { make(<element>,
              children: list(make(<char-string>, text: escape-html(?value))),
              name: ?"name") }
   { ?:name ( ?value:expression, ?attribute-list ) }
    => { make(<element>,
              children: list(make(<char-string>, text: escape-html(?value))),
              name: ?"name",
              attributes: vector(?attribute-list)) }
   { ?:name ( ?attribute-list ) }
    => { make(<element>,
              name: ?"name",
              attributes: vector(?attribute-list)) }
   //{ comment ( ?value:expression ) } =>  { make-comment(?value) }

  element-list:
   { } => { }
   { ?element, ... } => { ?element, ... }

  attribute-list:
   { ?key:expression => ?value:expression }
                 => { make(<attribute>, name: ?"key", value: ?value) }
   { ?key:expression => ?value:expression, ... }
                 => { make(<attribute>, name: ?"key", value: ?value), ... }

end;
