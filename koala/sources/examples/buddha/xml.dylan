module: buddha

/*
with-xml-builder()
  html {
    head {
      title("foo")
    },
    body {
      div(id => "foobar",
          class => "narf") {
        a("here", href => "http://www.foo.com"),
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
                                            text: ?value) }
   { ?:name { ?element-list } }
    => { make(<element>, children: list(?element-list), name: ?"name") }
   { ?:name ( ?attribute-list ) { ?element-list } }
    => { make(<element>,
              children: list(?element-list),
              name: ?"name",
              attributes: vector(?attribute-list)) }
   { ?:name ( ?value:expression ) }
    => { make(<element>,
              children: list(make(<char-string>, text: ?value)),
              name: ?"name") }
   { ?:name ( ?value:expression, ?attribute-list ) }
    => { make(<element>, name: ?"name", attributes: vector(?attribute-list)) }
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