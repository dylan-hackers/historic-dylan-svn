Module:    xsd-generator
Synopsis:  Parse xsd to dylan classes
Author:    Hannes Mehnert, Bastian Mueller
Copyright: (C) 2007,.  All rights reserved.

define method main () => ()
  // Your program starts here...
end method main;

define abstract class <xml-stanza> (<object>)
end;

define method encode-xml (object :: <xml-stanza>) => (res :: <element>)
  make(<element>,
       name: class-xml-name(object),
       children: xml-elements(object),
       attributes: xml-attributes(object));
end;

define method encode-xml (object :: <string>) => (res :: <xml>)
  with-xml() text(object) end;
end;


//optional, required!
//check
define macro attribute-list
  { attribute-list(?:name; ?rules:*) }
    => { define method xml-attributes (object :: ?name) => (res :: <collection>)
           let names-and-getters = list(?rules);
           let res = make(<stretchy-vector>);
           for (name-and-getter in names-and-getters)
             if (slot-initialized?(object, name-and-getter.tail))
               add!(res, make(<attribute>,
                              name: name-and-getter.head,
                              value: as(<string>, name-and-getter.tail(object))));
             end;
           end;
           res;
         end }

  rules:
    { } => { }
    { attribute ?:name ?rest:*; ... } => { pair(?"name", ?name), ... }
    { ?rest:*; ... } => { ... }
end;

//one-of
//min-occurences
//sequence -> min-occur = 1!
define macro element-list
  { element-list(?:name; ?rules:*) }
 => { define method xml-elements (object :: ?name) => (res :: <collection>)
        let getters = list(?rules);
        let res = make(<stretchy-vector>);
        for (name-and-getter in getters)
          if (slot-initialized?(object, name-and-getter.tail))
            let obj = name-and-getter.tail(object);
            local method do-add (f :: <object>)
                    if (instance?(f, <string>))
                      add!(res, make(<element>,
                                     name: name-and-getter.head,
                                     children: list(make(<char-string>, text: f))));
                    else
                      add!(res, encode-xml(f))
                    end;
                  end;
            if (instance?(obj, <stretchy-vector>))
              do(do-add, obj)
            else
              do-add(obj);
            end;
          end;
        end;
        res;
      end; }

  rules:
    { } => { }
    { element ?:name ?rest:*; ... } => { pair(?"name", ?name), ... }
    { sequence-of{?sequence-rules:*}; ... } =>  { ?sequence-rules, ... }
    { ?rest:*; ... } => { ... }

  sequence-rules:
    { } => { }
    { one-of{?seq-rules:*}, ?rest:*; ... } => { ?seq-rules, ... }
    { element ?:name ?rest:*; ... } => { pair(?"name", ?name ## "s"), ... }

  seq-rules:
    { } => { }
    { element ?:name ?rest:*; ... } => { pair(?"name", ?name ## "s"), ... }
end;

define abstract class <wrapped-stanza> (<object>)
end;
define macro stanza-definer
  { define ?attributes:name wrapped stanza ?:name ()
      name ?xml-name:expression;
      namespace ?namespace:expression;
      ?body:*
    end }
  => {
    define abstract class ?name (<wrapped-stanza>)
      constant class slot class-xml-name :: <string> = ?xml-name;
    end;
  }

  { define ?attributes:name stanza ?:name ()
      name ?xml-name:expression;
      namespace ?namespace:expression;
      ?rules:*
    end }
  => {
    define-class(?xml-name; ?name; ?rules);
    attribute-list(?name; ?rules);
    element-list(?name; ?rules);
    define-setters(?name; ?rules)
 }
end;

//max-occurs
//check
//one-of
define macro define-real-setter
 { define-real-setter(?object:name) } => { }
 { define-real-setter(?object:name; element ?:name ?rest:*; ?rest2:*) }
 => { define method "add-" ## ?name (obj :: ?object, value :: <object>)
        ?name ## "s"(obj) := add!(?name ## "s"(obj), value)
      end;
      define-real-setter(?object; ?rest2)
    }
 { define-real-setter(?object:name; one-of { ?rest:* }, ?res:*; ?rest2:*) }
 => { define-real-setter(?object; ?rest); define-real-setter(?object; ?rest2) }
end;

define macro define-setters
  { define-setters(?:name; sequence-of{?rest:*} ; ?rules:*) }
  => { define-real-setter(?name; ?rest);
       define-setters(?name; ?rules) }
  { define-setters(?:name; ?rest:*; ?rules:*) }
  => { define-setters(?name; ?rules) }
  { define-setters(?:name) } => { }
end;

define macro define-class
  { define-class(?xml-name:expression; ?:name; ?rules:*) }
  => { define class ?name (<xml-stanza>)
         constant class slot class-xml-name :: <string> = ?xml-name;
         ?rules
       end; }

  rules:
    { } => { }
    { ?rule:*; ... } => { ?rule ; ... }

  rule:
    { sequence-of{?sequence-rules:*} } =>  { ?sequence-rules }
    { one-of{?rules:*}, ?rest:* } => { ?rules }
    { element ?:name :: wrapped(?type:expression, ?real-type:expression) ?rest:* } => { slot ?name :: ?real-type }
    { element ?:name :: ?type:expression, ?rest:* } => { slot ?name :: ?type }
    { attribute ?:name :: ?type:expression = ?default:expression, ?rest:* } => { slot ?name :: ?type = ?default }
    { attribute ?:name :: ?type:expression, ?rest:* } => { slot ?name :: ?type }
    { content :: ?type:expression } => { slot %content :: ?type }

  sequence-rules:
    { } => { }
    { ?sequence-rule:*; ... } => { ?sequence-rule ; ... }

  sequence-rule:
    { one-of{?sequence-rules:*} ?rest:* } => { ?sequence-rules }
    { element ?:name ?rest:* } => { slot ?name ## "s" :: <stretchy-vector> = make(<stretchy-vector>) }

end;
/*
make(<client-message>, body: "foo", subject: "bar", from: "fnord");
<message><body>foo</body><thread>23</thread></message>
<message><foo/><bar/><foobar/><foo/></message>

element.seq1-one1-subject
<messsage><subject>foo</subject><subject>fnord</subject><subject>barf</subject></message>
*/
define constant <nmtoken> = <string>;
define qualified stanza client-message ()
  name "message";
  namespace "jabber:client";
  sequence-of { 
    one-of { 
      element subject :: wrapped(<client-subject>, <string>);
      element body :: wrapped(<client-body>, <string>);
      element thread :: wrapped(<client-thread>, <string>);
    }, min-occurrences: 0, max-occurrences: unbounded;
    element error :: wrapped(<client-error>, <string>);
  };
  repeated element foobar :: <string>, min-occurs: 23;
  attribute from :: <string> = "dddd";
  attribute id :: <nmtoken> = "2342";
  attribute to :: <string> = "foo@bar.ocm";
  attribute type :: one-of(#"chat", #"error", #"groupchat", #"headline", #"normal") = #"normal";
  attribute xml-lang :: <string>;
end;




begin
  let msg = make(client-message);
  add-subject(msg, "foo");
  
end;
