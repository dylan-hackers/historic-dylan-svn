module: web-framework-macro
author: Hannes Mehnert <hannes@mehnert.org>

define class <web-form-warning> (<condition>)
  constant slot error-string :: <string>, required-init-keyword: warning:;
end;

define class <web-success> (<web-form-warning>)
end;

define class <web-error> (<error>)
  constant slot error-string :: <string>, required-init-keyword: error:;
end;

define open generic check (object :: <object>, #key test-result)
 => (res :: <boolean>);

define method show (object :: <string>)
  object
end;

define method show (object :: <integer>)
  integer-to-string(object)
end;

define method show (object :: <boolean>)
  if (object)
    "true"
  else
    "false"
  end
end;

define method show (object :: <object>)
  as(<string>, object)
end;

define method get-url-from-type (type) => (string :: <string>)
  copy-sequence(type.debug-name,
                start: 1,
                end: type.debug-name.size - 1)
end;

define class <triple> (<object>)
  constant slot old-value, init-keyword: old-value:;
  constant slot new-value, init-keyword: new-value:;
  constant slot slot-name, init-keyword: slot-name:;
end;

define generic visible? (obj :: <object>) => (res :: <boolean>);

define method  visible? (obj :: <object>) => (res :: <boolean>);
  #t;
end;
define open class <reference-object> (<object>)
  slot visible? :: <boolean> = #t, init-keyword: visible?:;
end;

define class <slot> (<object>)
  constant slot slot-name :: <string>, init-keyword: name:;
  constant slot slot-type :: <object>, init-keyword: type:;
  constant slot slot-getter-method :: <function>, init-keyword: getter:;
  constant slot slot-setter-method :: <function>, init-keyword: setter:;
  constant slot default :: <object> = #f, init-keyword: default:;
  constant slot default-function :: <function> = method(x :: <object>) #f end,
    init-keyword: default-function:;
  constant slot default-help-text :: false-or(<string>) = #f,
    init-keyword: default-help-text:;
end;

define open generic list-reference-slots
    (object :: subclass(<object>), #next next-method)
 => (res :: <list>);

define open generic reference-slots
    (object :: subclass(<object>), #next next-method)
 => (res :: <list>);

define open generic data-slots
    (object :: subclass(<object>), #next next-method)
 => (res :: <list>);

define method list-reference-slots (object :: subclass(<object>))
 => (res :: <list>)
  #()
end;

define method reference-slots (object :: subclass(<object>))
 => (res :: <list>)
  #()
end;

define method data-slots (object :: subclass(<object>))
 => (res :: <list>)
  #()
end;

define macro web-lists
  { web-lists(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { has-many ?slot-name:name; ... }
    => { make(<slot>,
              name: ?"slot-name" ## "s",
              type: "<" ## ?slot-name ## ">",
              getter: ?slot-name ## "s",
              setter: ?slot-name ## "s-setter"), ... }
    { has-many ?slot-name:name \:: ?slot-type:*; ... }
    => { make(<slot>,
              name: ?"slot-name" ## "s",
              type: ?slot-type,
              getter: ?slot-name ## "s",
              setter: ?slot-name ## "s-setter"), ... }
    { ?other:*; ... }
    => { ... }
end;

define macro web-reference
  { web-reference(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { has-a ?slot-name:name; ... }
    => { make(<slot>,
              name: ?"slot-name",
              type: "<" ## ?slot-name ## ">",
              getter: ?slot-name,
              setter: ?slot-name ## "-setter"), ... }
    { ?other:*; ... }
    => { ... }
end;

define macro web-data
  { web-data(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { data ?slot-name:name \:: ?slot-type:name; ... }
    => { make(<slot>,
              name: ?"slot-name",
              type: ?slot-type,
              getter: ?slot-name,
              setter: ?slot-name ## "-setter"), ... }
    { data ?slot-name:name \:: ?slot-type:name, ?default-function:expression ; ... }
    => { make(<slot>,
              name: ?"slot-name",
              type: ?slot-type,
              getter: ?slot-name,
              setter: ?slot-name ## "-setter",
              default-help-text: ?"default-function",
              default-function: method (?=object :: <object>) ?default-function end), ... }
    { data ?slot-name:name \:: ?slot-type:name = ?default:expression; ... }
    => { make(<slot>,
              name: ?"slot-name",
              type: ?slot-type,
              getter: ?slot-name,
              setter: ?slot-name ## "-setter",
              default: ?default), ... }
    { ?other:*; ... }
    => { ... }
end;

define macro define-class
 { define-class(?:name; ?superclass:*; ?slots:*) }
    => { define class ?name (?superclass) ?slots end }

    slots:
    { } => { }
    { ?slot:*; ... } => { ?slot ; ... }
    
    slot:
    { data ?slot-name:name \:: ?slot-type:* }
    => { slot ?slot-name :: ?slot-type, init-keyword: ?#"slot-name" }
    { slot ?slot-name:name \:: ?slot-type:* }
    => { slot ?slot-name :: ?slot-type, init-keyword: ?#"slot-name" }
    { has-many ?slot-name:name }
    => { slot ?slot-name ## "s" :: <stretchy-vector> = make(<stretchy-vector>) }
    { has-many ?slot-name:name \:: ?slot-type:* }
    => { slot ?slot-name ## "s" :: <stretchy-vector> = make(<stretchy-vector>) }
    { has-a ?slot-name:name }
    => { slot ?slot-name /* :: "<" ## ?slot-name ## ">" */,
              init-keyword: ?#"slot-name" }
end;

define macro web-class-definer
  { define web-class ?:name (?superclass:*)
      ?class-slots:*
    end }
    => { define-class(?name; ?superclass; ?class-slots);
         define inline method list-reference-slots
             (object :: subclass(?name), #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-lists(?class-slots))
         end;
         define inline method reference-slots
             (object :: subclass(?name), #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-reference(?class-slots));
         end;
         define inline method data-slots
             (object :: subclass(?name), #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-data(?class-slots));
         end; }
end;
