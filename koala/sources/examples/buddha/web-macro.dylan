module: web-macro
author: Hannes Mehnert <hannes@mehnert.org>

define class <slot> (<object>)
  constant slot slot-name :: <string>, init-keyword: name:;
  constant slot slot-type :: <object>, init-keyword: type:;
  constant slot slot-getter-method :: <function>, init-keyword: getter:;
end;

define generic list-reference-slots
    (object :: <object>, #next next-method)
 => (res :: <list>);

define generic reference-slots
    (object :: <object>, #next next-method)
 => (res :: <list>);

define generic data-slots
    (object :: <object>, #next next-method)
 => (res :: <list>);

define method list-reference-slots (object :: <object>)
 => (res :: <list>)
  #()
end;

define method reference-slots (object :: <object>)
 => (res :: <list>)
  #()
end;

define method data-slots (object :: <object>)
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
              getter: ?slot-name ## "s"), ... }
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
              type: "<" ## ?#"slot-name" ## ">",
              getter: ?slot-name ## "s"), ... }
    { ?other:*; ... }
    => { ... }
end;

define macro web-data
  { web-data(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { data ?slot-name:name \:: ?slot-type:*; ... }
    => { make(<slot>,
              name: ?"slot-name",
              type: ?slot-type,
              getter: ?slot-name), ... }
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
    { has-many ?slot-name:name }
    => { slot ?slot-name ## "s" :: <list> = #() }
    { has-a ?slot-name:name }
    => { slot ?slot-name :: "<" ## ?slot-name ## ">",
              init-keyword: ?#"slot-name" }
end;

define macro web-class-definer
  { define web-class ?:name (?superclass:*)
      ?class-slots:*
    end }
    => { define-class(?name; ?superclass; ?class-slots);
         define inline method list-reference-slots
             (object :: ?name, #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-lists(?class-slots))
         end;
         define inline method reference-slots
             (object :: ?name, #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-reference(?class-slots));
         end;
         define inline method data-slots
             (object :: ?name, #next next-method)
          => (res :: <list>)
           concatenate(next-method(), web-data(?class-slots));
         end; }
end;
