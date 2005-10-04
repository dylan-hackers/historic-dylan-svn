module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define macro web-lists
  { web-lists(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { has-many ?slot-name:name; ... }
    => { as(<symbol>, ?"slot-name" ## "s"), ... }
    { ?other:*; ... }
    => { ... }
end;

define macro web-reference
  { web-reference(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { has-a ?slot-name:name; ... }
    => { ?#"slot-name", ... }
    { ?other:*; ... }
    => { ... }
end;

define macro web-data
  { web-data(?slots) } => { list(?slots) }

    slots:
    { } => { }
    { data ?slot-name:name ?other:*; ... }
    => { ?#"slot-name", ... }
    { ?other:*; ... }
    => { ... }
end;

define macro define-class
 { define-class(?:name; ?superclass:*; (?slots:*)) }
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
    => { define-class(?name; ?superclass; (?class-slots));
         define inline method list-reference-slots (object :: ?name)
          => (res :: <list>)
           web-lists(?class-slots)
         end;
         define inline method reference-slots (object :: ?name)
          => (res :: <list>)
           web-reference(?class-slots)
         end;
         define inline method data-slots (object :: ?name)
          => (res :: <list>)
           web-data(?class-slots)
         end; }
end;

define web-class <foo> (<object>)
  data namen :: <string>;
  has-many vlan;
  has-many network;
  has-many zone;
  data foo :: <string>;
  has-a config;
  has-a host;
  has-many subnet;
end;

let class = make(<foo>);
format-out("list: %=\n", list-reference-slots(class));
format-out("reference: %=\n", reference-slots(class));
format-out("data: %=\n", data-slots(class));
for (slot in class.object-class.slot-descriptors)
  format-out("slot: %s\n", slot.slot-getter.debug-name)
end;
format-out("class %=\n", class);

/*
define web-class <config> (<object>)
  data name :: <string>;
  has-many vlan;
  has-many network;
  has-many zone;
end;
*/
/*
==>

define class <config> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot vlan :: <list> = #();
  slot network :: <list> = #();
  slot zone :: <list> = #();
end;

define method list-reference-slots (config :: <config>)
  list(#"vlan", #"network", #"zone")
end;

define method reference-slots (config :: <config>)
  list();
end;

define method data-slots (config :: <config>)
  list(#"name")
end;
*/
/*
define web-class <host> (<object>)
  data name :: <string>;
  data ipv4-address :: <ip-address>;
  data mac-address :: <mac-address>;
  has-a subnet;
  has-a zone;
end;
*/
/*
==>

define class <host> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot ipv4-address :: <ip-address>, init-keyword: ip-address:;
  slot mac-address :: <mac-address>, init-keyword: mac-address:;
  slot subnet :: <subnet>, init-keyword: subnet:;
  slot zone :: <zone>, init-keyword: zone:;
end;

define method list-reference-slots (host :: <host>)
  list();
end;

define method reference-slots (host :: <host>)
  list(#"subnet", #"zone")
end;

define method data-slots (host :: <host>)
  list(#"name", #"ipv4-address", #"mac-address");
end;
*/
