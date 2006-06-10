module: xmpp
synopsis: 
author: 
copyright: 

define macro xmpp-element-definer
  { define xmpp-element ?:name (?superclasses:*)
      ?slots:*
    end }
   => { 
        define-xmpp-element-class(?name; ?name; ?superclasses; ?slots);
        define-xmpp-element-initializer(?name; ?slots);
        define-xmpp-element-generic-method(?slots);
        define-xmpp-element-getter-setter("<" ## ?name ## ">"; ?slots);
      }

  { define abstract xmpp-element ?:name (?superclasses:*)
      ?slots:*
    end }
   => {
        define-xmpp-element-class(?name; ?name; ?superclasses; ?slots);
        define-xmpp-element-initializer(?name; ?slots);
        define-xmpp-element-generic-method(?slots);
        define-xmpp-element-getter-setter("<" ## ?name ## ">"; ?slots);
      }

  { define xmpp-element ?:name = ?real-name:name (?superclasses:*)
      ?slots:*
    end }
   => { 
        define-xmpp-element-class(?name; ?real-name; ?superclasses; ?slots);
        define-xmpp-element-initializer(?name; ?slots);
        define-xmpp-element-generic-method(?slots);
        define-xmpp-element-getter-setter("<" ## ?name ## ">"; ?slots);
      }

  superclasses:
      { } => { <xmpp-element> }
      { ?superclass:*, ... } => { "<" ## ?superclass ## ">" , ... }
end macro xmpp-element-definer;

define macro define-xmpp-element-class
  { define-xmpp-element-class(?:name; ?real-name:name; ?superclasses:*; ?slots:*) }
   => { define class "<" ## ?name ## ">" (?superclasses) 
          inherited slot name-with-proper-capitalization = ?"real-name"; 
          ?slots
        end }
  { define-xmpp-element-class(?:name; none; ?superclasses:*; ?slots:*) }
   => { define class "<" ## ?name ## ">" (?superclasses)
          ?slots
        end }

  slots:
    { } => { }
    { ?slot:*; ... } => { ?slot ; ... }

  slot:
    { } => { }
    { ?foo:* ?slot-name:name \:: ?slot-type:* } => { virtual slot ?slot-name }
    { ?foo:* ?slot-name:name alias ?alias-name:expression \:: ?slot-type:* } => { virtual slot ?slot-name }

end macro define-xmpp-element-class;

define macro define-xmpp-element-initialize-setter
  { define-xmpp-element-initialize-setter(?:name; ?foo:* ?slot-name:name \:: ?slot-type:*; ?rest:* ) }
   => {
        if (?slot-name)
          ?slot-name ## "-setter" (?slot-name, ?name);
        end if;
        define-xmpp-element-initialize-setter(?name, ?rest)
      }
  { define-xmpp-element-initialize-setter(?:name; ?foo:* ?slot-name:name alias ?alias-name:expression \:: ?slot-type:*; ?rest:* ) }
   => {
        if (?slot-name)
          ?slot-name ## "-setter" (?slot-name, ?name);
        end if;
        define-xmpp-element-initialize-setter(?name, ?rest)
      }

end macro define-xmpp-element-initialize-setter;

define macro define-xmpp-element-initializer
  { define-xmpp-element-initializer(?:name; ?slots:*) }
   => {
        define method initialize (?name :: "<" ## ?name ## ">", #key define-xmpp-element-initializer-keywords(?slots) , #all-keys )
          define-xmpp-element-initialize-setter(?name, ?slots);
        end method
      }
end macro define-xmpp-element-initializer;

define macro define-xmpp-element-initializer-keywords
    { define-xmpp-element-initializer-keywords(?slots:*) } => { ?slots }

    slots:
      { } => { }
      { ?slot:*; ... } => { ?slot, ... }

    slot:
      { ?foo:* ?slot-name:name \:: ?slot-type:* } => { ?slot-name }
      { ?foo:* ?slot-name:name alias ?alias-name:expression \:: ?slot-type:* } => { ?slot-name }
end macro define-xmpp-element-initializer-keywords;

define macro define-xmpp-element-generic-method
  { define-xmpp-element-generic-method(?slots:*) }
   => { 
        ?slots
      }
      
  slots:
    { } => { }
    { ?slot:*; ... } => { ?slot ... }

  slot:
    { } => { }
    { ?foo:* ?slot-name:name \:: ?slot-type:* } => 
      { define generic ?slot-name ## "-setter" (?slot-name :: <object>, element :: <xmpp-element>) => (res :: <object>);
        define generic ?slot-name (element :: <xmpp-element>) => (res :: <object>);
      }
    { ?foo:* ?slot-name:name alias ?alias-name:expression \:: ?slot-type:* } =>       
      { define generic ?slot-name ## "-setter" (?slot-name :: <object>, element :: <xmpp-element>) => (res :: <object>);
        define generic ?slot-name (element :: <xmpp-element>) => (res :: <object>);
      }


end macro define-xmpp-element-generic-method;

define macro define-xmpp-element-getter-setter
  { define-xmpp-element-getter-setter(?:name; attribute-property ?slot-name:name \:: ?slot-type:*; ?rest:*) }
   => {
        define method ?slot-name ## "-setter" (value, object :: ?name) => (res);
          add-attribute(object, make(<attribute>, name: ?"slot-name", value: as(<string>, value)));
          value;
        end method;
        define method ?slot-name (object :: ?name) => (res :: false-or(?slot-type));
          let a = attribute(object, ?"slot-name");
          a & as(?slot-type, a.attribute-value);
        end method;
        define-xmpp-element-getter-setter(?name; ?rest)
      }

 { define-xmpp-element-getter-setter(?:name; attribute-property ?slot-name:name alias ?alias-name:expression \:: ?slot-type:*; ?rest:*) }
   => {
        define method ?slot-name ## "-setter" (value, object :: ?name) => (res);
          add-attribute(object, make(<attribute>, name: ?alias-name, value: as(<string>, value)));
          value;
        end method;
        define method ?slot-name (object :: ?name) => (res :: false-or(?slot-type));
          let a = attribute(object, ?alias-name);
          a & as(?slot-type, a.attribute-value);
        end method;
        define-xmpp-element-getter-setter(?name; ?rest)
      }


  { define-xmpp-element-getter-setter(?:name) } => { } 
end macro define-xmpp-element-getter-setter;

define xmpp-element foo-stanza ()
  attribute-property foo-id :: <string>;
  attribute-property foo-from :: <jid>;
  attribute-property foo-to :: <jid>;
  attribute-property foo-type :: <symbol>;
  attribute-property foo-language alias "xml:lang" :: <string>;
end xmpp-element foo-stanza;

/*
define element authentication-query ("query", "jabber:iq:auth")
end element query;
*/
