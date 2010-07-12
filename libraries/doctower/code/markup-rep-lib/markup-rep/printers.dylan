module: markup-rep

define method print-object (o :: <topic>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
   end printing-logical-block;
end method;

define method print-object (o :: <api-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "api topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <library-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "library topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("modules", o.modules-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <module-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "module topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("bindings", o.bindings-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <class-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "class topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("adjectives", o.adjectives-section, s);
      print-topic-section("keywords", o.keywords-section, s);
      print-topic-section("conds", o.conds-section, s);
      print-topic-section("inheritables", o.inheritables-section, s);
      print-topic-section("supers", o.supers-section, s);
      print-topic-section("subs", o.subs-section, s);
      print-topic-section("funcs-on", o.funcs-on-section, s);
      print-topic-section("funcs-returning", o.funcs-returning-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <function-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "function topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("adjectives", o.adjectives-section, s);
      print-topic-section("args", o.args-section, s);
      print-topic-section("vals", o.vals-section, s);
      print-topic-section("conds", o.conds-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <generic-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "generic topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("adjectives", o.adjectives-section, s);
      print-topic-section("args", o.args-section, s);
      print-topic-section("vals", o.vals-section, s);
      print-topic-section("conds", o.conds-section, s);
      print-topic-section("methods", o.methods-section, s);
   end printing-logical-block;
end method;

define method print-object (o :: <variable-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "variable topic %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "fqn %=, ", o.fully-qualified-name);
      pprint-newline(#"fill", s);
      write(s, "parent ");
      if (instance?(o.parent, <topic>))
         format(s, "{topic %=, id %=}", o.parent.title, o.parent.id);
      else
         format(s, "%=", o.parent);
      end if;
      write(s, ", ");
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
      print-topic-section("definitions", o.definitions-section, s);
      print-topic-section("adjectives", o.adjectives-section, s);
      print-topic-section("value", o.value-section, s);
   end printing-logical-block;
end method;

define method print-topic-section (name :: <string>, section, s :: <stream>) => ()
   write(s, ", ");
   pprint-newline(#"linear", s);
   format(s, "%s ", name);
   case
      section & section.content.empty? => write(s, "empty");
      otherwise => format(s, "%=", section);
   end case
end method;

define method print-object (o :: <section>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "section %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"linear", s);
      format(s, "content %=", o.content);
   end printing-logical-block;
end method;

define method print-object
   (o :: type-union(<title-seq>, <markup-seq>, <content-seq>, <topic-content-seq>),
    s :: <stream>)
=> ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, select (o by instance?)
                  <title-seq> => "title ";
                  <markup-seq> => "markup ";
                  <content-seq> => "section-content ";
                  <topic-content-seq> => "topic-content ";
               end select);
      for (i in o)
         format(s, "%=, ", i);
         pprint-newline(#"fill", s);
      end for;
   end printing-logical-block;
end method;

define method print-message (o :: <title-seq>, s :: <stream>) => ()
   write(s, o.stringify-title)
end method;

define method print-object (o :: <paragraph>, s :: <stream>) => ()
   format(s, "{para %=}", o.content)
end method;

define method print-object (o :: <footnote>, s :: <stream>) => ()
   format(s, "{footnote #%s %=}", o.index, o.content)
end method;

define method print-object (o :: <code-phrase>, s :: <stream>) => ()
   format(s, "{code-phrase %=}", o.text)
end method;

define method print-object (o :: <api/parm-name>, s :: <stream>) => ()
   format(s, "{api/parm-name %=}", o.text)
end method;

define method print-object (o :: <xref>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "xref %= ", o.text);
      pprint-newline(#"fill", s);
      if (instance?(o.target, <topic>))
         format(s, "{topic %=, id %=}", o.target.title, o.target.id);
      else
         format(s, "%=", o.target);
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <vi-xref>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "vi-xref %= ", o.text);
      pprint-newline(#"fill", s);
      if (instance?(o.target, <topic>))
         format(s, "{topic %=, id %=}", o.target.title, o.target.id);
      else
         format(s, "%=", o.target);
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <conref>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "conref (%s) ", o.style);
      pprint-newline(#"fill", s);
      if (instance?(o.target, <topic>))
         format(s, "{topic %=, id %=} ", o.target.title, o.target.id);
      else
         format(s, "%= ", o.target);
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <target-placeholder>, s :: <stream>) => ()
   format(s, "{placeholder %=}", o.target)
end method;

define method print-object
   (o :: type-union(<ordered-list>, <unordered-list>), s :: <stream>)
=> ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, select (o by instance?)
                  <ordered-list> => "ordered-list ";
                  <unordered-list> => "unordered-list ";
               end select);
      for (i in o.items)
         format(s, "%=, ", i);
         pprint-newline(#"linear", s);
      end for;
   end printing-logical-block;
end method;

define method print-object (o :: <defn-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, select (o by instance?)
                  <one-line-defn-list> => "one-line-defn-list ";
                  <many-line-defn-list> => "many-line-defn-list ";
               end select);
      for (i from 0 below dimension(o.items, 0))
         format(s, "%= => ", o.items[i, 0]);
         pprint-newline(#"fill", s);
         format(s, "%=, ", o.items[i, 1]);
         pprint-newline(#"linear", s);
      end for;
   end printing-logical-block;
end method;
