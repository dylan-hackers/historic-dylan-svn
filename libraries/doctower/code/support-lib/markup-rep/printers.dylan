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
      pprint-newline(#"fill", s);
      format(s, "content %=", o.content);
   end printing-logical-block;
end method;

define method print-object (o :: <api-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "api topic %=, ", o.title);
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
      pprint-newline(#"fill", s);
      format(s, "content %=", o.content);
      write(s, ", ");
      pprint-newline(#"fill", s);
      format(s, "definitions %=", o.definitions-section);
   end printing-logical-block;
end method;

define method print-object (o :: <library-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "library topic %=, ", o.title);
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
      pprint-newline(#"fill", s);
      format(s, "content %=", o.content);
      write(s, ", ");
      pprint-newline(#"fill", s);
      format(s, "definitions %=", o.definitions-section);
      write(s, ", ");
      pprint-newline(#"fill", s);
      format(s, "modules %=", o.modules-section);
   end printing-logical-block;
end method;

define method print-object (o :: <module-doc>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "module topic %=, ", o.title);
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
      pprint-newline(#"fill", s);
      format(s, "content %=", o.content);
      write(s, ", ");
      pprint-newline(#"fill", s);
      format(s, "definitions %=", o.definitions-section);
      write(s, ", ");
      pprint-newline(#"fill", s);
      format(s, "bindings %=", o.bindings-section);
   end printing-logical-block;
end method;

define method print-object (o :: <section>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "section %=, ", o.title);
      pprint-newline(#"fill", s);
      format(s, "id %=, ", o.id);
      pprint-newline(#"fill", s);
      format(s, "content %=", o.content);
   end printing-logical-block;
end method;

define method print-object (o :: <topic-level-style>, s :: <stream>) => ()
   format(s, "{topic-style '%c'%s%s%s}", o.line-character,
          (o.overline? & " over") | "", (o.midline? & " mid") | "",
          (o.underline? & " under") | "");
end method;

define method print-object
   (o :: type-union(<title-seq>, <markup-seq>, <content-seq>, <topic-content-seq>),
    s :: <stream>)
=> ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, select (o by instance?)
                  <title-seq> => "title";
                  <markup-seq> => "markup";
                  <content-seq> => "section-content";
                  <topic-content-seq> => "topic-content";
               end select);
      for (i in o)
         format(s, " %=,", i);
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

define method print-object (o :: <target-placeholder>, s :: <stream>) => ()
   format(s, "{placeholder %=}", o.target)
end method;

define method print-object
   (o :: type-union(<ordered-list>, <unordered-list>), s :: <stream>)
=> ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, select (o by instance?)
                  <ordered-list> => "ordered-list";
                  <unordered-list> => "unordered-list";
               end select);
      write-element(s, ' ');
      for (i in o.items)
         format(s, "%=, ", i);
         pprint-newline(#"fill", s);
      end for;
   end printing-logical-block;
end method;
