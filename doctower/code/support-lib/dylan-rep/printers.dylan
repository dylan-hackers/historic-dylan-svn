module: dylan-rep

define method print-object (o :: <known-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "known-library %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <unknown-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "unknown-library %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <local-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "local-module %=", o.local-name);
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <imported-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "imported-module %=", o.local-name);
      write(s, ", ");
      pprint-newline(#"fill", s);
      write(s, "from ");
      if (o.stray?)
         write(s, "unknown");
      else
         format(s, "%= ", o.import-name);
         pprint-newline(#"fill", s);
         format(s, "in %=", o.used-library);
      end if;
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <local-binding>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "local-binding %=", o.local-name);
      pprint-newline(#"fill", s);
      unless (o.definition)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "undefined");
      end unless;
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <imported-binding>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "imported-binding %=, ", o.local-name);
      pprint-newline(#"fill", s);
      write(s, "from ");
      if (o.stray?)
         write(s, "unknown");
      else
         format(s, "%= ", o.import-name);
         pprint-newline(#"fill", s);
         format(s, "in %=", o.used-module);
      end if;
      unless (o.definition)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "undefined");
      end unless;
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;

define method print-object (o :: <fragment>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "fragment \"");
      for (elem in o.source-text)
         case
            instance?(o, <character>) => write-element(s, elem);
            otherwise => format(s, "{%s}", elem.source-name);
         end case;
      end for;
      write(s, "\"");
   end printing-logical-block;
end method;

define method print-message (o :: <fragment>, s :: <stream>) => ()
   for (elem in o.source-text)
      case
         instance?(o, <character>) => write-element(s, elem);
         otherwise => format(s, "%s", elem.source-name);
      end case;
   end for;
end method;
