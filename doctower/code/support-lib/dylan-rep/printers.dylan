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
      format(s, "local-binding %=, ", o.local-name);
      pprint-newline(#"fill", s);
      if (o.definition)
         format(s, "def %=", o.definition);
      else
         write(s, "no def");
      end if;
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
      write(s, ", ");
      pprint-newline(#"fill", s);
      if (o.definition)
         format(s, "def %=", o.definition);
      else
         write(s, "no def");
      end if;
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;
