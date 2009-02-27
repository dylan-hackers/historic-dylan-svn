module: dylan-rep

define method print-object (o :: <known-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "known-library %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <unknown-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "unknown-library");
      unless (o.anonymous?) format(s, " %=", o.local-name) end;
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
      format(s, "imported-module %=, ", o.local-name);
      pprint-newline(#"fill", s);
      format(s, "from %= ", o.import-name);
      pprint-newline(#"fill", s);
      format(s, "in %=", o.used-library);
      if (o.exported?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "exported");
      end if;
   end printing-logical-block;
end method;
