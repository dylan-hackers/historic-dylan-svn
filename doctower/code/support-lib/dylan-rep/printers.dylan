module: dylan-rep

define method print-object (o :: <library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "library %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <unknown-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "unknown-library %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <exported-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "exported-module %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <internal-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "internal-module %=", o.local-name);
   end printing-logical-block;
end method;

define method print-object (o :: <reexported-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "reexported-module %=, ", o.local-name);
      pprint-newline(#"fill", s);
      format(s, "from %=:%=", o.used-library.local-name, o.import-name);
   end printing-logical-block;
end method;

define method print-object (o :: <imported-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "imported-module %=, ", o.local-name);
      pprint-newline(#"fill", s);
      format(s, "from %=:%=", o.used-library.local-name, o.import-name);
   end printing-logical-block;
end method;
