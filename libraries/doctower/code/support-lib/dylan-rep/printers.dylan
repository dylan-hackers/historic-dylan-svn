module: dylan-rep

define method print-object (o :: <library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "library %=", o.local-name);
   end printing-logical-block;
end method;

