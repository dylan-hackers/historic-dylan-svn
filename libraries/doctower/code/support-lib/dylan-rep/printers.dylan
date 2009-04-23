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

define method print-object (o :: <class-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "class-defn ");
      pprint-newline(#"fill", s);
      format(s, "slots %d, ", o.explicit-defn.slots.size);
      pprint-newline(#"fill", s);
      format(s, "init-args %d", o.explicit-defn.init-args.size);
   end printing-logical-block;
end method;

define method print-object (o :: <generic-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "generic-defn ");
      pprint-newline(#"fill", s);
      unless (o.all-defns.empty?)
         format(s, "%=, ", o.all-defns.first);
         pprint-newline(#"fill", s);
      end unless;
      format(s, "expl %d, ", if (o.explicit-defn) 1 else 0 end);
      pprint-newline(#"fill", s);
      format(s, "impl %d", o.implicit-defns.size);
   end printing-logical-block;
end method;

define method print-object (o :: <function-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "function-defn ");
      pprint-newline(#"fill", s);
      format(s, "%=", o.explicit-defn);
   end printing-logical-block;
end method;

define method print-object (o :: <constant-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "constant-defn ");
      pprint-newline(#"fill", s);
      format(s, "type %=, ", o.explicit-defn.type);
      pprint-newline(#"fill", s);
      format(s, "value %=", o.explicit-defn.value);
   end printing-logical-block;
end method;

define method print-object (o :: <variable-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "variable-defn ");
      pprint-newline(#"fill", s);
      format(s, "type %=, ", o.explicit-defn.type);
      pprint-newline(#"fill", s);
      format(s, "value %=", o.explicit-defn.value);
   end printing-logical-block;
end method;

define method print-object (o :: <macro-defn>, s :: <stream>) => ()
   write(s, "{macro-defn}");
end method;

define method print-object (o :: <func/gen-definition>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "func/gen-definition ");
      printing-logical-block (s, prefix: "(", suffix: ")")
         for (param in o.parameter-list.param-list.req-params, prepend = "" then ", ")
            write(s, prepend);
            pprint-newline(#"fill", s);
            format(s, "%=", param.type);
         end for;
         unless (instance?(o.parameter-list.param-list, <fixed-param-list>))
            write(s, "...")
         end unless;
      end printing-logical-block;
      write(s, " => ");
      printing-logical-block (s, prefix: "(", suffix: ")")
         for (val in o.parameter-list.value-list.req-values, prepend = "" then ", ")
            write(s, prepend);
            pprint-newline(#"fill", s);
            format(s, "%=", val.type);
         end for;
      end printing-logical-block;
   end printing-logical-block;
end method;

define method print-object (o :: <parameter-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "parameter-list ");
      pprint-newline(#"fill", s);
      format(s, "params %=, ", o.param-list);
      pprint-newline(#"fill", s);
      format(s, "values %=", o.value-list);
   end printing-logical-block;
end method;

define method print-object (o :: <fixed-param-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "fixed-param-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d", o.req-params.size);
   end printing-logical-block;
end method;

define method print-object (o :: <key-param-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "key-param-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d, ", o.req-params.size);
      pprint-newline(#"fill", s);
      format(s, "key %d", o.key-params.size);
      when (o.rest-param)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "rest");
      end when;
      when (o.all-keys?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "all-keys");
      end when;
   end printing-logical-block;
end method;

define method print-object (o :: <var-param-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "var-param-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d, ", o.req-params.size);
      pprint-newline(#"fill", s);
      write(s, "rest");
   end printing-logical-block;
end method;

define method print-object (o :: <value-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "value-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d", o.req-values.size);
      when (o.rest-value)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "rest");
      end when;
   end printing-logical-block;
end method;

define method print-object (o :: <fragment>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "fragment \"");
      for (elem in o.source-text)
         case
            instance?(elem, <character>) => write-element(s, elem);
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
