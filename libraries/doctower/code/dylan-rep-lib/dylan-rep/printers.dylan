module: dylan-rep


define method print-object (o :: <source-name>, s :: <stream>) => ()
   format(s, "{name \"%s\"}", o)
end method;

define method print-message (o :: <library-name>, s :: <stream>) => ()
   format(s, "%s", o.library-name)
end method;

define method print-message (o :: <module-name>, s :: <stream>) => ()
   format(s, "%s:%s", o.library-name, o.module-name)
end method;

define method print-message (o :: <binding-name>, s :: <stream>) => ()
   format(s, "%s:%s:%s", o.library-name, o.module-name, o.binding-name)
end method;

define method print-object (o :: <defined-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "defined-library \"%s\" ", o.canonical-name);
      pprint-newline(#"fill", s);
      format(s, "(%s)", o.provenance);
      unless (o.file-markup-tokens.empty?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "file docs");
      end unless;
      unless (o.markup-tokens.empty?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "docs");
      end unless;
   end printing-logical-block;
end method;

define method print-object (o :: <undefined-library>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "undefined-library \"%s\" ", o.canonical-name);
      pprint-newline(#"fill", s);
      format(s, "(%s)", o.provenance);
   end printing-logical-block;
end method;

define method print-object (o :: <defined-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "defined-module \"%s\" ", o.canonical-name);
      pprint-newline(#"fill", s);
      format(s, "(%s)", o.provenance);
      when (o.aliases.size > 1)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "aliased");
      end when;
      unless (o.markup-tokens.empty?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "docs");
      end unless;
   end printing-logical-block;
end method;

define method print-object (o :: <undefined-module>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "undefined-module \"%s\" ", o.canonical-name);
      pprint-newline(#"fill", s);
      format(s, "(%s)", o.provenance);
      when (o.aliases.size > 1)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "aliased");
      end when;
   end printing-logical-block;
end method;

define method print-object (o :: <binding>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "%s ", select (o by instance?)
                          <class-binding> => "class-binding";
                          <function-binding> => "function-binding";
                          <generic-binding> => "generic-binding";
                          <constant-binding> => "constant-binding";
                          <variable-binding> => "variable-binding";
                          <macro-binding> => "macro-binding";
                          <empty-binding> => "empty-binding";
                          <placeholder-binding> => "placeholder-binding";
                          otherwise => "binding";
                       end select);
      pprint-newline(#"fill", s);
      format(s, "\"%s\" ", o.canonical-name);
      pprint-newline(#"fill", s);
      format(s, "(%s)", o.provenance);
      when (o.aliases.size == 1)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "local");
      end when;
      unless (o.all-defns.empty?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "defns");
      end unless;
      unless (o.valid-binding?)
         write(s, ", ");
         pprint-newline(#"fill", s);
         write(s, "incomplete");
      end unless;
   end printing-logical-block;
end method;


define method print-object (o :: <implicit/explicit-defn>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      format(s, "%s ", select (o by instance?)
                          <explicit-generic-defn> => "explicit-generic-defn";
                          <implicit-generic-defn> => "implicit-generic-defn";
                          <explicit-function-defn> => "explicit-function-defn";
                          <explicit-class-defn> => "explicit-class-defn";
                          <explicit-macro-defn> => "explicit-macro-defn";
                          <explicit-constant-defn> => "explicit-constant-defn";
                          <explicit-variable-defn> => "explicit-variable-defn";
                          otherwise => "implicit/explicit-defn";
                       end select);
      pprint-newline(#"fill", s);
      format(s, "@ %s", o.source-location);
   end printing-logical-block;
end method;


// define method print-object (o :: <class-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "class-defn ");
//       pprint-newline(#"fill", s);
//       format(s, "slots %d, ", o.explicit-defn.slots.size);
//       pprint-newline(#"fill", s);
//       format(s, "init-args %d", o.explicit-defn.init-args.size);
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <generic-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "generic-defn ");
//       pprint-newline(#"fill", s);
//       unless (o.all-defns.empty?)
//          format(s, "%=, ", o.all-defns.first);
//          pprint-newline(#"fill", s);
//       end unless;
//       format(s, "expl %d, ", if (o.explicit-defn) 1 else 0 end);
//       pprint-newline(#"fill", s);
//       format(s, "impl %d", o.implicit-defns.size);
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <function-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "function-defn ");
//       pprint-newline(#"fill", s);
//       format(s, "%=", o.explicit-defn);
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <constant-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "constant-defn ");
//       pprint-newline(#"fill", s);
//       format(s, "type %=, ", o.explicit-defn.type);
//       pprint-newline(#"fill", s);
//       format(s, "value %=", o.explicit-defn.value);
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <variable-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "variable-defn ");
//       pprint-newline(#"fill", s);
//       format(s, "type %=, ", o.explicit-defn.type);
//       pprint-newline(#"fill", s);
//       format(s, "value %=", o.explicit-defn.value);
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <macro-defn>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "macro-defn");
//       unless (o.markup-tokens.empty?)
//          write(s, ", ");
//          pprint-newline(#"fill", s);
//          write(s, "docs");
//       end unless;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <func/gen-definition>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "func/gen-definition ");
//       printing-logical-block (s, prefix: "(", suffix: ")")
//          for (param in o.parameter-list.param-list.req-params, prepend = "" then ", ")
//             write(s, prepend);
//             pprint-newline(#"fill", s);
//             format(s, "%=", param.type);
//          end for;
//          unless (instance?(o.parameter-list.param-list, <fixed-param-list>))
//             write(s, "...")
//          end unless;
//       end printing-logical-block;
//       write(s, " => ");
//       printing-logical-block (s, prefix: "(", suffix: ")")
//          for (val in o.parameter-list.value-list.req-values, prepend = "" then ", ")
//             write(s, prepend);
//             pprint-newline(#"fill", s);
//             format(s, "%=", val.type);
//          end for;
//       end printing-logical-block;
//       when (instance?(o, <implicit-generic-defn>))
//          unless (o.markup-tokens.empty?)
//             write(s, ", ");
//             pprint-newline(#"fill", s);
//             write(s, "docs");
//          end unless;
//       end when;
//    end printing-logical-block;
// end method;
// 
// define method print-object (o :: <parameter-list>, s :: <stream>) => ()
//    printing-logical-block (s, prefix: "{", suffix: "}")
//       write(s, "parameter-list ");
//       pprint-newline(#"fill", s);
//       format(s, "params %=, ", o.param-list);
//       pprint-newline(#"fill", s);
//       format(s, "values %=", o.value-list);
//    end printing-logical-block;
// end method;

define method print-object (o :: <fixed-param-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "fixed-param-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d", o.req-params.size);
      unless (o.req-params.empty?)
         unless (every?(compose(empty?, markup-tokens), o.req-params))
            write(s, ", ");
            pprint-newline(#"fill", s);
            write(s, "docs");
         end unless;
      end unless;
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
      unless (o.req-params.empty? & o.key-params.empty?)
         unless (every?(compose(empty?, markup-tokens),
                        concatenate(o.req-params, o.key-params)))
            write(s, ", ");
            pprint-newline(#"fill", s);
            write(s, "docs");
         end unless;
      end unless;
   end printing-logical-block;
end method;

define method print-object (o :: <var-param-list>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "var-param-list ");
      pprint-newline(#"fill", s);
      format(s, "req %d, ", o.req-params.size);
      pprint-newline(#"fill", s);
      write(s, "rest");
      unless (o.req-params.empty?)
         unless (every?(compose(empty?, markup-tokens), o.req-params))
            write(s, ", ");
            pprint-newline(#"fill", s);
            write(s, "docs");
         end unless;
      end unless;
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
      unless (o.req-values.empty?)
         unless (every?(compose(empty?, markup-tokens), o.req-values))
            write(s, ", ");
            pprint-newline(#"fill", s);
            write(s, "docs");
         end unless;
      end unless;
   end printing-logical-block;
end method;

define method print-object (o :: <fragment>, s :: <stream>) => ()
   printing-logical-block (s, prefix: "{", suffix: "}")
      write(s, "fragment \"");
      for (elem in o.source-text)
         case
            instance?(elem, <source-name>) => format(s, "{%s}", elem);
            otherwise => write-element(s, elem);
         end case;
      end for;
      write(s, "\"");
   end printing-logical-block;
end method;

define method print-message (o :: <fragment>, s :: <stream>) => ()
   for (elem in o.source-text)
      case
         instance?(o, <source-name>) => write(s, elem.local-name);
         otherwise => format(s, "%s", elem);
      end case;
   end for;
end method;
