module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define method gen-row (stream :: <stream>, list)
 => ()
  format(stream, "<tr>");
  do(method(x)
         format(stream, "<td>%s</td>", x);
     end, list);
  format(stream, "</tr>\n");
end;

define method gen-option (stream :: <stream>,
                          key :: <string>,
                          value :: <string>)
 => ()
  format(stream, "<option value=\"%s\">%s</option>\n",
         key, value);
end;

define macro with-table
  { with-table (?stream:variable, ?titles:expression)
      ?body:body
    end }
  => { begin
         format(?stream, "<table>\n<tr>");
         do(method(x)
                format(?stream, "<th>%s</th>\n", x);
            end, ?titles);
         format(?stream, "</tr>\n");
         ?body;
         format(?stream, "</table>\n");
       end }
end macro;

define macro with-select
 { with-select (?stream:variable, ?name:expression)
     ?body:body
   end }
  => { begin
         format(?stream, "<select name=\"%s\">\n", ?name);
         ?body;
         format(?stream, "</select>\n");
       end }
end macro;

define macro with-form
  { with-form (?stream:variable, ?name:expression)
      ?body:body
    end }
  => { begin
         format(?stream, "<form action=\"%s\" method=\"post\">\n", ?name);
         ?body;
         format(?stream, "</form>\n");
       end; }
end macro;

define method hidden-form-field (stream :: <stream>,
                                 name :: <string>,
                                 value :: <string>)
 => ()
  form-field(stream, name, type: "hidden", value: value, prepend-name: #f);
end;

define method submit-form-field (stream :: <stream>,
                                 name :: <string>,
                                 value :: <string>)
 => ()
  form-field(stream, name, type: "submit", value: value, prepend-name: #f);
end;

define method form-field (stream :: <stream>,
                          name :: <string>,
                          #key type :: <string> = "text",
                          value :: false-or(<string>) = #f,
                          prepend-name :: <boolean> = #t,
                          checked :: <boolean> = #f)
 => ()
  if (prepend-name)
    format(stream, "%s: ", name);
  end;
  format(stream, "<input type=\"%s\" name=\"%s\" ", type, name);
  if (value)
    format(stream, "value=\"%s\"", value);
  end if;
  if (checked)
    format(stream, " checked");
  end if;
  format(stream, ">\n");
end;
