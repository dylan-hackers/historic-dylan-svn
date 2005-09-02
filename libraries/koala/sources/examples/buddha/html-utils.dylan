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

define method gen-link (stream :: <stream>,
                        link :: <string>,
                        name :: <string>)
 => ()
  format(stream, "<a href=\"%s\">%s</a>\n", link, name);
end;

define method gen-stylesheet (stream :: <stream>,
                              link :: <string>)
 => ()
  format(stream, "<link rel=\"stylesheet\" href=\"%s\" />", link);
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

define macro with-body
 { with-body (?stream:variable)
     ?body:body
   end }
  => { begin
         format(?stream, "<body>\n");
         ?body;
         format(?stream, "</body>\n");
       end }
end macro;

define macro with-html
 { with-html (?stream:variable)
     ?body:body
   end }
  => { begin
         format(?stream,
                "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n");
         format(?stream, "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n");
         ?body;
         format(?stream, "</html>\n");
       end }
end macro;

define macro with-header
 { with-header (?stream:variable, ?title:expression)
     ?body:body
   end }
  => { begin
         format(?stream, "<head>\n\t<title>%s</title>\n", ?title);
         ?body;
         format(?stream, "</head>\n");
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

define macro with-div
  { with-div (?stream:variable, ?name:expression, ?value:expression)
      ?body:body
    end }
  => { begin
         format(?stream, "<div %s=\"%s\">\n", ?name, ?value);
         ?body;
         format(?stream, "</div>\n");
       end; }
end macro;

define macro with-form
  { with-form (?stream:variable, ?name:expression)
      ?body:body
    end }
  => { begin
         format(?stream, "<form action=\"%s\" method=\"post\">\n", ?name);
         with-div(?stream, "class", "edit")
           ?body;
         end;
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
    format(stream, "value=\"%s\" ", value);
  end if;
  if (checked)
    format(stream, "checked=\"checked\" ");
  end if;
  format(stream, "/>\n");
end;
