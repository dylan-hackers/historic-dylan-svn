module: code-browser
Synopsis: Brwose Open Dylan environment objects
Author:   Andreas Bogk, Bastian Mueller, Hannes Mehnert

define tag variable-value in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  let value = variable-value(*project*, *environment-object*);
  format(output-stream(response), "%=", value);
end;

define tag variable-type in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  let type = variable-type(*project*, *environment-object*);
  format(output-stream(response), "<a href=\"%s\">%s</a>",
         do-canonical-link(type),
         html-name(type));
end;

define tag thread-variable in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  if (instance?(*environment-object*, <thread-variable-object>))
    format(output-stream(response), "thread");
  end
end; 
