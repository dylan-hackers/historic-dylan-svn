Module:    circle-tool
Synopsis:  A dynamic tool for drawing circles.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define class <circle-tool> (<dynamic-draw-tool>)
end class <circle-tool>;

define method tool-description(tool :: <circle-tool>) => (d :: <string>)
  "Draw a circle"
end method tool-description;

define method do-tool(tool :: <circle-tool>, surface, x, y) => ()
  draw-circle(surface, x, y, 20, filled?: #f);
end method do-tool;

define constant $the-tool = make(<circle-tool>);

define class <circle-tool-dynamic-library> (<dynamic-library>)
end class <circle-tool-dynamic-library>;

define method dynamic-library-description(lib :: <circle-tool-dynamic-library>) => (s :: <string>)
  "Circle Tool Dynamic Library"
end method dynamic-library-description;

begin
  register-dynamic-library( 
    make(<circle-tool-dynamic-library>, 
         load-callback: method () register-tool($the-tool) end,
         unload-callback: method() unregister-tool($the-tool) end));                                       
end;
