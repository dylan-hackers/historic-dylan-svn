Module:    icon-play
Synopsis:  Example of using icons.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define constant $clock-icon = 
    read-image-as(<win32-icon>, "CLOCK", #"icon"); 

define frame <icon-frame> (<simple-frame>)
  pane the-icon (frame)
    make(<label>, label: $clock-icon);
  
  layout (frame)
    labelling("The clock icon")
      frame.the-icon;
    end;

end frame <icon-frame>;

define method main () => ()
  start-frame(make(<icon-frame>));
end method main;

begin
  main();
end;
