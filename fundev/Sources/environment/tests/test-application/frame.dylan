Module:    environment-test-application
Synopsis:  An application for use by the environment test suite
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <test-frame> (<simple-frame>)
  pane my-button (frame)
    make(<button>, label: "Hello world!");
  layout (frame)
    frame.my-button;
  keyword title: = "Test Frame";
end frame <test-frame>;

define function main ()
  start-frame(make(<test-frame>))
end function main;

main();
