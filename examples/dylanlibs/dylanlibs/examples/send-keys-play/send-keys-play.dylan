Module:    send-keys-play
Synopsis:  A brief description of the project.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

 
define method main () => ()
  // Start a DOS Application
  make(<thread>, function: 
    method()
      let status = run-application("edit.com");
      format-out("Done: %=.\n", status);
    end);

  // Wait for edit to start by looking for some text
  wait-for-console-text(3, 0, "File");

  // Type some text
  send-keys("This should appear in the edit window.");
  send-keys(create-up-down-key-press(#"return"));
  send-keys("Demo completed. Exiting Edit will close the application down.");
end method main;

begin
  main();
  format-out("Done.\n");
end;
