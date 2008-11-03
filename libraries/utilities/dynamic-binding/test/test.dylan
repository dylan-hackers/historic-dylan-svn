module: test
synopsis: Simple demonstration program.
author: Dustin Voss

define function main(name, arguments)
   with-dynamic-bindings (str :: <string> = "Hello, world!")
      format-out("Initial value:\t%=\n", dynamic-binding(str));
      change-value();
      final-value();
   end;
end function main;

define function change-value()
   dynamic-binding(str) := "Goodbye, cruel world!";
end function;

define function final-value()
   format-out("Final value:\t%=\n", dynamic-binding(str));
end function;

main(application-name(), application-arguments());
