module: nehe01
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  format-out("Hello, world!\n");
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
