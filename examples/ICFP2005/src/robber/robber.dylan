module: robber
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  make(<world>);
  format-out("Hello, I'm a robber!\n");
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
