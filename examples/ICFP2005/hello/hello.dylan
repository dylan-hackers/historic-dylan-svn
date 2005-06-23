module: hello
synopsis: 
author: 
copyright: 

define constant <my-list> = <list>;

define function main(name, arguments)
  format-out("Hello, world!\n");
  make(<my-list>);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
