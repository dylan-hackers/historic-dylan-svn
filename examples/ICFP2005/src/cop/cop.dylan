module: cop
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  make(<world>);
  format-out("Hello, I'm a cop!\n");
  exit-application(0);
end function main;

main(application-name(), application-arguments());
