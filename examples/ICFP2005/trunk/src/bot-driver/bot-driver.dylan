module: bot-driver
synopsis: 
author: 
copyright: 

define function main(name, arguments)
  drive-agent(make(find-bot(arguments[0])), *standard-input*, *standard-output*);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
