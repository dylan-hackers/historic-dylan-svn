module: robber
synopsis: 
author: 
copyright: 
    
define function main(name, arguments)
  //let robber = make(<alterna-robber>);
  //let robber = make(<random-walk-robber>);
  let robber = make(<bruce-robber>);
  drive-agent(robber, *standard-input*, *standard-output*);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
