Module:   koala-app
Synopsis: Koala HTTP Server Application
Author:   Carl Gay

define function main
    () => ()
  let config-file =
    if(application-arguments().size > 0)
      application-arguments()[0]
    end;
  start-server(config-file: config-file);
end;

begin
  main();
end;
