Module:   koala-app
Synopsis: Koala HTTP Server Application
Author:   Carl Gay

define method main () => ()
  let args = application-arguments();
  let config = #f;
  let pos = find-key(args, method (x) as-lowercase(x) = "--config" end);
  if (pos & (args.size > pos + 1))
    config := args[pos + 1];
  end;
  start-server(config-file: config);
end;

begin
  main();
end;
