Module:    tidy-com-example
Synopsis:  Parsing HTML
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define method main () => ()
  let page = 
    with-http-session(session = make-http-session())
      do-http-request(session, "http://www.double.co.nz");
    end;
  let (tidied-page, warnings, errors) = html-tidy(page);
  if(warnings)
    format-out("\nWarnings:\n");
    for(warning in warnings)
      format-out("  %s\n", warning)
    end for;
  else
    format-out("No Warnings.");
  end if;

  if(errors)
    format-out("Errors:\n");
    for(error in errors)
      format-out("  %s\n", error)
    end for;
  else
    format-out("No Errors.\n");
  end if;

end method main;

begin
  start-http-client();
  main();
  stop-http-client();
end;
