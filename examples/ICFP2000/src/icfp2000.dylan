module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  let input-stream = *standard-input*;
  if (arguments.size > 0 & arguments[0] ~= "-")
    input-stream := make(<file-stream>, direction: #"input", locator: arguments[0]);
  end if;

  block ()
    let lexed = lex-gml(input-stream);
    let out = lexed.run-gml;
    
    format-out("Run sucessful, final stack: ");
    if (out = #())
      format-out("<empty>");
    else
      do(method(val) format-out("%= ", val) end, out);
    end;
    format-out("\n");

  exception (e :: <condition>)

    format-out("Sorry, Dylan Hacker has detected an error\n");
    format-out("\n=========================================\n");
    report-condition(e, *standard-output*);
    format-out("\n=========================================\n");
    format-out("\nProgram terminating with error status\n");

    exit-application(1);
  end;

  exit-application(0);
end function main;

// Debugging wedge
define method print-object(v :: <3D-vector>, stream :: <stream>)
 => ();
  print-object(vector(v.x, v.y, v.z), stream);
end method print-object;

// Invoke our main() function.
main(application-name(), application-arguments());
