module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
 
  let input-stream = *standard-input*;
  if (arguments & arguments[0] ~= "-")
    input-stream := make(<file-stream>, direction: #"input", locator: arguments[0]);
  end if;

  begin
    let lexed = lex-gml(input-stream);
    let out = lexed.run-gml;
    
   //format-out("input : %=\n\n", in);
    format-out("lexed : %=\n\n", lexed);
    format-out("output: %=\n\n", out);
    force-output(*standard-output*);
  end;

  exit-application(0);
end function main;

// Debugging wedge
define method print-object(v :: <vector3D>, stream :: <stream>)
 => ();
  print-object(vector(v.x, v.y, v.z, v.w), stream);
end method print-object;

// Invoke our main() function.
main(application-name(), application-arguments());
