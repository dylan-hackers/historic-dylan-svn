module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
/*
  let v = vector3D(1.0, 2.0, 3.0);
  let a = v.magnitude;
  let b = v.homogenize;
  let c = b.magnitude;

  let d = v.normalize;
  let e = b.normalize;

  let f = d.homogenize;
  let g = e.homogenize;

  format-out("v = %=, a = %=, b = %=, c = %=\n", v, a, b, c);
  format-out("x = %=, y = %=\n", d, e);
  format-out("x = %=, y = %=\n", f, g);
*/
 
  let input-stream = *standard-input*;
  if (arguments.size > 0 & arguments[0] ~= "-")
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
define method print-object(v :: <3D-vector>, stream :: <stream>)
 => ();
  print-object(vector(v.x, v.y, v.z), stream);
end method print-object;

// Invoke our main() function.
main(application-name(), application-arguments());
