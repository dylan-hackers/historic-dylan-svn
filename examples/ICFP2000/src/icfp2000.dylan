module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  /*
  let mat2 = identity-matrix(dimensions: #[4,4]);
  let mat = matrix(#[1.0, 0.0, 0.0, 0.0],
		    #[0.0, 1.0, 0.0, 0.25],
		    #[0.0, 0.0, 1.0, 0.0],
		    #[0.0, 0.0, 0.0, 1.0]);
		    
  let vec = vector3D(1.0, 2.0, 3.0, 4.0);

  format-out("%=\n%=\n%=\n", mat, vec, homogenize(vec));
  format-out("%=\n%=\n%=\n", homogenize(mat * vec), vec * mat, vec * vec);
  
  format-out("parsing 3.141592654123456789e27 gives %=\n",
	     atof("3.141592654123456789e27"));
  */

  begin
    let lexed = lex-gml(*standard-input*);
    let out = lexed.run-gml;
    
   //format-out("input : %=\n\n", in);
    format-out("lexed : %=\n\n", lexed);
    format-out("output: %=\n\n", out);
    force-output(*standard-output*);
  end;

  //test-matrix-vs-transform();
  //exit-application(0);
/*
  let o1 = make(<sphere>);
  let o2 = make(<plane>);
  let o3 = make(<plane>);
  o1.surface-interpreter-entry := blue-texture;
  o2.surface-interpreter-entry := red-texture;
  o3.surface-interpreter-entry := blue-texture;

  o1 := uniform-scale(o1, 0.3);

  let l = make(<star>, direction: vector3D(0.2, -1.0, 0.0, 0.0),
	       color: make-white());


//  o2 := x-rotate(o2, $double-pi / 4.0);
  o2 := translate(o2, 0.0, -0.5, 0.0);

  o3 := translate(o3, 0.0, 0.5, 0.0);

  

  let o = make(<csg-union>, of: vector(o1, o2));
  
//  z-rotate!(o, $double-pi / 16.0);

  let (p, n) = intersection-before(o, make(<ray>, 
					   position: vector3D(0.0, 0.0, -1.0, 1.0),
					   direction: vector3D(0.0, 0.0,  1.0, 0.0)),
				   1.0/0.0);

  format-out("Ray towards origin --  point: %= normal: %=\n", p, n);
	     

  format-out("Ray towards upper-left: %=\n",
	     intersection-before(o, make(<ray>, 
					 position: vector3D(0.0, 0.0, -1.0, 1.0),
					 direction: vector3D(-1.0, 1.0,  1.0, 0.0)),
				 1.0/0.0));

  force-output(*standard-output*);

  render-image(o, 1, "render.ppm", 
	       make(<color>, red: 0.0, green: 0.0, blue: 0.0), 
	       vector(l), 
	       256, 256, $double-pi / 2.0);
*/
  exit-application(0);
end function main;

// Debugging wedge
define method print-object(v :: <vector3D>, stream :: <stream>)
 => ();
  print-object(vector(v.x, v.y, v.z, v.w), stream);
end method print-object;

// Invoke our main() function.
main(application-name(), application-arguments());
