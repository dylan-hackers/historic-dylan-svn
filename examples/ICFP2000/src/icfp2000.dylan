module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  let mat = identity-matrix(dimensions: #[4,4]);
  let vec = #[1.0, 2.0, 3.0, 4.0];

  format-out("%=\n%=\n%=\n", mat, vec, homogenize(vec));
  format-out("%=\n%=\n%=\n", mat * vec, vec * mat, vec * vec);

  let red :: <color> = make(<color>, red: 1.0, blue: 0.0, green: 0.0);
  let green :: <color> = make(<color>, red: 0.0, green: 1.0, blue: 0.0);
  let file = make(<ppm-image>, filename: "test.ppm", width: 256,
		  height: 256, depth: 255);

  for (i from 0 below 256*256/2)
    write-pixel(file, red);
    write-pixel(file, green);
  end for;
  close-ppm(file);

  let o = make(<obj>);
  o.model := make(<sphere>);

  render-image(o, 1, "render.ppm", 
	       make(<color>, red: 1.0, green: 1.0, blue:1.0),
	       make(<stretchy-vector>), 256, 256, $double-pi);

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
