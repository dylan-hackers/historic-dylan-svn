module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  let mat = identity-matrix(dimensions: #[4,4]);
  let vec = #[1.0, 2.0, 3.0, 4.0];

  format-out("%=\n%=\n%=\n", mat, vec, homogenize(vec));
  format-out("%=\n%=\n%=\n", mat * vec, vec * mat, vec * vec);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
