module: icfp2000
synopsis: Dylan Hackers entry in the Third Annual ICFP Programming Contest
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  let mat = identity-matrix(dimensions: #[4,4]);

  format-out("%=\n", mat);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
