module: icfp2002
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;


define function main(name, arguments)
  format-out("Hello ICFP2002!\n");
  exit-application(0);
end function main;

main(application-name(), application-arguments());
