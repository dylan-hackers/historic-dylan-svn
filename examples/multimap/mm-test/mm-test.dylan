module:    mm-test
synopsis:  exercises the multimap
author:    Douglas M. Auclair
copyright: (c) 2002, GD license

define constant $vars = make(<multimap>);

define function main(name, arguments)
  $vars[#"metasyntactic"] := "foo";
  $vars[#"metasyntactic"] := "bar";
  $vars[#"metasyntactic"] := "baz";
  $vars[#"metasyntactic"] := "quux";
  $vars[#"cartesian"] := "x";
  $vars[#"loop"] := #"i";
  $vars[#"loop"] := #"i";
  $vars[#"loop"] := #"j";

  format-out("Metasyntactic vars: %=\n", $vars[#"metasyntactic"]);
  format-out("Cartesian vars: %=\n", $vars[#"cartesian"]);
  format-out("Loop vars: %=\n", $vars[#"loop"]);
  format-out("Unit default: %=\n", element($vars, #"bloop", default: 7));
  format-out("Seq default: %=\n", 
             element($vars, #"miu", 
                     default: #("miuumiu", "miiiiiu", "mimmmiu")));
  force-output(*standard-output*);

  block()
    // and an error
    $vars[#"floop"];
  exception(cond :: <condition>)
    format-out("Encountered %= when attempting $vars[#\"floop\"]\n", cond);
  end;

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
