Author:    Andy Armstrong
Synopsis:  A Dylan application to play around in
Copyright: 1997 Harlequin Group plc. All rights reserved.
Module:    dylan-playground

define method main ()
  notify-user("The Dylan playground.\n\n"
	      "If you see this message you have probably started the "
	      "dylan-playground project unintentionally.  Use the "
	      "Open Playground command in the Help menu to interact "
	      "with the dylan-playground instead.");
end method main;

main();
