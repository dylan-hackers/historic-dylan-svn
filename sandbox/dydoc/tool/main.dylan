module: tool

//// Arguments

define argument-parser <my-arg-parser> ()
   regular-arguments files;
   option help?, long: "help", short: "h?";
   synopsis print-help,
      usage: "dydoc <project file>",
      synopsis: "Creates documentation as specified by the project file."
end argument-parser;


//// Main

define function main(name, arguments)

   // Check arguments
   let args = make(<my-arg-parser>);
   parse-arguments(args, arguments);
   if (args.help? | args.files.empty?)
      print-help(args, *standard-output*);
      exit-application(0);
   end;
   
   // TODO: Read project file and create <dydoc-input> objects.
   // TODO: For now, just use a <doc-file>.
   let doc-file = make(<file-stream>, locator: args.files[0], if-does-not-exist: #"signal");
   let doc-tree = create-doc-tree(#(), vector(doc-file), #());
   // TODO: Write doc-tree as HTML or DITA.
   // TODO: For now, just output it.
   print(doc-tree, *standard-output*, pretty?: #t);

   exit-application(0);
end function main;


// Invoke our main() function.
main(application-name(), application-arguments());
