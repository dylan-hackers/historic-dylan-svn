module: main

//// Arguments

define argument-parser <my-arg-parser> ()
   regular-arguments files;
   option toc-pattern = "toc", long: "toc", short: "t",
      kind: <optional-parameter-option-parser>,
      description: "Extension of table of contents files";
   option cfg-pattern = "cfg", long: "cfg", short: "c",
      kind: <optional-parameter-option-parser>,
      description: "Extension of configuration files";
   option doc-pattern = "txt", long: "doc", short: "d",
      kind: <optional-parameter-option-parser>,
      description: "Extension of documentation text files";
   option title = "Untitled", long: "title",
      kind: <optional-parameter-option-parser>,
      description: "Title of documentation";
   option tab-size = "8", long: "tabs",
      kind: <optional-parameter-option-parser>,
      description: "Tab size";
   option help?, long: "help", short: "h?",
      description: "Help";
   synopsis print-help,
      usage: "doctower [options] <files>",
      description: "Creates documentation from files."
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

   // TODO: Expand wildcards in args.files.
   let toc-files = make(<stretchy-vector>);
   let doc-files = make(<stretchy-vector>);
   let src-files = make(<stretchy-vector>);
   for (filename in args.files)
      let loc = as(<file-locator>, filename);
      block ()
         // if-does-not-exist: #f would be preferable, but not implemented.
         let file = make(<file-stream>, locator: loc, if-does-not-exist: #"signal");
         select (loc.locator-extension by case-insensitive-equal?)
            args.doc-pattern => doc-files := add!(doc-files, file);
            args.toc-pattern => toc-files := add!(toc-files, file);
            ("dylan", "dyl", "lid") => src-files := add!(src-files, file);
            otherwise => close(file);
         end select;
      exception (err :: <file-does-not-exist-error>)
         file-not-found(#f, filename: filename)
      end block;
   end for;

   block ()
      let doc-tree = create-doc-tree(toc-files, doc-files, src-files);
      // TODO: Write doc-tree as HTML or DITA.
      // TODO: For now, just output it.
      print(doc-tree, *standard-output*, pretty?: #t);
   cleanup
      do(close, toc-files);
      do(close, doc-files);
      do(close, src-files);
   end block;

   exit-application(0);
end function main;


// Invoke our main() function with error handlers.
begin
   let handler <user-visible-error> =
         method (cond, next)
            report-condition(cond, *standard-error*);
            new-line(*standard-error*);
            exit-application(cond.error-code);
         end;
   main(application-name(), application-arguments());
end
