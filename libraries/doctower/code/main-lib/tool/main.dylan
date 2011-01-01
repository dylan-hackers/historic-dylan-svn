module: main

//// Arguments

// TODO: Make --name-list a separate task from doc gen.

define argument-parser <my-arg-parser> ()
   regular-arguments files;
   option output-path, " <directory>",
      format-to-string("Documentation path [%s]", *output-directory*),
      long: "output-dir", short: "o", kind: <parameter-option-parser>;
   option output-formats, " html|dita",
      format-to-string("Documentation format [%s]", *output-types*.item-string-list),
      long: "format", short: "f", kind: <repeated-parameter-option-parser>;
   option package-title, " <title>",
      format-to-string("Documentation title [%s]", *package-title*),
      long: "title", short: "t", kind: <parameter-option-parser>;
   option cfg-pattern, " <ext>",
      format-to-string("Configuration files [%s]", *config-file-extension*),
      long: "cfg", short: "c", kind: <parameter-option-parser>;
   option toc-pattern, " <ext>",
      format-to-string("Table of contents files [%s]", *contents-file-extension*),
      long: "toc", kind: <parameter-option-parser>;
   option doc-pattern, " <ext>",
      format-to-string("Documentation text files [%s]", *topic-file-extension*),
      long: "doc", kind: <parameter-option-parser>;
   option template-path, " <directory>",
      format-to-string("Template files [%s]", *template-directory*),
      long: "templates", short: "T", kind: <parameter-option-parser>;
   option api-list-filename, " <filename>",
      "Write fully qualified API names to file",
      long: "names", short: "n", kind: <parameter-option-parser>;
   option ignore-comments?,
      "Ignore source code documentation comments",
      long: "no-comment", short: "N";
   // option tab-size = "8",
   //    "=<n>",
   //    "Tab size [8]",
   //    long: "tabsize", kind: <parameter-option-parser>;
   option disabled-warnings, " <nn>",
      "Hide warning message",
      long: "no-warn", short: "w", kind: <repeated-parameter-option-parser>;
   option stop-on-errors?,
      "Stop on first error or warning",
      long: "stop";
   option debug-features, " <feature>",
      "Enable developer debugging feature",
      long: "debug", short: "D", kind: <repeated-parameter-option-parser>;
   option quiet?,
      "Hide progress messages",
      long: "quiet", short: "q";
   option help?,
      "Show this help message and exit",
      long: "help";
   option version?,
      "Show program version and exit",
      long: "version";
   synopsis print-help,
      usage: "doctower [options] <files>",
      description: "\n"
         "Creates Dylan API documentation from files. Files may include configuration\n"
         "files, table of contents files, documentation text files, Dylan source code\n"
         "files, and Dylan LID files."
end argument-parser;


//// Main

define constant $disabled-warnings = make(<stretchy-vector>);
define variable *stop-on-errors?* :: <boolean> = #f;
define variable *error-code* :: false-or(<integer>) = #f;

define function main (name, arguments)

   // Retrieve arguments

   let args = make(<my-arg-parser>);
   let good-options? = parse-arguments(args, arguments);
   
   case
      ~good-options? =>
         error-in-command-arguments();
      args.help? =>
         print-help(args, *standard-output*);
         format-out("\nDeveloper debugging features are %s.\n",
               $debug-features.item-string-list);
         exit-application(0);
      args.version? =>
         format-out("Doctower 1.0\nby Dustin Voss\n");
         exit-application(0);
      args.files.empty? =>
         no-files-in-command-arguments();
   end case;
   
   block()
      map-into($disabled-warnings, string-to-integer, args.disabled-warnings)
   exception (e :: <error>)
      error-in-command-option(option: "--no-warn");
   end block;

   *stop-on-errors?* := args.stop-on-errors?;
   *verbose?* := ~args.quiet?;
   *debug-features* := map(curry(as, <symbol>), args.debug-features);
   unless (every?(rcurry(member?, $debug-features), *debug-features*))
      error-in-command-option(option: "--debug");
   end unless;
   
   // Retrieve and process config files
   
   let file-locators = map(curry(as, <file-locator>), args.files);
   *config-file-extension* := args.cfg-pattern | *config-file-extension*;
   let cfg-files = choose(
         method (loc :: <file-locator>) => (cfg-locator? :: <boolean>)
            case-insensitive-equal?(*config-file-extension*, loc.locator-extension)
         end method, file-locators);
   
   // TODO: Process config files. We can have multiple config files, but they
   // cannot collectively define a config more than once.

   // Override configs with command-line options

   *contents-file-extension* := args.toc-pattern | *contents-file-extension*;
   *topic-file-extension* := args.doc-pattern | *topic-file-extension*;
   *package-title* := args.package-title | *package-title*;
   *scan-only?* := args.ignore-comments? | *scan-only?*;

   if (args.template-path)
      *template-directory* := as(<directory-locator>, args.template-path)
   end if;
   
   if (args.output-path)
      *output-directory* := as(<directory-locator>, args.output-path)
   end if;
   
   if (~args.output-formats.empty?)
      *output-types* := map(curry(as, <symbol>), args.output-formats);
   end if;
   
   if (args.api-list-filename)
      *api-list-file* := as(<file-locator>, args.api-list-filename)
   end if;

   // Classify input files

   let toc-files = make(<stretchy-vector>);
   let doc-files = make(<stretchy-vector>);
   let src-files = make(<stretchy-vector>);
   for (loc in file-locators)
      select (loc.locator-extension by case-insensitive-equal?)
         *config-file-extension*
            => #f /* Already dealt with these */;
         *topic-file-extension*
            => doc-files := add!(doc-files, loc);
         *contents-file-extension*
            => toc-files := add!(toc-files, loc);
         ("dylan", "dyl", "lid")
            => src-files := add!(src-files, loc);
         otherwise
            => file-type-not-known(filename: as(<string>, loc));
      end select;
   end for;
   
   // Build documentation

   let doc-tree = create-doc-tree(toc-files, doc-files, src-files);
   
   unless (*error-code*)
      create-output-files(doc-tree)
   end unless;

   if (debugging?(#"doc-tree"))
      log("--- Doc tree ---");
      print(doc-tree, *standard-output*, pretty?: #t);
      new-line(*standard-output*);
   end if;

   exit-application(*error-code* | 0);
end function main;


// Invoke our main() function with error handlers.
begin
   let handler <user-visible-error> =
         method (cond, next)
            report-condition(cond, *standard-error*);
            new-line(*standard-error*);
            force-output(*standard-error*);
            when (*stop-on-errors?*)
               exit-application(cond.error-code);
            end when;
            *error-code* := *error-code* | cond.error-code;
            signal(make(<skip-error-restart>, condition: cond));
         end method;

   let handler <user-visible-warning> =
         method (cond, next)
            case
               member?(cond.error-code, $disabled-warnings) =>
                  #f;
               *stop-on-errors?* =>
                  report-condition(cond, *standard-error*);
                  new-line(*standard-error*);
                  force-output(*standard-error*);
                  exit-application(cond.error-code);
               otherwise =>
                  report-condition(cond, *standard-output*);
                  new-line(*standard-output*);
                  force-output(*standard-output*);
            end case
         end method;

   let handler <skip-error-restart> =
         method (cond, next)
            exit-application(*error-code*);
         end method;
         
   *default-line-length* := 132;
   main(application-name(), application-arguments());
end
