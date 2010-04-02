module: template-files
synopsis: Processes template files for dylan-topics to fill in.


define variable $topic-template-path :: false-or(<directory-locator>) = #f;

define constant $template-types = #[
   #"all-libraries-topic",
   #"all-modules-topic",
   #"lib-modules-topic",
   #"library-topic",
   #"module-topic"
];

define variable *templates* :: <table> = make(<table>);


define method create-topic-templates () => ()
   verbose-log("Parsing templates");
   for (template-type in $template-types)
      let template-file = make(<file-locator>, directory: $topic-template-path,
            base: as(<string>, template-type), extension: "txt");

      with-open-file (template-stream = template-file)
         block ()
            *templates*[template-type] := make(<template>, document: template-stream);
         exception (err :: <parse-failure>)
            error("Failed to parse template %s at %d: %s", template-file,
                  err.parse-position, err.parse-expected)
         end block
      end with-open-file;
   end for;
end method;


define method discard-topic-templates () => ()
   remove-all-keys!(*templates*)
end method;


define method topic-template (template-type :: <symbol>)
=> (template :: <template>)
   *templates*[template-type]
end method;
