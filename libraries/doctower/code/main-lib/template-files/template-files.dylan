module: template-files
synopsis: Processes template files for dylan-topics to fill in.


define variable $topic-template-path :: false-or(<directory-locator>) = #f;

define constant $template-types = #[
   #"all-catalog-topics",
   #"lib-catalog-topics",
   #"mod-catalog-topics",
   #"library-topic",
   #"module-topic",
   #"class-topic",
   #"function-topic",
   #"generic-topic",
   #"method-topic",
   #"constant-topic",
   #"variable-topic",
   #"macro-topic",
   #"unbound-topic"
];

define variable *templates* :: <table> = make(<table>);


define method create-topic-templates () => ()
   verbose-log("Reading cached templates");
   for (template-type in $template-types)
      let template-file = make(<file-locator>, directory: $topic-template-path,
            base: as(<string>, template-type), extension: "txt");
      let cache-file = make(<file-locator>, directory: $topic-template-path,
            base: as(<string>, template-type), extension: "dat");
      
      let template = valid-cache?(cache-file, template-file)
            & read-cached-template(cache-file);
      
      unless (template)
         template := parse-template(template-file);
         write-cached-template(cache-file, template);
      end unless;
      
      *templates*[template-type] := template;
   end for
end method;


define method valid-cache? (cache-file :: <file-locator>, template-file :: <file-locator>)
=> (valid? :: <boolean>)
   cache-file.file-exists?
      & file-property(template-file, #"modification-date")
            <= file-property(cache-file, #"modification-date")
end method;


define method read-cached-template (cache-file :: <file-locator>)
=> (template :: false-or(<template>))
   with-open-file (file = cache-file, element-type: <byte>)
      let file-contents = as(<byte-vector>, file.stream-contents);
      block ()
         make(<template>, persisted-template: file-contents)
      exception (<error>)
         #f
      end block
   end with-open-file
end method;


define method write-cached-template
   (cache-file :: <file-locator>, template :: <template>)
=> ()
   with-open-file (file = cache-file, direction: #"output", element-type: <byte>)
      write(file, template.persistable-template)
   end with-open-file
end method;


define method parse-template (template-file :: <file-locator>)
=> (template :: <template>)
   verbose-log("Parsing template %s", template-file);
   with-open-file (file = template-file)
      let file-contents = make(<string-stream>, contents: file.stream-contents);
      let template-text = make(<canonical-text-stream>, inner-stream: file-contents);
      block ()
         make(<template>, document: template-text);
      exception (err :: <parse-failure>)
         error("Failed to parse template %s at %d: %s", template-file,
               err.parse-position, err.parse-expected)
      end block
   end with-open-file
end method;


define method discard-topic-templates () => ()
   remove-all-keys!(*templates*)
end method;


define method topic-template (template-type :: <symbol>)
=> (template :: <template>)
   *templates*[template-type]
end method;
