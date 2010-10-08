module: template-files
synopsis: Processes template files for dylan-topics to fill in.


define variable *templates* :: <table> = make(<table>);


define method create-templates
   (template-names :: <sequence>, #rest keys, #key, #all-keys)
=> ()
   for (template-type in template-names)
      let template-file = make(<file-locator>, directory: *template-directory*,
            base: as(<string>, template-type), extension: "txt");
      let cache-file = make(<file-locator>, directory: *template-directory*,
            base: as(<string>, template-type), extension: "dat");
      
      let template = valid-cache?(cache-file, template-file)
            & read-cached-template(cache-file, keys);
      
      unless (template)
         template := parse-template(template-file, keys);
         write-cached-template(cache-file, template);
      end unless;
      
      *templates*[template-type] := template;
   end for
end method;


define method valid-cache?
   (cache-file :: <file-locator>, template-file :: <file-locator>)
=> (valid? :: <boolean>)
   cache-file.file-exists?
      & file-property(template-file, #"modification-date")
            <= file-property(cache-file, #"modification-date")
end method;


define method read-cached-template
   (cache-file :: <file-locator>, keys :: <sequence>)
=> (template :: false-or(<template>))
   with-open-file (file = cache-file, element-type: <byte>)
      let file-contents = as(<byte-vector>, file.stream-contents);
      block ()
         apply(make, <template>, persisted-template:, file-contents, keys)
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


define method parse-template
   (template-file :: <file-locator>, keys :: <sequence>)
=> (template :: <template>)
   verbose-log("Parsing template %s", template-file);
   with-open-file (file = template-file)
      let file-contents = make(<string-stream>, contents: file.stream-contents);
      let template-text = make(<canonical-text-stream>, inner-stream: file-contents);
      block ()
         apply(make, <template>, document:, template-text, keys);
      exception (err :: <parse-failure>)
         error("Failed to parse template %s at %d: %s", template-file,
               err.parse-position, err.parse-expected)
      end block
   end with-open-file
end method;


define method discard-templates () => ()
   remove-all-keys!(*templates*)
end method;


define method template-by-name (template-type :: <symbol>)
=> (template :: <template>)
   *templates*[template-type]
end method;
