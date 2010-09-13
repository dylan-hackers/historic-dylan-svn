module: template-test

define function main (name, arguments)
   // *parser-trace* := *standard-output*;
   // *parser-cache-hits* := #t;
   
   let template =
      with-open-file (stream = arguments.first)
         make(<template>, document: stream);
      end with-open-file;
   
   let bytes = template.persistable-template;
   with-open-file(stream = concatenate(arguments.first, ".bin"), direction: #"output")
      write(stream, bytes)
   end with-open-file;
   
   let template = make(<template>, persisted-template: bytes);

   let alphabet = #[ "Alpha", "Beta", "Gamma" ];
   let output = process-template(template,
         variables: template-vocabulary(template, alphabet),
         operations: template-vocabulary(template, as-uppercase, first));
   format-out("%s", output);
   
   exit-application(0);
end function;

// Invoke our main() function.
main(application-name(), application-arguments());
