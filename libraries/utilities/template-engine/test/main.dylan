module: template-test

define function main (name, arguments)
   // *parser-trace* := *standard-output*;
   // *parser-cache-hits* := #t;
   
   let template =
      with-open-file (stream = arguments.first)
         make(<template>, document: stream);
      end with-open-file;
   
   let alphabet = #[ "Alpha", "Beta", "Gamma" ];
   let output = process-template(template,
         variables: template-vocabulary(template, alphabet),
         operations: template-vocabulary(template, as-uppercase, first));
   format-out("%s", output);

   exit-application(0);
end function;

// Invoke our main() function.
main(application-name(), application-arguments());
