module: main

define variable *interpreter-library* = #f;

define method evaluate(expression :: <string>)
  if(~ *interpreter-library*)
    *interpreter-library* := find-library(#"foo", create: #t);
  end if;
  *Current-Library* := *interpreter-library*;
  *Current-Module*  := find-module(*interpreter-library*, #"dylan-user");
  *top-level-forms* := make(<stretchy-vector>);
  format(*standard-output*, "evaluating %=\n", expression);
  let tokenizer = make(<lexer>, 
                       source: make(<source-buffer>, 
                                    buffer: as(<byte-vector>, expression)),
                       start-line: 0,
                       start-posn: 0);
  parse-source-record(tokenizer);
  for(tlf in *top-level-forms*)
    select(tlf by instance?)
      <expression-tlf> =>
        let expression = tlf.tlf-expression;
        
        format(*standard-output*, "got tlf %=, an expression %=\n", tlf, expression);
        force-output(*standard-output*);
        
        let component = make(<fer-component>);
        let builder = make-builder(component);
        let result-var = make-ssa-var(builder, #"result", object-ctype());
        let result-type = make-values-ctype(#(), #f);
        let name-obj = make(<anonymous-name>, location: tlf.source-location);
        let init-function-region
          = build-function-body(builder, $Default-Policy, tlf.source-location, #f,
                                name-obj, #(), result-type, #t);
        fer-convert(builder, expression, lexenv-for-tlf(tlf), #"let", result-var);
        build-return
          (builder, $Default-Policy, tlf.source-location, init-function-region, result-var);
        end-body(builder);
        
        let inits = builder-result(builder);
        build-region(builder, inits);
        end-body(builder);
        
        unless (instance?(inits, <empty-region>))
          optimize-component(*current-optimizer*, component);
          dump-fer(inits);
        end unless;
        
      otherwise =>
        compiler-error-location(tlf, "only expressions are supported");
    end select;
  end for;
end method evaluate;