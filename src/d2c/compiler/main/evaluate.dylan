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
        let result-type = object-ctype();
        let result-var = make-ssa-var(builder, #"result", result-type);
        fer-convert(builder, expression,
                    lexenv-for-tlf(tlf), #"let", result-var);
        let inits = builder-result(builder);

        let name-obj = make(<anonymous-name>, location: tlf.source-location);
        let init-function-region
          = build-function-body(builder, $Default-Policy,
                                tlf.source-location, #f,
                                name-obj, #(), result-type, #t);
        build-region(builder, inits);
        build-return
          (builder, $Default-Policy, tlf.source-location,
           init-function-region, result-var);
        
        end-body(builder);

        format(*standard-output*, "Before optimization:\n");
        dump-fer(component);
        optimize-component(*current-optimizer*, component);
        format(*standard-output*, "After optimization:\n");
        dump-fer(component);
        force-output(*standard-output*);

        format(*standard-output*, "evaluated expression: %=\n",
               fer-evaluate(component,
                            curry(error, "trying to access %= in an empty environment")));
        force-output(*standard-output*);

      otherwise =>
        compiler-error-location(tlf, "only expressions are supported");
    end select;
  end for;
end method evaluate;

// ########## fer-evaluate ##########
define generic fer-evaluate(region :: <region>, environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???

define method fer-evaluate(return :: <return>, environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate-expression(return.depends-on.source-exp, environment)
end;

define method fer-evaluate(compound :: <compound-region>, environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  let regions = compound.regions;
  fer-evaluate-regions(regions.head, regions.tail, environment)
end;

// ########## fer-evaluate-regions ##########
define method fer-evaluate-regions(region :: <region>, more-regions == #(), environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  error("Did not encounter a <return> in control flow...");
end;

define method fer-evaluate-regions(region :: <region>, more-regions :: <list>, environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate-regions(more-regions.head, more-regions.tail, fer-gather-bindings(region, environment))
end;

// ########## fer-gather-bindings ##########
define generic fer-gather-bindings(region :: <region>, environment :: <object>)
  => (extended-env, potential-value :: false-or(<ct-value>));

define method fer-gather-bindings(compound :: <compound-region>, environment :: <object>)
  => (extended-env, no-value :: #f.singleton);
  fer-gather-regions-bindings(compound.regions, environment)
end;


define method fer-gather-bindings(simple :: <simple-region>, environment :: <object>)
  => (extended-env, no-value :: #f.singleton);
  fer-gather-assigns-bindings(simple.first-assign, environment);
end;

// ########## fer-gather-regions-bindings ##########
define generic fer-gather-regions-bindings(regions :: <list>, environment :: <object>)
  => (same-env, potential-value :: false-or(<ct-value>));

define method fer-gather-regions-bindings(no-regions == #(), environment :: <object>)
  => (same-env, no-value :: #f.singleton);
  environment
end;

define method fer-gather-regions-bindings(regions :: <list>, environment :: <object>)
  => (same-env, potential-value :: false-or(<ct-value>));
  environment // ##### for now...
  
end;


// ########## fer-gather-assigns-bindings ##########
define method fer-gather-assigns-bindings(no-assign == #f, environment :: <object>)
  => extended-env;
  environment
end;

define method fer-gather-assigns-bindings(assign :: <abstract-assignment>, environment :: <object>)
  => extended-env;
  let extended-env = fer-gather-assign-bindings(assign.defines, assign.depends-on.source-exp, environment);
  fer-gather-assigns-bindings(assign.next-op, extended-env);
end;

// ########## fer-gather-assign-bindings ##########
define method fer-gather-assign-bindings(defs :: <definition-site-variable>, expr :: <expression>, environment :: <object>)
  => extended-env;
  let var-value = fer-evaluate-expression(expr, environment);
  append-env(environment, defs, var-value)
end;


// ########## fer-evaluate-expression ##########
define generic fer-evaluate-expression(expr :: <expression>, environment :: <object>)
 => result :: <ct-value>;

define method fer-evaluate-expression(expr :: <literal-constant>, environment :: <object>)
 => result :: <ct-value>;
  expr.value
end;


// ########## append-env ##########
define function append-env(prev-env :: <object>, new-biding, new-value) => new-env;
  method(var)
    if (var == new-biding)
      new-value
    else
      prev-env(var)
    end if;
  end method;
end function;
