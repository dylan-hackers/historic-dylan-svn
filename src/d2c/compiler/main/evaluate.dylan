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
        let result-var = make-local-var(builder, #"result", result-type);
        fer-convert(builder, expression,
                    lexenv-for-tlf(tlf), #"assignment", result-var);
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
               fer-evaluate(init-function-region.body,
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

define method fer-evaluate(the-if :: <if-region>, environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  let test-value
    = fer-evaluate-expression(the-if.depends-on.source-exp,
                              environment);
  if(test-value == as(<ct-value>, #f))
    fer-evaluate(the-if.else-region, environment);
  else
    fer-evaluate(the-if.then-region, environment);
  end if;
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

define method fer-evaluate-regions(return :: <return>, more-regions == #(), environment :: <object>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate(return, environment)
end;

// ########## fer-gather-bindings ##########
define generic fer-gather-bindings(region :: <region>, environment :: <object>)
  => (extended-env, potential-value :: false-or(<ct-value>));

define method fer-gather-bindings(compound :: <compound-region>, environment :: <object>)
  => (extended-env, no-value :: #f.singleton);
  fer-gather-regions-bindings(compound.regions, environment)
end;

define method fer-gather-bindings(the-if :: <if-region>, environment :: <object>)
  => (extended-env, no-value :: #f.singleton);
  let test-value
    = fer-evaluate-expression(the-if.depends-on.source-exp,
                              environment);
  if(test-value == as(<ct-value>, #f))
    fer-gather-bindings(the-if.else-region, environment);
  else
    fer-gather-bindings(the-if.then-region, environment);
  end if;
end method;

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
  format(*standard-output*, "fer-gather-regions-bindings %=", regions);
  force-output(*standard-output*);
  fer-gather-regions-bindings(regions.tail, fer-gather-bindings(regions.head, environment))
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
define generic fer-gather-assign-bindings(defs :: false-or(<definition-site-variable>), expr :: <expression>, environment :: <object>)
 => extended-env;

define method fer-gather-assign-bindings(defs :: <definition-site-variable>, expr :: <expression>, environment :: <object>)
 => extended-env;
  let var-value = fer-evaluate-expression(expr, environment);
  append-environment(environment, defs, var-value)
end;

define method fer-gather-assign-bindings(no-more-defs == #f, expr :: <expression>, environment :: <object>)
 => retained-env;
  environment
end;


// ########## fer-evaluate-expression ##########
define generic fer-evaluate-expression(expr :: <expression>, environment :: <object>)
 => result :: <ct-value>;

define method fer-evaluate-expression(expr :: <literal-constant>, environment :: <object>)
 => result :: <ct-value>;
  expr.value
end;

define method fer-evaluate-expression(var :: <abstract-variable>, environment :: <object>)
 => result :: <ct-value>;
  var.environment
end;

define method fer-evaluate-expression(primitive :: <primitive>, environment :: <object>)
 => result :: <ct-value>;
  fer-evaluate-primitive(primitive.primitive-name, primitive.depends-on, environment);
end;


// ########## fer-evaluate-primitive ##########
define generic fer-evaluate-primitive(name :: <symbol>, depends-on :: false-or(<dependency>), environment :: <object>)
 => result :: <ct-value>;


define method fer-evaluate-primitive(name == #"fixnum-+", depends-on :: <dependency>, environment :: <object>)
 => result :: <ct-value>;
 let lhs = fer-evaluate-expression(depends-on.source-exp, environment);
 let rhs = fer-evaluate-expression(depends-on.dependent-next.source-exp, environment);
 
 as(<ct-value>, lhs.literal-value + rhs.literal-value)
end;

 define macro primitive-emulator-definer
  {
    define primitive-emulator (?prim:name) end
  }
  =>
  {
    define method fer-evaluate-primitive(name == "fixnum-" ## ?#"prim", depends-on :: <dependency>, environment :: <object>)
     => result :: <ct-value>;
      let lhs = fer-evaluate-expression(depends-on.source-exp, environment);
      let rhs = fer-evaluate-expression(depends-on.dependent-next.source-exp, environment);
      as(<ct-value>, ?prim(lhs.literal-value, rhs.literal-value))
    end;
  }
end;

define primitive-emulator (\*) end;

// ########## append-environment ##########
define function append-environment(prev-env :: <object>, new-binding, new-value) => new-env;
  method(var)
    if (var == new-binding)
      new-value
    else
      prev-env(var)
    end if;
  end method;
end function;
