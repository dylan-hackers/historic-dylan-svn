module: main
rcs-header: $Header: /scm/cvs/src/d2c/compiler/main/evaluate.dylan,v 1.1.2.30 2002/07/29 18:25:30 gabor Exp $
copyright: see below

//======================================================================
//
// Copyright (c) 2002  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// This file contains an interpreter that can give back <ct-values> for
// certain FER constructions.


define variable *interpreter-library* = #f;

define method evaluate(expression :: <string>)
  if(~ *interpreter-library*)
    *interpreter-library* := find-library(#"foo", create: #t); // ### FIXME: arbitrary name
    seed-representations();
    inherit-slots();
    inherit-overrides();
    layout-instance-slots();
  end if;
  *Current-Library* := *interpreter-library*;
  *Current-Module*  := find-module(*interpreter-library*, #"dylan-user");
  *top-level-forms* := make(<stretchy-vector>);
  format(*debug-output*, "evaluating %=\n", expression);
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
        
        format(*debug-output*, "got tlf %=, an expression %=\n", tlf, expression);
        force-output(*debug-output*);
        
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

        format(*debug-output*, "\n\nBefore optimization:\n");
        dump-fer(component);
        optimize-component(*current-optimizer*, component);
        format(*debug-output*, "\n\nAfter optimization:\n");
        dump-fer(component);
        force-output(*debug-output*);

        format(*debug-output*, "\n\nevaluated expression: %=\n",
               fer-evaluate(init-function-region.body,
                            curry(error, "trying to access %= in an empty environment")));
        force-output(*debug-output*);

      otherwise =>
        compiler-error-location(tlf, "only expressions are supported");
    end select;
  end for;
end method evaluate;


define constant <interpreter-environment> :: <type> = <object>;




// ##############################################################################
// ##############################################################################
// ###################### fer-evaluate-the-new-generation #######################
// ##############################################################################
// ##############################################################################



// ##############################################################################
// ##############################################################################
// ##############################################################################
// ##############################################################################


// ########## fer-evaluate ##########
define generic fer-evaluate(region :: <region>, environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???

define method fer-evaluate(return :: <return>, environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate-expression(return.depends-on.source-exp, environment)
end;

define method fer-evaluate(compound :: <compound-region>, environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  let regions = compound.regions;
  fer-evaluate-regions(regions.head, regions.tail, environment)
end;

define method fer-evaluate(the-if :: <if-region>, environment :: <interpreter-environment>)
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
define method fer-evaluate-regions(region :: <region>, more-regions == #(), environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  error("Did not encounter a <return> in control flow...");
end;

define method fer-evaluate-regions(region :: <region>, more-regions :: <list>, environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate-regions(more-regions.head, more-regions.tail, fer-gather-bindings(region, environment))
end;

define method fer-evaluate-regions(return :: <return>, more-regions == #(), environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  fer-evaluate(return, environment)
end;

define method fer-evaluate-regions(exit :: <exit>, more-regions == #(), environment :: <interpreter-environment>)
  => ct-value :: <ct-value>; // multivalues???
  fer-gather-bindings(exit, environment)
end;

// ########## fer-gather-bindings ##########
define generic fer-gather-bindings(region :: <region>, environment :: <interpreter-environment>)
  => (extended-env, potential-value :: false-or(<ct-value>));

define method fer-gather-bindings(compound :: <compound-region>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<compound-region>} %=\n", compound);
//  force-output(*debug-output*);
  fer-gather-regions-bindings(compound.regions, environment)
end;

define method fer-gather-bindings(the-if :: <if-region>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<if-region>} %=\n", the-if);
//  force-output(*debug-output*);
  let test-value
    = fer-evaluate-expression(the-if.depends-on.source-exp,
                              environment);
  if(test-value == as(<ct-value>, #f))
    fer-gather-bindings(the-if.else-region, environment);
  else
    fer-gather-bindings(the-if.then-region, environment);
  end if;
end method;

define class <exit-condition>(<condition>)
  constant slot exit-block :: <block-region-mixin>, required-init-keyword: block:;
  constant slot exit-environment :: <interpreter-environment>, required-init-keyword: environment:;
end class <exit-condition>;

define method fer-gather-bindings(block-region :: <block-region>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<block-region>} %=\n", block-region);
//  force-output(*debug-output*);
  block ()
    fer-gather-bindings(block-region.body, environment)
  exception (exit :: <exit-condition>, test: method(exit :: <exit-condition>)
                                               exit.exit-block == block-region
                                             end method)
    exit.exit-environment;
  end block
end;

define method fer-gather-bindings(loop :: <loop-region>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<loop-region>} %=\n", loop);
//  force-output(*debug-output*);
  local method repeat(environment :: <interpreter-environment>)
      repeat(fer-gather-bindings(loop.body, environment))
    end method;
  repeat(environment);
end;

define method fer-gather-bindings(exit :: <exit>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<exit>} %=\n", exit);
//  force-output(*debug-output*);
  signal(make(<exit-condition>, block: exit.block-of, environment: environment));
end;



define method fer-gather-bindings(simple :: <simple-region>, environment :: <interpreter-environment>)
  => (extended-env, no-value :: #f.singleton);
//  format(*debug-output*, "fer-gather-bindings{<simple>} %=\n", simple);
//  force-output(*debug-output*);
  fer-gather-assigns-bindings(simple.first-assign, environment);
end;

// ########## fer-gather-regions-bindings ##########
define generic fer-gather-regions-bindings(regions :: <list>, environment :: <interpreter-environment>)
  => (same-env, potential-value :: false-or(<ct-value>));

define method fer-gather-regions-bindings(no-regions == #(), environment :: <interpreter-environment>)
  => (same-env, no-value :: #f.singleton);
  environment
end;

define method fer-gather-regions-bindings(regions :: <list>, environment :: <interpreter-environment>)
  => (same-env, potential-value :: false-or(<ct-value>));
//  format(*debug-output*, "fer-gather-regions-bindings %=\n", regions);
//  force-output(*debug-output*);
  
  let head = regions.head;
  if (instance?(head, <compound-region>))
  
    let (env, value) = fer-gather-bindings(head, environment);
    if (value)
      compiler-warning("Well, I thought that RETURN[110](result[122]) can only appear just at the end of <function-region>s\n");
      values(environment, value)
    else
      fer-gather-regions-bindings(regions.tail, fer-gather-bindings(head, environment))
    end
  else
    fer-gather-regions-bindings(regions.tail, fer-gather-bindings(head, environment))
  end
end;


// ########## fer-gather-assigns-bindings ##########
define method fer-gather-assigns-bindings(no-assign == #f, environment :: <interpreter-environment>)
  => extended-env;
  environment
end;

define method fer-gather-assigns-bindings(assign :: <abstract-assignment>, environment :: <interpreter-environment>)
  => extended-env;
  let extended-env = fer-gather-assign-bindings(assign.defines, assign.depends-on.source-exp, environment);
  fer-gather-assigns-bindings(assign.next-op, extended-env);
end;

// ########## fer-gather-assign-bindings ##########
define generic fer-gather-assign-bindings(defs :: false-or(<definition-site-variable>), expr :: <expression>, environment :: <interpreter-environment>)
 => extended-env;

define method fer-gather-assign-bindings(defs :: <ssa-variable>, expr :: <expression>, environment :: <interpreter-environment>)
 => extended-env;
  let var-value = fer-evaluate-expression(expr, environment);
  append-environment(environment, defs, var-value)
end;

define method fer-gather-assign-bindings(defs :: <initial-definition>, expr :: <expression>, environment :: <interpreter-environment>)
 => extended-env;
  let var-value = fer-evaluate-expression(expr, environment);
  append-environment(environment, defs.definition-of, var-value) // ### we should perhaps take in account that this var may already have been recorded in the env...
end;

define method fer-gather-assign-bindings(no-more-defs == #f, expr :: <expression>, environment :: <interpreter-environment>)
 => retained-env;
  environment
end;

// ########## fer-evaluate-call ##########
define generic fer-evaluate-call(func :: <abstract-function-literal>,
                                 operands :: false-or(<dependency>),
                                 callee-environment :: <interpreter-environment>)
 => result :: <ct-value>;

define method fer-evaluate-call(func :: <method-literal>, operands :: false-or(<dependency>), callee-environment :: <interpreter-environment>)
 => result :: <ct-value>;
//  fer-evaluate(func.main-entry.body, curry(error, "no, there is no variable %= in environment")) // #####  func.main-entry.body

  let prologue = func.main-entry.prologue;
  
  let prologue-assignment :: <abstract-assignment> = prologue.dependents.dependent;
  let vars-to-be-bound :: false-or(<definition-site-variable>) = prologue-assignment.defines;

  local method prologue-environment(vars :: false-or(<definition-site-variable>),
                                    operands :: false-or(<dependency>),
                                    #key to-extend :: <interpreter-environment> = curry(error, "no, there is no variable %= in environment"))
  	 => prologue-environment :: <interpreter-environment>;
  	  if (vars)
  	    operands | error("too few arguments passed to <method-literal>");
  	    prologue-environment(vars.definer-next, operands.dependent-next,
                                 to-extend: append-environment(to-extend, vars,
                                                               fer-evaluate-expression(operands.source-exp, callee-environment)));
  	  else
  	    to-extend
  	  end if;
        end method;

  fer-evaluate(func.main-entry.body, prologue-environment(vars-to-be-bound, operands))
end;

// ########## fer-evaluate-expression ##########
define generic fer-evaluate-expression(expr :: <expression>, environment :: <interpreter-environment>)
 => result :: <ct-value>;

define method fer-evaluate-expression(expr :: <literal-constant>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  expr.value
end;

define method fer-evaluate-expression(expr :: <method-literal>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  expr.ct-function
end;

define method fer-evaluate-expression(expr :: <known-call>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
      let func :: <method-literal> = expr.depends-on.source-exp;
      let args = expr.depends-on.dependent-next;
      
      fer-evaluate-call(func, args, environment);
end;

define method fer-evaluate-expression(var :: <abstract-variable>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  var.environment
end;

define method fer-evaluate-expression(primitive :: <primitive>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  fer-evaluate-primitive(primitive.primitive-name, primitive.depends-on, environment);
end;

define method fer-evaluate-expression(prologue :: <prologue>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  let prologue-assignment :: <abstract-assignment> = prologue.dependents.dependent;
  let vars-to-be-bound :: false-or(<definition-site-variable>) = prologue-assignment.defines;
  environment(vars-to-be-bound); // we only support one variable for now...###
end;


define method fer-evaluate-expression(slot-ref :: <heap-slot-ref>, environment :: <interpreter-environment>)
 => result :: <ct-value>;
  let slot-info = slot-ref.slot-info;
  let obj = slot-ref.depends-on.dependent-next.source-exp;
  
  slot-info.slot-read-only? | error("cannot evaluate writeable <heap-slot-ref> of %=", obj);
  
  if (slot-info.slot-init-value
      & slot-info.slot-init-value ~== #t)
    slot-info.slot-init-value
  else
    // we need to eval the init function too??? ### side-effects?
    
    let obj-value = fer-evaluate-expression(obj, environment);
    obj-value // for now...
  end;
  
end;


// ########## fer-evaluate-primitive ##########
define generic fer-evaluate-primitive(name :: <symbol>, depends-on :: false-or(<dependency>), environment :: <interpreter-environment>)
 => result :: <ct-value>;

define macro primitive-emulator-definer
  {
    define primitive-emulator ?:name end
  }
  =>
  {
    define method fer-evaluate-primitive(name == "fixnum-" ## ?#"name", depends-on :: <dependency>, environment :: <interpreter-environment>)
     => result :: <ct-value>;
      let lhs = fer-evaluate-expression(depends-on.source-exp, environment);
      let rhs = fer-evaluate-expression(depends-on.dependent-next.source-exp, environment);
      as(<ct-value>, ?name(lhs.literal-value, rhs.literal-value))
    end;
  }

  {
    define primitive-emulator (?:name) end
  }
  =>
  {
    define method fer-evaluate-primitive(name == ?#"name", depends-on :: <dependency>, environment :: <interpreter-environment>)
     => result :: <ct-value>;
      let lhs :: <eql-ct-value> = fer-evaluate-expression(depends-on.source-exp, environment);
      let rhs :: <eql-ct-value> = fer-evaluate-expression(depends-on.dependent-next.source-exp, environment);
      as(<ct-value>, ?name(lhs, rhs))
    end;
  }
end;

define primitive-emulator \+ end;
define primitive-emulator \- end;
define primitive-emulator \* end;
define primitive-emulator \< end;
define primitive-emulator \= end;
define primitive-emulator logior end;
define primitive-emulator logxor end;
define primitive-emulator logand end;
define primitive-emulator (\==) end;

// ########## append-environment ##########
define function append-environment(prev-env :: <interpreter-environment>, new-binding, new-value) => new-env;

//  format(*debug-output*, "append-environment %= %= \n", new-binding, new-value);
//  force-output(*debug-output*);


  method(var)
    if (var == new-binding)
      new-value
    else
      prev-env(var)
    end if;
  end method;
end function;
