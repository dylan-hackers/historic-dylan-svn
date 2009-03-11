Synopsis: upgrading of calls to more efficient entry points
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Module:   dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// SIGNATURE ACCESSORS THAT FAVOR MODEL OVER DEFINITION
/// TO AVOID LOADING BOTH FROM DATABASE

define macro best-function-signature-accessor-aux-definer
 { define best-function-signature-accessor-aux ?:name specd ?spec-name:name modeled ?model-name:name }
    => { define method "best-function-" ## ?name (function :: <&function>)
	     let sig = ^function-signature(function);
	     if (sig)
	       "^signature-" ## ?model-name(sig)
	     else
	       "spec-argument-" ## ?spec-name(signature-spec(function))
	     end if
	 end method }
end macro;

define macro best-function-signature-accessor-definer
 { define best-function-signature-accessor ?:name }
    => { define best-function-signature-accessor-aux ?name specd ?name modeled ?name }
end macro;

define best-function-signature-accessor-aux specializers 
  specd required-variable-specs modeled required;
define best-function-signature-accessor optionals?;
define best-function-signature-accessor key?;
define best-function-signature-accessor rest?;
define best-function-signature-accessor all-keys?;
define best-function-signature-accessor number-keys;
define best-function-signature-accessor number-required;


//// protocol

define generic maybe-upgrade-call
    (c :: <function-call>, f :: <&callable-object>) => (upgraded? :: <boolean>);

define generic maybe-upgrade-required-call
    (c :: <function-call>, f :: <&callable-object>) => (upgraded? :: <boolean>);

define generic maybe-upgrade-rest-call
    (c :: <function-call>, f :: <&callable-object>) => (upgraded? :: <boolean>);

define generic maybe-upgrade-keyword-call
    (c :: <function-call>, f :: <&callable-object>) => (upgraded? :: <boolean>);

//// Simple entry-point pass

// define compilation-pass analyze-calls,
//   visit: computations,
//   optimization: low,
//   before: single-value-propagation;

define method analyze-calls (c :: <computation>)
  #f
end method;

define variable *call-upgrading?* = #t;

define method analyze-calls (c :: <function-call>)
  // If what's being called is not a valid function, or there is some
  // clear incompatibility between the arguments and the function,
  // don't attempt to do anything with the call.
  let ef = call-effective-function(c);
  let call-ok? = maybe-check-function-call(c);
  if (*call-upgrading?*)
    if (call-ok? & ef)
      maybe-upgrade-call(c, ef)
    elseif (*profile-all-calls?* 
	      & instance?(c, <simple-call>) & ~instance?(c, <engine-node-call>) 
	      & instance?(ef, <&generic-function>))
      upgrade-gf-to-profiling-call-site-cache(c, ef, #[]);
    else 
      #f
    end if
  else 
    #f
  end if;
end method;

define method maybe-upgrade-call 
    (c :: <function-call>, f :: <&callable-object>) => (res :: <boolean>)
  maybe-upgrade-function-call(c, f)
end method;

define method maybe-upgrade-function-call
    (c :: <function-call>, f :: <&callable-object>) => (res :: singleton(#f))
  #f
end method;

define method maybe-upgrade-function-call
    (c :: <simple-call>, f :: <&generic-function>) => (res :: <boolean>)
  unless (call-congruent?(c))
    // if (*colorize-dispatch*)
    //   color-dispatch(c, #"lambda-call") // TODO: SHOULD BE CONGRUENT-CALL
    // end;
    if (best-function-optionals?(f))
      maybe-upgrade-rest-call(c, f)
    else
      maybe-upgrade-required-call(c, f)
    end if
  end unless;
end method;

define method maybe-upgrade-function-call
    (c :: <function-call>, f :: <&lambda>) => (res :: <boolean>)
  when (*colorize-dispatch*)
    color-dispatch(c, #"lambda-call")
  end when;
  if (best-function-key?(f))
    maybe-upgrade-keyword-call(c, f)
  elseif (best-function-rest?(f))
    maybe-upgrade-rest-call(c, f)
  else
    maybe-upgrade-required-call(c, f)
  end if
end method;

define inline method maybe-rest-references
    (env :: <environment>, ref) => (res :: false-or(<argument-sequence>))
  maybe-vector-element-references(ref)
end method;

define method maybe-upgrade-function-call (c :: <function-call>, f :: <&iep>)
 => (res :: <boolean>)
  #f
end method;

define method maybe-upgrade-required-call 
    (c :: <function-call>, f :: <&callable-object>)
 => (res :: singleton(#f))
  #f
end method;

define method maybe-upgrade-rest-call
    (c :: <function-call>, f :: <&callable-object>)
 => (res :: singleton(#f))
  #f
end method;

define method maybe-upgrade-keyword-call
    (c :: <function-call>, f :: <&callable-object>)
 => (res :: singleton(#f))
  #f
end method;

define inline method do-callers
    (function :: <function>, f :: <&lambda-or-code>) => ()
  for (use in f.users)
    // TODO: MAYBE REFINE THE FOLLOWING
    let ref = 
      if (instance?(use, <make-closure>))
        use.temporary
      else
        use
      end if;
    for (use-use in ref.users)
      if (instance?(use-use, <call>))
        function(use-use);
      end if;
    end for;
  end for;
end method;

////
//// IEP UPGRADES
////

// define method maybe-refine-call-temporary! 
//     (f :: <&lambda>, t :: <multiple-value-temporary>) => ()
// //  let sig-spec = signature-spec(f);
// // the mv-temp slots now record what is desired (the <value-context>)
// // not what is received.
// //  t.required-values := spec-value-number-required(sig-spec);
// //  t.rest-values?    := spec-value-rest?(sig-spec);
// end method;

// define method maybe-refine-call-temporary! 
//     (f :: <&function>, t :: false-or(<temporary>)) => ()
// end method;

define method upgrade-to-congruent-call! 
    (c :: <simple-call>, f :: <&generic-function>)
  // maybe-refine-call-temporary!(f, c.temporary);
  call-congruent?(c) := #t;
  // re-optimize(c);
  // do-callers(re-optimize, f);
end method;

define method upgrade-to-congruent-call! (c :: <simple-call>, f :: <&lambda>)
  // maybe-refine-call-temporary!(f, c.temporary);
  call-congruent?(c) := #t;
  call-iep?(c)       := #t;
  re-optimize(c);
  do-callers(re-optimize, f.iep);
end method;

define method upgrade-to-congruent-call! (c :: <apply>, f :: <&function>)
  let (congruent-call, congruent-call-tmp)
    = make-with-temporary(c.environment, <simple-call>,
                          function:  c.function,
                          arguments: c.arguments,
                          temporary-class: call-temporary-class(c));
  replace-computation!(c, congruent-call, congruent-call, congruent-call-tmp);
  upgrade-to-congruent-call!(congruent-call, f);
end method;

define method upgrade-to-congruent-call! (c :: <method-apply>, f :: <&function>)
  let (congruent-call, congruent-call-tmp)
    = make-with-temporary(c.environment, <method-call>,
                          function:  c.function,
                          arguments: c.arguments,
                          next-methods: c.next-methods,
                          temporary-class: call-temporary-class(c));
  replace-computation!(c, congruent-call, congruent-call, congruent-call-tmp);
  upgrade-to-congruent-call!(congruent-call, f);
end method;

define method maybe-upgrade-required-call (c :: <simple-call>, f :: <&lambda>)
 => (res :: singleton(#t))
  // format-out("UPGRADING %=\n", f);
  // check-required-arguments(c, f);
  upgrade-to-congruent-call!(c, f);
  #t
end method;

define method maybe-upgrade-required-call (c :: <simple-call>, f :: <&generic-function>)
 => (res :: singleton(#t))
  upgrade-to-congruent-call!(c, f);
  #T
end method;

define method generate-stack-vector 
    (env :: <environment>, arguments :: <simple-object-vector>)
 => (vector-c :: <computation>, vector-t :: <temporary>)
  let (rest-c, rest-t)
  = make-with-temporary
      (env, <stack-vector>, temporary-class: <stack-vector-temporary>,
       arguments: arguments);
  rest-t.number-values := size(arguments);
  re-optimize(rest-c);
  values(rest-c, rest-t)
end method;

define method maybe-upgrade-rest-call
    (call :: <apply>, func :: <&lambda>) => (res :: <boolean>)
  // Only upgrade when the positional arguments exactly match the expected
  // required arguments, and the final argument can be inferred to be a
  // simple object vector.
  let number-required = best-function-number-required(func);
  let args = call.arguments;
  let arg-count = args.size;
  let cache = library-type-cache(current-library-description());
  if (arg-count - 1 == number-required)
        // Apply computations aren't generated unless the following condition
        // is satisfied - see calls.dylan
        // & guaranteed-joint?
        //    (type-estimate(last(args), cache), 
        //       dylan-value(#"<simple-object-vector>"))
    upgrade-to-congruent-call!(call, func);
    #t
  else
    #f
  end;
end method;

define method function-uses-rest? (func :: <&lambda>) => (well? :: <boolean>)
  lambda-rest?(func)
end method;

define method function-uses-rest? (func :: <&generic-function>) => (well? :: <boolean>)
  #t
end method;

define function upgrade-rest-call
    (call :: <simple-call>, func :: <&function>) => (res :: singleton(#t))
  let number-required = best-function-number-required(func);
  if (number-required = call.arguments.size)
    // Construct an empty #rest parameter
    let new-arguments :: <simple-object-vector>
      = make(<vector>, size: number-required + 1);
    for (i :: <integer> from 0 below number-required)
      new-arguments[i] := call.arguments[i];
    end for;
    let rest-t = make-object-reference(#[]);
    new-arguments[number-required] := rest-t;
    add-user!(rest-t, call);
    call.arguments := new-arguments;
    upgrade-to-congruent-call!(call, func);
  else 
    let rest-t
      = if (function-uses-rest?(func))
          let (rest-c, rest-t)
            = generate-stack-vector
                (call.environment, 
                 copy-sequence(call.arguments, start: number-required));
	  insert-computation-before!(call, rest-c);
          rest-t
        else
          make-object-reference(#[])
        end if;
    let new-arguments :: <simple-object-vector>
      = make(<vector>, size: number-required + 1);
    for (i :: <integer> from 0 below number-required)
      new-arguments[i] := call.arguments[i];
    end for;
    new-arguments[number-required] := rest-t;
    add-user!(rest-t, call);
    for (i :: <integer> from number-required below call.arguments.size)
      remove-user!(call.arguments[i], call);
    end for;
    call.arguments := new-arguments;
    upgrade-to-congruent-call!(call, func);
  end;
  #t
end function;

define method maybe-upgrade-rest-call
    (call :: <simple-call>, func :: <&lambda>) => (res :: singleton(#t))
  if (call-congruent?(call))
    upgrade-to-congruent-call!(call, func);
  else
    upgrade-rest-call(call, func)
  end if
end method;

define method maybe-upgrade-rest-call
    (call :: <simple-call>, func :: <&generic-function>) => (res :: singleton(#t))
  upgrade-rest-call(call, func)
end method;

define method maybe-upgrade-rest-call
    (call :: <method-call>, func :: <&lambda>) => (res :: singleton(#t))
  /*
  unless (lambda-rest?(func))
    let number-required = best-function-number-required(func);
    let args      = arguments(call);
    let empty-ref = make-object-reference(#[]);
    remove-user!(args[number-required], call);
    add-user!(empty-ref, call);
    args[number-required] := empty-ref;
  end unless;
  */
  upgrade-to-congruent-call!(call, func);
  #t
end method;

//// KEYWORD

// keyword processing

define function process-keyword-values-into
    (new-arguments :: <simple-object-vector>, call :: <simple-call>,
     f :: <&lambda>, key-arg-values :: <simple-object-vector>, 
     optional-arguments :: <argument-sequence>, bail :: <function>) => ()
  // We compute these properties lazily only if we see a key we can't
  // account for.
  let all-keys-computed? = #f;
  let all-keys? = #f;
  let number-required = best-function-number-required(f);
  for (key-index :: <integer> from size(key-arg-values) - 1 to 0 by -1,
       arg-index :: <integer> from size(optional-arguments) - 1 by -2)
    let keyword = key-arg-values[key-index];
    block (stop)
      for (j :: <integer> from 0 below f.keyword-specifiers.size by 2,
           k :: <integer> from number-required + 1)
        if (keyword == f.keyword-specifiers[j])
          new-arguments[k] := optional-arguments[arg-index];
          stop();
        end if;
      finally
        // An #all-keys in the generic seems to be the most common case
        // (consider make) so we'll cache the results in case there are
        // a lot of keywords.
        if (~all-keys-computed?)
          all-keys? 
            := best-function-all-keys?(f)
                | (instance?(call, <method-call>)
                     // Consider the generic function
                     & best-function-all-keys?(^method-generic-function(f)));
          all-keys-computed? := #t;
        end if;
        if (~(all-keys? | keyword-in-next-methods?(keyword, call)))
          // Prevent further attempts to upgrade the call.
          call.compatibility-state := $compatibility-checked-incompatible;
	  note(<unknown-keyword-in-call>,
	       source-location:  dfm-source-location(call),
	       context-id:       dfm-context-id(call),
	       function:         f,
	       known-keywords:   compute-known-keywords(f),
	       supplied-keyword: keyword);
          bail(#f);
        end if;
      end for;
    end block;
  end for;
end;

define method compute-known-keywords (f :: <&lambda>) => (keys :: <argument-sequence>)
  collecting (as <argument-sequence>)
    for (j :: <integer> from 0 below f.keyword-specifiers.size by 2)
      collect(f.keyword-specifiers[j])
    end for;
  end collecting;
end method;

// Is this keyword recognized by any of the next-method sequence?

define method keyword-in-next-methods? 
    (keyword, call :: <simple-call>) => (well? :: <boolean>)
  #f
end method;

define method keyword-in-next-methods? 
    (keyword, call :: <method-call>) => (well? :: <boolean>)
  let (constant?, next) = constant-value?(call.next-methods);
  if (constant?)
    block (return)
      for (m in next)
        if (best-function-all-keys?(m))
          return(#t);
        else
          let specifiers = keyword-specifiers(m);
          for (keyword-index :: <integer> from 0 below size(specifiers) by 2)
            let m-keyword = specifiers[keyword-index];
            if (m-keyword == keyword)
              return(#t);
            end;
          end;
        end;
      finally
        #f
      end;
    end;
  end;
end method;

define method insert-default-reference!
    (c :: <simple-call>, object) => (object-t :: <value-reference>)
  let (first, last, object-t) 
    = convert-reference(c.environment, $single, object);
  insert-computations-before!(c, first, last);
  object-t
end method;

define function parameters-size (m :: <&lambda>) => (res :: <integer>)
  best-function-number-required(m) 
    + if (best-function-optionals?(m)) 1 else 0 end
    + best-function-number-keys(m)
end function;

define method maybe-upgrade-keyword-call
    (call :: <simple-call>, func :: <&lambda>) => (res :: <boolean>)
  let congruent-call? = call-congruent?(call);
  let number-required = best-function-number-required(func);
  let old-arguments = arguments(call);
  let optional-arguments
    = if (congruent-call?)
        maybe-rest-references
          (call.environment, old-arguments[number-required]);
      else
        copy-sequence(call.arguments, start: number-required);
      end if;
  if (optional-arguments)
    let optional-arguments :: <simple-object-vector>
      = as(<simple-object-vector>, optional-arguments);
    let number-optionals = size(optional-arguments);
    block (bail)
      unless (even?(number-optionals))
	bail(#f)
      end;
      let n-keyword-pairs = floor/((number-optionals), 2);
      let key-arg-values = make(<simple-object-vector>, size: n-keyword-pairs);

      // collect constant keyword values
      for (i :: <integer> from 0 below n-keyword-pairs,
	   j :: <integer> from 0 by 2)
	let (constant?, key-value) = constant-value?(optional-arguments[j]);
	unless (constant?) bail(#f); end;
	key-arg-values[i] := key-value;
      end;

      let n-new-arguments = parameters-size(func);
      let new-arguments = make(<simple-object-vector>,
			       size: n-new-arguments);
      for (i :: <integer> from 0 below number-required)
	new-arguments[i] := old-arguments[i];
      end;

      for (j :: <integer> from 1 by 2,
	   i :: <integer> from number-required + 1 below n-new-arguments)
	let default-value = func.keyword-specifiers[j];
	// HACK: should force func to be optimized first and reschedule this
	//       in the ~optimized case
	// TODO: make things inlineable which come from the current
	//       compilation
	let (inlineable?, inline-default-value) = inlineable?(default-value);
	if (default-value == &unbound 
              & ~lambda-optimized?(func) | ~inlineable?)
	  bail(#f)
	end;
	new-arguments[i] := inline-default-value;
      end for;

      process-keyword-values-into
	(new-arguments, call, func, key-arg-values, optional-arguments, bail);

      for (argument in optional-arguments)
	remove-user!(argument, call);
      end for;
      for (i :: <integer> from number-required + 1 below n-new-arguments)
	let arg-val = new-arguments[i];
	if (instance?(arg-val, <value-reference>))
	  add-user!(arg-val, call);
	else
	  let default-t = insert-default-reference!(call, arg-val);
	  new-arguments[i] := default-t;
	  add-user!(default-t, call);
	end;
      end for;

      let rest?
	= lambda-rest?(func);
      let rest-t
	= if (congruent-call?)
	    if (rest?)
	      old-arguments[number-required]
	    else
	      remove-user!(old-arguments[number-required], call);
	      let rest-t = make-object-reference(#[]);
	      add-user!(rest-t, call);
	      rest-t
	    end if;
	  elseif (~rest? | empty?(optional-arguments))
	    let rest-t = make-object-reference(#[]);
	    add-user!(rest-t, call);
	    rest-t
	  else
	    let (rest-c, rest-t)
	      = generate-stack-vector(call.environment, optional-arguments);
	    add-user!(rest-t, call);
	    insert-computation-before!(call, rest-c);
	    rest-t
	  end if;

      new-arguments[number-required] := rest-t;
      call.arguments := new-arguments;
      upgrade-to-congruent-call!(call, func);
      #t
    end block;
  elseif (size(keyword-specifiers(func)) == 0) // no keyword dispatching needed
    maybe-upgrade-rest-call(call, func)
  end if;
end method;

define method maybe-upgrade-keyword-call
    (call :: <apply>, func :: <&lambda>) => (res :: <boolean>)
  // If there are no explicit keys being trapped, it degrades to a 
  // #rest upgrade.
  if (empty?(func.keyword-specifiers))
    maybe-upgrade-rest-call(call, func);
  else
    #f
  end;
end method;

//// SUPPORT

/*
language: prefix-dylan

(define xep-0 (lambda (f n)
  ((%iep f))))

(define xep-1 (lambda (f n a1)
  ((%iep f) a1)))

(define xep-2 (lambda (f n a1 a2)
  ((%iep f) a1 a2)))

(define xep-3 (lambda (f n a1 a2 a3)
  ((%iep f) a1 a2 a3)))

(define rest-xep-0 (lambda (f n . args)
  ((%iep f) args)))

(define rest-xep-1 (lambda (f n a1 . args)
  ((%iep f) a1 args)))

(define rest-xep-2 (lambda (f n a1 a2 . args)
  ((%iep f) a1 a2 args)))

(define rest-xep-3 (lambda (f n a1 a2 a3 . args)
  ((%iep f) a1 a2 a3 args)))
*/
