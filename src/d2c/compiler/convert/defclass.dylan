module: define-classes
rcs-header: $Header: /scm/cvs/src/d2c/compiler/convert/defclass.dylan,v 1.2.2.2 1998/09/27 22:22:50 tc Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 Matthias Hölzl (tc)
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
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// <definition-parse>   		../parser/parse-tree.dylan
//   <define-class-parse>
//
// <abstract-slot-parse>
//   <slot-parse>
//   <inherited-slot-parse>
//   <init-arg-parse>
//
// <class-features-mixin>
//
// <class-definition>        		../base/defns.dylan
//   <real-class-definition>
//     <local-class-definition>
//
// <slot-defn>
// <override-defn>
//
// <abstract-method-definition>		../front/func-defns.dylan
//   <maker-function-definition>
//   <init-function-definition>
//
// <class-builder>
//   <class-definition-builder>
//   <slot-definition-builder>
//   <cclass-builder>
//
// <top-level-form-description>
//   <class-tlf-descr>
//     <simple-class-tlf-descr>
//     <hairy-class-tlf-descr>
//   <slot-defn-descr>
//   <maker-body-descr>
//

// Parse tree stuff.
// ================

// <define-class-parse>
//
define class <define-class-parse> (<definition-parse>)
  constant slot defclass-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot defclass-superclass-exprs :: <simple-object-vector>,
    required-init-keyword: superclass-exprs:;
  constant slot defclass-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  constant slot defclass-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <define-class-parse>;


// make-define-class
//
// See macro `class-definer' in ../main/bootstrap.dylan.
//
define-procedural-expander
  (#"make-define-class",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   supers-frag :: <fragment>, slots-frag :: <fragment>,
	   options-frag :: <fragment>)
       => ();
     generate-fragment
       (generator,
	make-parsed-fragment
	  (make(<define-class-parse>,
		name: extract-name(name-frag),
		superclass-exprs: map(expression-from-fragment,
				      split-fragment-at-commas(supers-frag)),
		slots: map(extract-slot, split-fragment-at-commas(slots-frag)),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)));
   end method);


// extract-slot -- internal.
//
// Checks whether `frag' is a slot parse and returns the token parse
// tree of `frag'.
//
define function extract-slot (frag :: <fragment>)
    => res :: <abstract-slot-parse>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $error-token
	& instance?(frag.fragment-token, <pre-parsed-token>)
	& instance?(frag.fragment-token.token-parse-tree,
		    <abstract-slot-parse>))
    frag.fragment-token.token-parse-tree;
  else
    error("bug in define class macro: %= isn't a slot parse", frag);
  end if;
end function extract-slot;


// <abstract-slot-parse>
//
define abstract class <abstract-slot-parse> (<object>)
end class <abstract-slot-parse>;


// <slot-parse>
//
define class <slot-parse> (<abstract-slot-parse>)
  constant slot slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <slot-parse>;


// make-slot
//
// See macro `class-definer' in ../main/bootstrap.dylan.
//
define-procedural-expander
  (#"make-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
    => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<slot-parse>,
		name: extract-name(name-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);


// <inherited-slot-parse>
//
define class <inherited-slot-parse> (<abstract-slot-parse>)
  constant slot inherited-slot-parse-name :: <identifier-token>,
    required-init-keyword: name:;
  constant slot inherited-slot-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <inherited-slot-parse>;


// make-inherited-slot
//
// See macro `class-definer' in ../main/bootstrap.dylan.
//
define-procedural-expander
  (#"make-inherited-slot",
   method (generator :: <expansion-generator>, name-frag :: <fragment>,
	   options-frag :: <fragment>)
    => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<inherited-slot-parse>,
		name: extract-name(name-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);


// <init-arg-parse>
//
define class <init-arg-parse> (<abstract-slot-parse>)
  constant slot init-arg-parse-keyword :: <symbol>,
    required-init-keyword: keyword:;
  constant slot init-arg-parse-options :: <simple-object-vector>,
    required-init-keyword: options:;
end class <init-arg-parse>;


// make-init-arg
//
// See macro `class-definer' in ../main/bootstrap.dylan.
//
define-procedural-expander
  (#"make-init-arg",
   method (generator :: <expansion-generator>, keyword-frag :: <fragment>,
	   options-frag :: <fragment>)
    => ();
     generate-fragment
       (generator,
	make-magic-fragment
	  (make(<init-arg-parse>,
		keyword: extract-keyword(keyword-frag),
		options: parse-property-list(make(<fragment-tokenizer>,
						  fragment: options-frag))),
	   source-location: generate-token-source-location(generator)))
   end method);


// extract-keyword -- internal.
//
// Checks whether `frag' is a keyword and returns its literal value.
//
define function extract-keyword (frag :: <fragment>)
    => keyword :: <symbol>;
  if (instance?(frag, <token-fragment>)
	& frag.fragment-token.token-kind == $symbol-token)
    frag.fragment-token.token-literal.literal-value;
  else
    error("Bug in define class macro: %= isn't a keyword.", frag);
  end if;
end function extract-keyword;

// <real-class-definition> 
//
define class <real-class-definition> (<class-definition>)
  //
  // The <cclass> for this class definition, #f if unknown (e.g. non-constant
  // superclasses), #"not-computed-yet" if we haven't computed it yet, or
  // #"computing" if we are actively working on it.
  slot class-defn-cclass
    :: type-union(<cclass>, one-of(#f, #"not-computed-yet", #"computing")),
    init-value: #"not-computed-yet", init-keyword: class:;
  //
  // Defered evaluations function, of #f if there isn't one.
  slot %class-defn-defered-evaluations-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
  //
  // The maker function, of #f if there isn't one.
  slot %class-defn-maker-function
    :: type-union(<ct-function>, one-of(#f, #"not-computed-yet")),
    init-value: #"not-computed-yet";
end;


// defn-type  -- method on exported GF.
//
// Return the compile-time-value for <class>.
//
define method defn-type (defn :: <real-class-definition>) => res :: <cclass>;
  dylan-value(#"<class>");
end;


// <class-features-mixin>  -- internal.
//
// Define slots that describe the features of a class (DRM p.52):
// abstract, primary, and sealed.
//
define class <class-features-mixin> (<object>)
  //
  // Several boolean flags, just what the names say.
  slot is-functional? :: <boolean>,
    required-init-keyword: functional:;
  slot is-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  slot is-abstract? :: <boolean>,
    required-init-keyword: abstract:;
  slot is-primary? :: <boolean>,
    required-init-keyword: primary:;
end class <class-features-mixin>;

// <local-class-definition>
//
define class <local-class-definition> (<real-class-definition>,
				       <class-features-mixin>)
  // 
  // Vector of <expression-parse>s for the superclasses.
  slot class-defn-supers :: <simple-object-vector>,
    required-init-keyword: supers:;
  //
  // Vector of the slots.
  // Each member is of type <slot-defn>.
  slot class-defn-slots :: <simple-object-vector>,
    required-init-keyword: slots:;
  //
  // Vector of slot init value overrides.
  // Each member is of type <override-defn>.
  slot class-defn-overrides :: <simple-object-vector>,
    required-init-keyword: overrides:;
end;  


// <slot-defn>
//
define class <slot-defn> (<object>)
  //
  // The class that introduces this slot.
  slot slot-defn-class :: <real-class-definition>;
  //
  // #t if this slot is sealed, #f if not.  This really means that the getter
  // generic function is sealed on this class and the setter (if any) is sealed
  // on object and this class.
  slot slot-defn-sealed? :: <boolean>,
    required-init-keyword: sealed:;
  //
  // The allocation of this slot.
  slot slot-defn-allocation :: <slot-allocation>,
    required-init-keyword: allocation:;
  //
  // The expression to compute the type.
  slot slot-defn-type :: false-or(<expression-parse>),
    required-init-keyword: type:;
  //
  // The name of the getter generic function.
  slot slot-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The getter method.  Filled in when computed.
  slot slot-defn-getter :: <getter-method-definition>;
  //
  // The name of the setter generic function, or #f if there is no setter.
  slot slot-defn-setter-name :: false-or(<name>),
    required-init-keyword: setter-name:;
  //
  // The setter method.  Filled in when computed.
  slot slot-defn-setter :: false-or(<setter-method-definition>);
  //
  // The init-value expression, or #f if one wasn't supplied.
  slot slot-defn-init-value :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function, or #f if there isn't one.
  slot slot-defn-init-function :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-defn-init-keyword :: false-or(<symbol>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // #t if the init-keyword is required, #f if not.
  slot slot-defn-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
  //
  // The sizer slot defn.
  slot slot-defn-sizer-defn :: false-or(<slot-defn>),
    init-value: #f, init-keyword: sizer-defn:;

  // The slot-info for this slot, or #f if we haven't computed it or don't know
  // enough about the class to compute it at all.
  slot slot-defn-info :: false-or(<slot-info>),
    init-value: #f;
end;


// <override-defn>
//
define class <override-defn> (<object>)
  //
  // The class that introduces this override.
  slot override-defn-class :: <real-class-definition>;
  //
  // The name of the getter.
  slot override-defn-getter-name :: <name>,
    required-init-keyword: getter-name:;
  //
  // The init-value expression, or #f if none.
  slot override-defn-init-value :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function expression, or #f if none.
  slot override-defn-init-function :: false-or(<expression-parse>),
    init-value: #f, init-keyword: init-function:;
  //
  // The <override-info> for this override, or #f if we haven't computed it
  // or don't know enough about the class to compute it at all.
  slot override-defn-info :: false-or(<override-info>),
    init-value: #f;
end;


// <maker-function-definition>
//
define class <maker-function-definition> (<abstract-method-definition>)
  slot maker-func-defn-class-defn :: <class-definition>,
    init-keyword: class-defn:;
end class <maker-function-definition>;

define sealed domain make (singleton(<maker-function-definition>));
define sealed domain initialize (<maker-function-definition>);


// <init-function-definition>
//
define class <init-function-definition> (<abstract-method-definition>)
  slot init-func-defn-method-parse :: false-or(<method-parse>),
    init-value: #f, init-keyword: method-parse:;
end class <init-function-definition>;


// Parse builders.
// ==============
//
// When processing top level forms we need to gather a lot of
// information about the form on which we are currently working.
// We encapsulate this information in "builder" classes.


// <class-builder> -- internal.
//
// During the processing of top-level forms we pass around an instance
// of a subclasses of <class-builder> that holds all information
// about the tlf in question.  Note that there is not generic
// `make-definition-builder' since every type of <class-builder> needs
// different additional info.
//
define abstract class <class-builder> (<object>)
end class;


// <class-definition-builder> -- internal.
//
// A helper class that contains all the infomation that
// `process-top-level-form' needs about a class parse.
//
define class <class-definition-builder> (<class-builder>,
					 <class-features-mixin>)
  slot name :: <symbol>,
    required-init-keyword: name:;
  slot slots :: <stretchy-vector>,
    required-init-keyword: slots:;
  slot overrides :: <stretchy-vector>,
    required-init-keyword: overrides:;
  slot class-supers :: <simple-object-vector>,
    required-init-keyword: class-supers:;
end class <class-definition-builder>;


// make-class-definition-builder -- internal.
//
// Create a <class-definition-builder> from a <define-class-parse>.
//
define function make-class-definition-builder
    (class-parse :: <define-class-parse>)
 => (builder :: <class-definition-builder>);
  let (class-functional?-fragment :: false-or(<fragment>),
       class-sealed?-fragment :: false-or(<fragment>),
       class-primary?-fragment :: false-or(<fragment>),
       class-abstract?-fragment :: false-or(<fragment>))
    = extract-properties(class-parse.defclass-options,
			 #"functional", #"sealed",
			 #"primary", #"abstract");
  let name :: <symbol>
    = class-parse.defclass-name.token-symbol;
  make(<class-definition-builder>,
       name: name,
       class-parse: class-parse,
       class-supers: class-parse.defclass-superclass-exprs,
       slots: make(<stretchy-vector>),
       overrides: make(<stretchy-vector>),
       functional:
	 class-functional?-fragment
	 & extract-boolean(class-functional?-fragment),
       sealed:
	 ~class-sealed?-fragment
	 | extract-boolean(class-sealed?-fragment),
       primary:
	 class-primary?-fragment
	 & extract-boolean(class-primary?-fragment),
       abstract:
	 class-abstract?-fragment
	 & extract-boolean(class-abstract?-fragment));
end function make-class-definition-builder;


// <slot-definition-builder>
//
// Info about a slot parse.
//
define class <slot-definition-builder> (<class-builder>)
  slot getter :: <symbol>,
    required-init-keyword: getter:;
  slot is-sealed? :: <boolean>,
    required-init-keyword: is-sealed?:;
  slot allocation :: <symbol>,
    required-init-keyword: allocation:;
  slot type :: false-or(<expression-parse>),
    required-init-keyword: type:;
  slot setter :: false-or(<symbol>),
    required-init-keyword: setter:;
  slot init-keyword :: false-or(<symbol>),
    required-init-keyword: init-keyword:;
  slot req-init-keyword :: false-or(<symbol>),
    required-init-keyword: req-init-keyword:;
  slot init-value :: false-or(<expression-parse>),
    required-init-keyword: init-value:;
  slot init-expr :: false-or(<expression-parse>),
    required-init-keyword: init-expr:;
  slot init-function :: false-or(<expression-parse>),
    required-init-keyword: init-function:;
  slot sizer :: false-or(<symbol>),
    required-init-keyword: sizer:;
  slot size-init-keyword :: false-or(<symbol>),
    required-init-keyword: size-init-keyword:;
  slot req-size-init-keyword :: false-or(<symbol>),
    required-init-keyword: req-size-init-keyword:;
  slot size-init-value :: false-or(<expression-parse>),
    required-init-keyword: size-init-value:;
  slot size-init-function :: false-or(<expression-parse>),
    required-init-keyword: size-init-function:;
  slot sizer-fragment :: false-or(<fragment>),
    required-init-keyword: sizer-fragment:;
  slot init-keyword-fragment :: false-or(<fragment>),
    required-init-keyword: init-keyword-fragment:;
  slot size-init-keyword-fragment :: false-or(<fragment>),
    required-init-keyword: size-init-keyword-fragment:;
  slot req-size-init-keyword-fragment :: false-or(<fragment>),
    required-init-keyword: req-size-init-keyword-fragment:;
end class <slot-definition-builder>;


// make-slot-definition-builder
//
define function make-slot-definition-builder
    (class :: <class-definition-builder>,
     slot :: <abstract-slot-parse>)
 => (builder :: <slot-definition-builder>);
  let (sealed?-fragment, allocation-fragment,
       type-fragment, setter-fragment,
       init-keyword-fragment, req-init-keyword-fragment,
       init-value-fragment, init-expr-fragment,
       init-function-fragment, sizer-fragment,
       size-init-keyword-fragment, req-size-init-keyword-fragment,
       size-init-value-fragment, size-init-function-fragment)
    = extract-properties(slot.slot-parse-options,
			 sealed:, allocation:, type:, setter:,
			 init-keyword:, required-init-keyword:,
			 init-value:, init-expr:, init-function:,
			 sizer:, size-init-keyword:,
			 required-size-init-keyword:,
			 size-init-value:, size-init-function:);
  let allocation :: <symbol>
    = if (allocation-fragment)
	extract-identifier(allocation-fragment).token-symbol;
      else
	#"instance";
      end;
  let getter :: <symbol> = slot.slot-parse-name.token-symbol;

  make(<slot-definition-builder>,
       getter: getter,
       is-sealed?:
	 sealed?-fragment
	 & extract-boolean(sealed?-fragment),
       allocation: allocation,
       type:
	 type-fragment
	 & expression-from-fragment(type-fragment),
       setter:
	 if (class.is-functional? & allocation == #"instance")
	   let id
	     = setter-fragment
	     & extract-identifier-or-false(setter-fragment);
	   if (id)
	     compiler-warning-location
	       (id,
		"Instance allocation slots in functional classes can't"
		  " have a setter.");
	   end;
	   #f;
	 elseif (setter-fragment)
	   let id = extract-identifier-or-false(setter-fragment);
	   id & id.token-symbol;
	 else
	   symcat(getter, "-setter");
	 end,
       init-keyword:
	 (init-keyword-fragment
	    & extract-keyword(init-keyword-fragment)),
       req-init-keyword:
	 (req-init-keyword-fragment
	    & extract-keyword(req-init-keyword-fragment)),
       init-value:
	 (init-value-fragment
	    & expression-from-fragment(init-value-fragment)),
       init-expr:
	 (init-expr-fragment
	    & expression-from-fragment(init-expr-fragment)),
       init-function:
	 (init-function-fragment
	    & expression-from-fragment(init-function-fragment)),
       sizer:
	 (sizer-fragment
	    & extract-identifier(sizer-fragment).token-symbol),
       size-init-keyword:
	 (size-init-keyword-fragment
	    & extract-keyword(size-init-keyword-fragment)),
       req-size-init-keyword:
	 (req-size-init-keyword-fragment
	    & extract-keyword(req-size-init-keyword-fragment)),
       size-init-value:
	 (size-init-value-fragment
	    & expression-from-fragment(size-init-value-fragment)),
       size-init-function:
	 (size-init-function-fragment
	    & expression-from-fragment(size-init-function-fragment)),
       sizer-fragment: sizer-fragment,
       init-keyword-fragment: init-keyword-fragment,
       size-init-keyword-fragment: size-init-keyword-fragment,
       req-size-init-keyword-fragment: req-size-init-keyword-fragment);
end function make-slot-definition-builder;


// extract-identifier-or-false -- internal.
//
define function extract-identifier-or-false (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $false-token =>
      #f;
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end function extract-identifier-or-false;


// extract-identifier -- internal.
//
define function extract-identifier (fragment :: <token-fragment>)
    => res :: false-or(<identifier-token>);
  let token = fragment.fragment-token;
  select (token.token-kind)
    $raw-ordinary-word-token, $ordinary-define-body-word-token,
    $ordinary-define-list-word-token, $quoted-name-token =>
      token;
    otherwise =>
      compiler-fatal-error
	("invalid identifier: %s", token);
  end select;
end function extract-identifier;


// <cclass-builder>  -- internal.
//
define class <cclass-builder> (<class-builder>)
  slot definition,
    required-init-keyword: definition:;
  slot super-exprs,
    required-init-keyword: super-exprs:;
  slot nsupers,
    required-init-keyword: nsupers:;
  slot supers,
    required-init-keyword: supers:;
  slot closest-super,
    required-init-keyword: closest-super:;
  slot closest-primary,
    required-init-keyword: closest-primary:;
  slot bogus?,
    required-init-keyword: bogus?:;
end class <cclass-builder>;

define function make-cclass-builder
    (defn :: <local-class-definition>)
 => builder :: <cclass-builder>;
  let super-exprs = defn.class-defn-supers;
  let nsupers = super-exprs.size;
  let supers = make(<simple-object-vector>, size: nsupers);
  let closest-super = #f;
  let closest-primary = #f;
  let bogus? = #f;
  make(<cclass-builder>,
       definition: defn, super-exprs: super-exprs,
       nsupers: nsupers, supers: supers,
       closest-super: closest-super,
       closest-primary: closest-primary,
       bogus?: bogus?);
end function make-cclass-builder;

// Top level form processing.
// =========================

// During top level form processing, we parse the `define class' form
// and build the necessary <local-class-definition>, <slot-defn>, and
// <override-defn> objects.  We only check for syntactic errors and
// local semantic errors.  By local semantic errors, I mean errors
// that can be detected by looking at nothing more than this class
// itself.
//
// We also note the class definition and any implicit definitions for
// slot accessors.

// process-top-level-form {<define-class-parse>}
//    -- method on imported GF.
//
define method process-top-level-form (form :: <define-class-parse>) => ();
  let class :: <class-definition-builder>
    = make-class-definition-builder(form);
  
  add-%object-class-override(form, class);
  process-slots(form, class);

  let defn :: <local-class-definition>
    = make-local-class-definition(form, class);
  implicitly-define-generics(defn);
  assign-override-classes(defn);

  note-variable-definition(defn);
  add!(*Top-Level-Forms*, make(<define-class-tlf>, defn: defn));
end method process-top-level-form;


// add-%object-class-override -- internal.
//
// If `class' is not abstract and has superclasses we add an override
// for the `%object-class' slot.
//
define function add-%object-class-override
    (class-parse :: <define-class-parse>,
     class :: <class-definition-builder>)
 => ();
  unless (class.is-abstract? | empty?(class-parse.defclass-superclass-exprs))
    add!(class.overrides,
	 make(<override-defn>,
	      getter-name: make(<basic-name>,
				symbol: #"%object-class",
				module: $Dylan-Module),
	      init-value: make(<varref-parse>,
			       id: class-parse.defclass-name)));
  end;
end function add-%object-class-override;


// process-slots -- internal.
//
define function process-slots
    (class-parse :: <define-class-parse>,
     class :: <class-definition-builder>)
 => ();
  for (option in class-parse.defclass-slots)
    block ()
      process-slot(class, option);
    exception (<fatal-error-recovery-restart>)
      #f;
    end block;
  end for;
end function process-slots;


// make-local-class-definition -- internal.
//
define function make-local-class-definition
    (class-parse :: <define-class-parse>,
     class :: <class-definition-builder>)
 => (definition :: <local-class-definition>);
  
  let slots = as(<simple-object-vector>, class.slots);
  let overrides = as(<simple-object-vector>, class.overrides);
  make(<local-class-definition>,
       name: make(<basic-name>,
		  symbol: class.name,
		  module: *Current-Module*),
       library: *Current-Library*,
       supers: class.class-supers,
       slots: slots,
       overrides: overrides,
       functional: class.is-functional?,
       sealed: class.is-sealed?,
       primary: class.is-primary?,
       abstract: class.is-abstract?);
end function make-local-class-definition;


// implicitly-define-generics -- internal.
//
// Create the implicit definitions for the accessor generic functions.
//
define function  implicitly-define-generics
    (defn :: <local-class-definition>)
 => ();
  let slots :: <simple-object-vector>
    = defn.class-defn-slots;
  for (slot in slots)
    slot.slot-defn-class := defn;
    //
    // Implicity define the accessor generics.
    if (slot.slot-defn-sizer-defn)
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 2, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 3, #f, #f);
      end;
    else
      implicitly-define-generic
	(*Current-Library*, slot.slot-defn-getter-name, 1, #f, #f);
      if (slot.slot-defn-setter-name)
	implicitly-define-generic
	  (*Current-Library*, slot.slot-defn-setter-name, 2, #f, #f);
      end;
    end;
  end;
end function implicitly-define-generics;


// assign-override-classes -- internal
//
define function assign-override-classes
    (defn :: <local-class-definition>);
  let overrides :: <simple-object-vector>
    = defn.class-defn-overrides;
  for (override in overrides)
    override.override-defn-class := defn;
  end for;
end function assign-override-classes;

// Processing slots.
// ================

// process-slot -- internal GF.
//
define generic process-slot
    (parse :: <class-definition-builder>, slot :: <abstract-slot-parse>)
 => ();


// process-slot {<slot-parse>} -- internal.
//
define method process-slot
    (class :: <class-definition-builder>,
     slot :: <slot-parse>)
    => ();
  let slot-builder :: <slot-definition-builder>
    = make-slot-definition-builder(class, slot);
  check-slot-inits(slot-builder);

  let getter-name
    = make(<basic-name>, symbol: slot-builder.getter,
	   module: *Current-Module*);
  let setter-name
    = slot-builder.setter
      & make(<basic-name>, symbol: slot-builder.setter,
	     module: *Current-Module*);
  let size-defn :: false-or(<slot-defn>)
    = make-sizer-slot-definition(class, slot-builder);
  
  let slot = make(<slot-defn>,
		  sealed: slot-builder.is-sealed? & #t,
		  allocation: slot-builder.allocation,
		  type: slot-builder.type,
		  getter-name: getter-name,
		  setter-name: setter-name,
		  init-value: slot-builder.init-value,
		  init-function: slot-builder.init-function,
		  init-keyword:
		    slot-builder.init-keyword
		    | slot-builder.req-init-keyword,
		  sizer-defn: size-defn,
		  init-keyword-required:
		    slot-builder.req-init-keyword & #t);
  add!(class.slots, slot);
end method process-slot;


// check-slot-inits -- internal.
//
// Check whether the init value, keyword and function arguments for
// `slot-builder' are allowed.  
//
define function check-slot-inits
    (slot-builder :: <slot-definition-builder>)
 => ();
  if (slot-builder.init-value)
    if (slot-builder.init-expr)
      compiler-fatal-error-location
	(slot-builder.init-value,
	 "Can't supply both an init-value: and an init-expression.");
    end if;
    if (slot-builder.init-function)
      compiler-fatal-error-location
	(slot-builder.init-value,
	 "Can't supply both an init-value: and an init-function:.");
    end;
    if (slot-builder.req-init-keyword)
      compiler-fatal-error-location
	(slot-builder.init-value,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
  elseif (slot-builder.init-expr)
    if (slot-builder.init-function)
      compiler-fatal-error-location
	(slot-builder.init-expr,
	 "Can't supply both an init-function: and an init-expression.");
    end if;
    if (slot-builder.req-init-keyword)
      compiler-fatal-error-location
	(slot-builder.init-expr,
	 "Can't supply both an init-value: and a required-init-keyword:.");
    end;
    if (instance?(slot-builder.init-expr, <literal-ref-parse>))
      slot-builder.init-value := slot-builder.init-expr;
    else
      slot-builder.init-function
	:= make(<method-ref-parse>,
		method: make(<method-parse>,
			     parameters: make(<parameter-list>, fixed: #[]),
			     body: slot-builder.init-expr));
    end if;
  elseif (slot-builder.init-function)
    if (slot-builder.req-init-keyword)
      compiler-fatal-error-location
	(slot-builder.init-function,
	 "Can't supply both an init-function: and a "
	   "required-init-keyword:.");
    end;
  end;
  if (slot-builder.init-keyword & slot-builder.req-init-keyword)
    compiler-fatal-error-location
      (simplify-source-location
	 (slot-builder.init-keyword-fragment.source-location),
       "Can't supply both an init-keyword: and a required-init-keyword:.");
  end;
end function check-slot-inits;


// make-sizer-slot-definition -- internal.
//
// If `class' has a variable sized slot we check its arguments and add
// a slot definition to `class', otherwise we check whether any
// arguments for the sizer slot are provided and report an error if
// this is the case.
//
define function make-sizer-slot-definition
    (class :: <class-definition-builder>,
     slot-builder :: <slot-definition-builder>)
 => (sizer-definition :: false-or(<slot-defn>));
  if (slot-builder.sizer)
    let sizer-name
      = make(<basic-name>,
	     symbol: slot-builder.sizer,
	     module: *Current-Module*);
    
    check-for-sizer-slot-error(slot-builder);
    let slot :: <slot-defn>
      = make(<slot-defn>,
	     sealed: slot-builder.is-sealed?,
	     allocation: #"instance",
	     type:
	       make(<varref-parse>,
		    id: make(<identifier-token>,
			     kind: $raw-ordinary-word-token,
			     symbol: #"<integer>",
			     module: $Dylan-Module,
			     uniquifier: make(<uniquifier>))),
	     getter-name: sizer-name,
	     setter-name: #f,
	     init-value: slot-builder.size-init-value,
	     init-function: slot-builder.size-init-function,
	     init-keyword:
	       slot-builder.size-init-keyword
	       | slot-builder.req-size-init-keyword,
	     init-keyword-required:
	       slot-builder.req-size-init-keyword & #t);
    add!(class.slots, slot);
    slot;
  else
    check-for-no-sizer-slot-error(slot-builder);
    #f;
  end;
end function make-sizer-slot-definition;


// check-for-sizer-slot-error -- internal.
//
// Do the error checking for `make-sizer-slot-definition' in the case
// that the class has a sizer slot.
//
define function check-for-sizer-slot-error
    (slot-builder :: <slot-definition-builder>)
 => ();
  unless (slot-builder.allocation == #"instance")
    compiler-fatal-error-location
      (simplify-source-location
	 (slot-builder.sizer-fragment.source-location),
       "Only instance allocation slots can be variable length, but "
	 "%s has %s allocation",
       slot-builder.getter, slot-builder.allocation);
  end unless;
  
  if (slot-builder.size-init-value)
    if (slot-builder.size-init-function)
      compiler-fatal-error-location
	(slot-builder.size-init-value,
	 "Can't have both a size-init-value: and size-init-function:");
    end if;
  elseif (~(slot-builder.size-init-function
	      | slot-builder.req-size-init-keyword))
    compiler-fatal-error
      ("The Initial size for vector slot %s must be supplied somehow.",
       slot-builder.getter);
  end if;
  
  if (slot-builder.size-init-keyword
	& slot-builder.req-size-init-keyword)
    compiler-fatal-error-location
      (simplify-source-location
	 (slot-builder.size-init-keyword-fragment.source-location),
       "Can't have both a size-init-keyword: and a "
	 "required-size-init-keyword:");
  end if;
end function check-for-sizer-slot-error;


// check-for-no-sizer-slot-error -- internal.
//
// Do the error checking for `make-sizer-slot-definition' in the case
// that the class does not have a sizer slot.
//
define function check-for-no-sizer-slot-error
    (slot-builder :: <slot-definition-builder>)
 => ();
  if (slot-builder.size-init-value)
    compiler-fatal-error-location
      (slot-builder.size-init-value,
       "Can't supply a size-init-value: without a sizer: generic "
	 "function");
  end;
  if (slot-builder.size-init-function)
    compiler-fatal-error-location
      (slot-builder.size-init-function,
       "Can't supply a size-init-function: without a "
	 "sizer: generic function");
  end;
  if (slot-builder.size-init-keyword)
    compiler-fatal-error-location
      (simplify-source-location
	 (slot-builder.size-init-keyword-fragment.source-location),
       "Can't supply a size-init-keyword: without a "
	 "sizer: generic function");
  end;
  if (slot-builder.req-size-init-keyword)
    compiler-fatal-error-location
      (simplify-source-location
	 (slot-builder.req-size-init-keyword-fragment.source-location),
       "Can't supply a required-size-init-keyword: "
	 "without a sizer: generic function");
  end;
end function check-for-no-sizer-slot-error;


// process-slot {<inherited-slot-parse>} -- internal.
//
define method process-slot
    (class :: <class-definition-builder>,
     slot :: <inherited-slot-parse>)
    => ();
  let (init-value-frag, init-expr-frag, init-function-frag)
    = extract-properties(slot.inherited-slot-parse-options,
			 init-value:, init-expr:, init-function:);

  let init-value
    = init-value-frag & expression-from-fragment(init-value-frag);
  let init-expr
    = init-expr-frag & expression-from-fragment(init-expr-frag);
  let init-function
    = init-function-frag & expression-from-fragment(init-function-frag);

  if (init-value)
    if (init-expr)
      compiler-fatal-error-location
	(init-expr,
	 "Can't supply both an init-value: and an init-expression.");
    end if;
    if (init-function)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-value: and an init-function:.");
    end;
  elseif (init-expr)
    if (init-function)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply both an init-function: and an init-expression.");
    end if;
    if (instance?(init-expr, <literal-ref-parse>))
      init-value := init-expr;
    else
      init-function
	:= make(<method-ref-parse>,
		method: make(<method-parse>,
			     parameters: make(<parameter-list>, fixed: #[]),
			     body: init-expr));
    end if;
  end;

  add!(class.overrides,
       make(<override-defn>,
	    getter-name:
	      make(<basic-name>,
		   symbol: slot.inherited-slot-parse-name.token-symbol,
		   module: *Current-Module*),
	    init-value: init-value,
	    init-function: init-function));
end method process-slot;


// process-slot {<init-arg-parse>} -- internal.
//
define method process-slot
    (class :: <class-definition-builder>,
     slot :: <init-arg-parse>)
 => ();
  let (required?-frag, type-frag, init-value-frag, init-function-frag)
    = extract-properties(slot.init-arg-parse-options,
			 required:, type:, init-value:, init-function:);

  let required? = required?-frag & extract-boolean(required?-frag);
  let type = type-frag & expression-from-fragment(type-frag);
  let init-value = init-value-frag & expression-from-fragment(init-value-frag);
  let init-function
    = init-function-frag & expression-from-fragment(init-function-frag);

  if (required?)
    if (init-value)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply an init-value: for required keyword init arg specs");
    end;
    if (init-function)
      compiler-fatal-error-location
	(init-function,
	 "Can't supply an init-function: for required keyword init arg specs");
    end;
  elseif (init-value)
    if (init-function)
      compiler-fatal-error-location
	(init-value,
	 "Can't supply both an init-value: and an "
	   "init-function: for keyword init arg specs");
    end;
  end;
  // ### Need to do something with it.
end method process-slot;

// CT-Values.
// =========

// ct-value {<real-class-definition>}
//    -- method on exported GF.
//
// Return the compile-time value for a class definition, computing it
// if necessary.  This is the <cclass> object of a class definition.
// If we can't compute that for some reason, return #f to indicate
// that this class doesn't have a compile-time value.
//
define method ct-value (defn :: <real-class-definition>)
    => res :: false-or(<cclass>);
  select (defn.class-defn-cclass)
    #"not-computed-yet" =>
      defn.class-defn-cclass := compute-cclass(defn);
    #"computing" =>
      compiler-error-location
	(defn,
	 "class %s circularly defined.",
	 defn.defn-name.name-symbol);
      #f;
    otherwise =>
      defn.class-defn-cclass;
  end;
end;


// compute-cclass  -- internal.
//
// Compute the <cclass> for `defn'.
//
define function compute-cclass (defn :: <local-class-definition>)
    => res :: false-or(<cclass>);
  //
  // Mark that we are trying to compute this class.
  defn.class-defn-cclass := #"computing";
  //
  // Evaluate the superclasses, and check them for validity.
  let cclass-builder :: <cclass-builder>
    = make-cclass-builder(defn);
  local
    //
    // superclass-not-cclass-error
    //
    method superclass-not-cclass-error
	(super :: false-or(<ct-value>),
	 super-expr :: <constituent-parse>,
	 index :: <integer>)
     => ();
      if (super)
	compiler-error-location
	  (super-expr.source-location,
	   "%s superclass of %s is not a class: %s.",
	   integer-to-english(index + 1, as: #"ordinal"),
	   defn.defn-name, super);
      else
	compiler-warning-location
	  (super-expr.source-location,
	   "%s superclass of %s is not obviously a constant.",
	   integer-to-english(index + 1, as: #"ordinal"),
	   defn.defn-name);
      end if;
      cclass-builder.bogus? := #t;
    end method superclass-not-cclass-error;


  for (index from 0 below cclass-builder.nsupers)
    let super-expr :: <constituent-parse>
      = cclass-builder.super-exprs[index];
    let super :: false-or(<ct-value>)
      = ct-eval(super-expr, #f);
    if (instance?(super, <cclass>))
      check-class-for-violations(cclass-builder, super, super-expr, index);
    else
      //
      // The superclass isn't a <class>.  So complain.
      superclass-not-cclass-error(super, super-expr, index);
    end if;
  end for;

  if (defn == dylan-defn(#"<object>"))
    unless (cclass-builder.nsupers.zero?)
      error("<object> has superclasses?");
    end unless;
  else
    if (cclass-builder.nsupers.zero?)
      compiler-error-location
	(defn, "%s has no superclasses.", defn.defn-name);
      cclass-builder.bogus? := #t;
    elseif (cclass-builder.closest-primary == #f & ~cclass-builder.bogus?)
      error("<object> isn't being inherited or isn't primary?");
    end if;
  end if;

  unless (cclass-builder.bogus?)
    //
    // Compute the slots and overrides.
    let slot-infos = map(compute-slot, defn.class-defn-slots);
    let override-infos = map(compute-override-info, defn.class-defn-overrides);
    //
    // Make and return the <cclass>.
    make-defined-cclass(defn, cclass-builder.supers, slot-infos, override-infos);
  end unless;
end function compute-cclass;


// make-defined-cclass -- internal.
//
// Return a <defined-cclass> for `defn'.
//
define function make-defined-cclass
    (defn :: <real-class-definition>,
     supers :: <simple-object-vector>,
     slot-infos :: <simple-object-vector>,
     override-infos :: <simple-object-vector>)
 => (cclass :: <defined-cclass>);
  make(<defined-cclass>,
       loading: #f,
       name: defn.defn-name,
       defn: defn,
       direct-superclasses: as(<list>, supers),
       not-functional:
	 // Do we proclude functional subclasses?
	 if (defn.is-functional?)
	   #f;
	 elseif (defn.is-abstract?)
	   ~supers.empty?
	     & (any?(not-functional?, supers)
		  | any?(inhibits-functional-classes?, slot-infos));
	 else
	   #t;
	 end,
       functional: defn.is-functional?,
       sealed: defn.is-sealed?,
       primary: defn.is-primary?,
       abstract: defn.is-abstract?,
       slots: slot-infos,
       overrides: override-infos);
end function make-defined-cclass;


// check-class-for-violations  -- internal.
//
define function check-class-for-violations
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>,
     index :: <integer>)
 => ();
  //
  // Store the superclass.
  cclass-builder.supers[index] := super;
  
  error-if-sealing-violation(cclass-builder, super, super-expr);
  warning-if-illegal-abstract-superclass(cclass-builder, super,
					 super-expr);
  //
  // Check that everything is okay with the functional adjective.
  if (cclass-builder.definition.is-functional?)
    error-if-non-functional-superclass(cclass-builder, super, super-expr);
  else
    error-if-functional-superclass(cclass-builder, super, super-expr);
  end if;
  error-if-unrelated-primaries(cclass-builder, super, super-expr);
end function check-class-for-violations;


// error-if-sealing-violation
//
define function error-if-sealing-violation
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>)
 => ();
  //
  // Make sure we arn't trying to inherit from a sealed class.
  if (super.sealed? & super.loaded?)
    compiler-error-location
      (super-expr.source-location,
       "%s can't inherit from %s because %s is sealed.",
       cclass-builder.definition.defn-name, super, super);
    cclass-builder.bogus? := #t;
  end if;
end function error-if-sealing-violation;


// warning-if-illegal-abstract-superclass  -- internal.
//
define function warning-if-illegal-abstract-superclass
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>)
 => ();
  //
  // Check that everything is okay with the abstract adjective.
  if (cclass-builder.definition.is-abstract? & ~super.abstract?)
    compiler-warning-location
      (super-expr.source-location,
       "abstract class %s can't inherit from %s because "
	 "%s is concrete -- ignoring abstract abjective.",
       cclass-builder.definition.defn-name, super, super);
    cclass-builder.definition.is-abstract? := #f;
  end if;
end function warning-if-illegal-abstract-superclass;


// error-if-non-functional-superclass  -- internal.
//
define function error-if-non-functional-superclass
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>)
 => ();
  //
  // Make sure we arn't trying to inherit from anything we can't.
  if (super.not-functional?)
    compiler-error-location
      (super-expr.source-location,
       "functional class %s can't inherit from %s "
	 "because %s %s and is not functional.",
       cclass-builder.definition.defn-name, super, super,
       if (super.abstract?)
	 "has instance slots";
       else
	 "is concrete";
       end if);
    cclass-builder.bogus? := #t;
  end if;
end function error-if-non-functional-superclass;


// error-if-functional-superclass
//
define function error-if-functional-superclass
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>)
 => ();
  //
  // It isn't a functional class, so make sure we arn't trying to
  // inherit from a functional class.
  if (super.functional?)
    compiler-error-location
      (super-expr.source-location,
       "class %s can't inherit from %s because %s is functional.",
       cclass-builder.definition.defn-name, super, super);
    cclass-builder.bogus? := #t;
  end if;
end function error-if-functional-superclass;


// error-if-unrelated-primaries
//
define function error-if-unrelated-primaries
    (cclass-builder :: <cclass-builder>,
     super :: false-or(<ct-value>),
     super-expr :: <constituent-parse>)
 => ();
  //
  // Check to see if this superclass's closest-primary-superclass
  // is closer than any of the others so far.
  let other-primary = super.closest-primary-superclass;
  if (~cclass-builder.closest-primary
	| csubtype?(other-primary, cclass-builder.closest-primary))
    cclass-builder.closest-super := super;
    cclass-builder.closest-primary := other-primary;
  elseif (~csubtype?(cclass-builder.closest-primary, other-primary))
    local method describe (primary, super)
	    if (primary == super)
	      as(<string>, primary.cclass-name.name-symbol);
	    else
	      format-to-string("%s (inherited via %s)",
			       primary.cclass-name.name-symbol,
			       super.cclass-name.name-symbol);
	    end;
	  end;
    compiler-error-location
      (super-expr.source-location,
       "%s can't inherit from %s and %s because they are both primary "
	 "and neither is a subclass of the other.",
       cclass-builder.definition.defn-name,
       describe(cclass-builder.closest-primary, cclass-builder.closest-super),
       describe(other-primary, super));
    cclass-builder.bogus? := #t;
  end if;
end function error-if-unrelated-primaries;


// compute-slot  -- internal.
//
define function compute-slot
    (slot :: <slot-defn>)
 => info :: <slot-info>;
  //
  // Note: we don't pass in anything for the type, init-value, or
  // init-function, because we need to compile-time-eval those, which we
  // can't do until tlf-finalization time.
  let info
    = if (slot.slot-defn-sizer-defn)
	make-slot-info-for-sizer-slot(slot);
      else
	make-slot-info-for-standard-slot(slot);
      end;
  slot.slot-defn-info := info;
  info;
end function compute-slot;


// make-slot-info-for-sizer-slot -- internal.
//
define function make-slot-info-for-sizer-slot
    (slot :: <slot-defn>)
 => (info :: <slot-info>);
  let getter-name = slot.slot-defn-getter-name;
  make(<vector-slot-info>,
       getter: find-variable(getter-name, create: #t),
       read-only: slot.slot-defn-setter-name == #f,
       init-value: slot.slot-defn-init-value & #t,
       init-function: slot.slot-defn-init-function & #t,
       init-keyword: slot.slot-defn-init-keyword,
       init-keyword-required:
	 slot.slot-defn-init-keyword-required?,
       size-slot: slot.slot-defn-sizer-defn.slot-defn-info);
end function make-slot-info-for-sizer-slot;


// make-slot-info-for-standard-slot -- internal.
//
define function make-slot-info-for-standard-slot
    (slot :: <slot-defn>)
 => (info :: <slot-info>);
  let getter-name = slot.slot-defn-getter-name;
  make(<slot-info>,
       allocation: slot.slot-defn-allocation,
       getter: find-variable(getter-name, create: #t),
       read-only: slot.slot-defn-setter-name == #f,
       init-value: slot.slot-defn-init-value & #t,
       init-function: slot.slot-defn-init-function & #t,
       init-keyword: slot.slot-defn-init-keyword,
       init-keyword-required:
	 slot.slot-defn-init-keyword-required?);
end function make-slot-info-for-standard-slot;
  

// compute-override-info  -- internal.
//
// Compute the override info for `override' and store it in the
// `override-defn-info' slot.
//
define function compute-override-info
    (override :: <override-defn>) => info :: <override-info>;
  let getter-name = override.override-defn-getter-name;
  //
  // Note: we don't pass in anything for the init-value or init-function,
  // because we need to compile-time-eval those, which we can't do until
  // tlf-finalization time.
  let info = make(<override-info>,
		  getter: find-variable(getter-name, create: #t),
		  init-value: override.override-defn-init-value & #t,
		  init-function: override.override-defn-init-function & #t);
  override.override-defn-info := info;
  info;
end function compute-override-info;


// inhibits-functionsl-classes? -- internal GF.
//
define generic inhibits-functional-classes?
    (slot :: <slot-info>) => res :: <boolean>;


// inhibits-functional-classes? {<slot-info>}
//    -- method on internal GF.
//
define method inhibits-functional-classes?
    (slot :: <slot-info>) => res :: <boolean>;
  #f;
end method inhibits-functional-classes?;


// inhibits-functional-classes? {<instance-slot-info>}
//    -- method on internal GF.
//
define method inhibits-functional-classes?
    (slot :: <instance-slot-info>) => res :: <boolean>;
  #t;
end method inhibits-functional-classes?;

// Top level form finalization.
// ===========================

// finalize-top-level-form {<define-class-tlf>}
//    -- method on imported GF.
//
define method finalize-top-level-form (tlf :: <define-class-tlf>) => ();
  let defn :: <definition> = tlf.tlf-defn;
  //
  // Compute the cclass if it hasn't been computed yet.
  let cclass :: false-or(<cclass>)
    = compute-tlf-cclass-if-necessary(defn);
  let class-type = cclass | make(<unknown-ctype>);

  // Finalize the slots.
  for (slot in defn.class-defn-slots)
    finalize-slot(slot, cclass, class-type, tlf);
  end for;

  // Finalize the overrides.
  for (override in defn.class-defn-overrides)
    finalize-tlf-override(override, tlf);
  end for;
end method finalize-top-level-form;


// compute--tlf-cclass-if-necessary -- internal.
//
// Compute `defn''s cclass if it is not yet computed, otherwise return
// whatever was previously computed.
//
define function compute-tlf-cclass-if-necessary
    (defn :: <definition>)
 => cclass :: false-or(<cclass>);
  if (defn.class-defn-cclass == #"not-computed-yet")
    defn.class-defn-cclass := compute-cclass(defn);
  else
    defn.class-defn-cclass;
  end;
end function compute-tlf-cclass-if-necessary;


// finalize-tlf-override  -- internal.
//
// Finalize `override'.
//
define function finalize-tlf-override
    (override :: <override-defn>,
     tlf :: <define-class-tlf>)
 => ();
  // Fill in the <override-info> with the init value.
  let info = override.override-defn-info;
  if (info)
    if (override.override-defn-init-function)
      let (ctv, change-to-init-value?)
	= maybe-define-init-function(override.override-defn-init-function, 
				     override.override-defn-getter-name,
				     tlf);
      if (ctv)
	if (change-to-init-value?)
	  info.override-init-function := #f;
	  info.override-init-value := ctv;
	else
	  info.override-init-function := ctv;
	end if;
      end if;
    elseif (override.override-defn-init-value)
      let init-val = ct-eval(override.override-defn-init-value, #f);
      if (init-val)
	info.override-init-value := init-val;
      end if;
    end if;
  end if;
end function finalize-tlf-override;


// finalize-slot
//
// Finalize `slot'.
//
define function finalize-slot
    (slot :: <slot-defn>, cclass :: false-or(<cclass>),
     class-type :: <ctype>, tlf :: <define-class-tlf>)
    => ();
  //
  // Compute the type of the slot.
  let slot-type :: <ctype> = compute-slot-type(slot);

  let specializers :: <list>
    = if (slot.slot-defn-sizer-defn)
	list(class-type, specifier-type(#"<integer>"));
      else
	list(class-type);
      end;

  // Fill in the <slot-info> with the type, init value, and init-function
  fill-in-slot-info(slot, slot-type, tlf);
  
  // Define the accessor methods.
  define-slot-accessors(slot, slot-type, cclass, tlf, specializers);
end function finalize-slot;


// compute-slot-type  -- internal.
//
define function compute-slot-type
    (slot :: <slot-defn>)
 => slot-type :: <ctype>;
  if (slot.slot-defn-type)
    let type = ct-eval(slot.slot-defn-type, #f);
    if (instance?(type, <ctype>))
      type;
    else
      make(<unknown-ctype>);
    end;
  else
    object-ctype();
  end;
end function compute-slot-type;


// fill-in-slot-info  -- internal.
//
define function fill-in-slot-info
    (slot :: <slot-defn>, slot-type :: <ctype>,
     tlf :: <define-class-tlf>)
 => ();
  let info = slot.slot-defn-info;
  if (info)
    info.slot-type := slot-type;
    if (slot.slot-defn-init-function)
      let (ctv, change-to-init-value?)
	= maybe-define-init-function(slot.slot-defn-init-function,
				     slot.slot-defn-getter-name,
				     tlf);
      if (ctv)
	if (change-to-init-value?)
	  info.slot-init-function := #f;
	  info.slot-init-value := ctv;
	else
	  info.slot-init-function := ctv;
	end if;
      end if;
    elseif (slot.slot-defn-init-value)
      let init-val = ct-eval(slot.slot-defn-init-value, #f);
      if (init-val)
	info.slot-init-value := init-val;
      end if;
    end if;
  end if;
end function fill-in-slot-info;


// define-slot-accessors  -- internal.
//
define function define-slot-accessors
    (slot :: <slot-defn>, slot-type :: <ctype>,
     cclass :: false-or(<cclass>), tlf :: <define-class-tlf>,
     specializers :: <list>)
 => ();    
  unless (slot.slot-defn-allocation == #"virtual")
    //
    // Extract the library from the class definition.
    let library :: <library> = tlf.tlf-defn.defn-library;
    //
    // Are the accessor methods hairy?
    let hairy? = ~cclass | instance?(slot-type, <unknown-ctype>);
    
    set-slot-defn-getter(slot, slot-type, specializers, library, hairy?);
    add-slot-defn-getter-to-gf(slot);
    add-seal-if-necessary(slot, tlf, specializers, library);
    set-slot-defn-setter(slot, slot-type, tlf, specializers,
			 library, hairy?);
  end unless;
end function define-slot-accessors;


// set-slot-defn-getter  -- internal.
//
define function set-slot-defn-getter
    (slot :: <slot-defn>, slot-type :: <ctype>,
     specializers :: <list>,
     library :: <library>, hairy? :: <boolean>)
 => ();
  slot.slot-defn-getter
    := make(<getter-method-definition>,
	    base-name: slot.slot-defn-getter-name,
	    library: library,
	    signature: make(<signature>,
			    specializers: specializers,
			    returns: slot-type),
	    hairy: hairy?,
	    slot: slot.slot-defn-info);
end function set-slot-defn-getter;


// add-slot-defn-getter-to-gf
//
define function add-slot-defn-getter-to-gf
    (slot :: <slot-defn>) => ();            
  let gf :: false-or(<generic-definition>)
    = slot.slot-defn-getter.method-defn-of;
  if (gf)
    ct-add-method(gf, slot.slot-defn-getter);
  end;
end function add-slot-defn-getter-to-gf;


// add-seal-if-necessary
//
define function add-seal-if-necessary
    (slot :: <slot-defn>, tlf :: <define-class-tlf>,
     specializers :: <list>, library :: <library>)
 => ();
  let gf :: false-or(<generic-definition>)
    = slot.slot-defn-getter.method-defn-of;
  if (slot.slot-defn-sealed?)
    if (gf)
      add-seal(gf, library, specializers, tlf);
    else
      compiler-error
	("%s doesn't name a generic function, so can't be sealed.",
	 slot.slot-defn-getter-name);
    end if;
  end if;
end function add-seal-if-necessary;


// set-slot-defn-setter
//
define function set-slot-defn-setter
    (slot :: <slot-defn>, slot-type :: <ctype>,
     tlf :: <define-class-tlf>, specializers :: <list>,
     library :: <library>, hairy? :: <boolean>)
 => ();
  slot.slot-defn-setter
    := if (slot.slot-defn-setter-name)
	 let defn = make(<setter-method-definition>,
			 base-name: slot.slot-defn-setter-name,
			 library: library,
			 signature: make(<signature>,
					 specializers:
					   pair(slot-type, specializers),
					 returns: slot-type),
			 hairy: hairy?,
			 slot: slot.slot-defn-info);
	 let gf = defn.method-defn-of;
	 if (gf)
	   ct-add-method(gf, defn);
	 end;
	 if (slot.slot-defn-sealed?)
	   if (gf)
	     add-seal(gf, library,
		      pair(object-ctype(), specializers), tlf);
	   else
	     compiler-error
	       ("%s doesn't name a generic function, so can't be sealed.",
		slot.slot-defn-setter-name);
	   end;
	 end;
	 defn;
       else
	 #f;
       end if;      
end function set-slot-defn-setter;


// maybe-define-init-function  -- internal.
//
define function maybe-define-init-function
    (expr :: <expression-parse>, getter-name :: <basic-name>,
     tlf :: <define-class-tlf>)
    => (ctv :: false-or(<ct-value>), change-to-init-value? :: <boolean>);
  let init-val :: false-or(<ct-value>) = ct-eval(expr, #f);
  if (init-val)
    return-init-val-or-error(init-val, expr);
  else
    let method-ref = expand-until-method-ref(expr);
    if (method-ref)
      maybe-define-init-function-from-method-ref
	(expr, getter-name, tlf, method-ref);
    else
      values(#f, #f);
    end if;
  end if;
end function maybe-define-init-function;


// return-init-val-or-error  -- internal.
//
// Return `init-val' and #f is `init-val' is a compile-time function,
// otherwise report an error and return (#f, #f).
//
define function return-init-val-or-error
    (init-fun :: <ct-value>, expr :: <expression-parse>)
 => (init-function :: false-or(<ct-value>),
     change-to-init-value? :: <false>);
  if (cinstance?(init-fun, function-ctype()))
    values(init-fun, #f);
  else
    compiler-error-location
      (expr, "Invalid init-function: %s.", init-fun);
    values(#f, #f);
  end if;
end function return-init-val-or-error;


// maybe-define-init-function-from-method-ref  -- internal.
//
define function maybe-define-init-function-from-method-ref
    (expr :: <expression-parse>, getter-name :: <basic-name>,
     tlf :: <define-class-tlf>, method-ref :: <method-ref-parse>)
 => (ctv :: false-or(<ct-value>), change-to-init-value? :: <boolean>);
  let method-parse = method-ref.method-ref-method;
  let (signature, anything-non-constant?)
    = compute-signature(method-parse.method-parameters,
			method-parse.method-returns);
  if (anything-non-constant?)
    values(#f, #f);
  else
    let result-type = first(signature.returns.positional-types,
			    default: signature.returns.rest-value-type);
    let ctv = ct-eval(method-parse.method-body, #f);
    if (ctv & cinstance?(ctv, result-type))
      // Change it to an init-value.
      values(ctv, #t);
    else
      // Make a constant init-function definition.
      define-constant-init-function
	(getter-name, tlf, method-parse);
    end if;
  end if;
end function maybe-define-init-function-from-method-ref;


// define-constant-init-function  -- internal.
//
define function define-constant-init-function
    (getter-name :: <basic-name>, tlf :: <define-class-tlf>,
     method-parse :: <method-parse>)
 => (ctv :: false-or(<ct-value>), change-to-init-value? :: <false>);
      let result-param
	= (first(method-parse.method-returns.varlist-fixed,
		 default: method-parse.method-returns.varlist-rest)
	     | make(<parameter>,
		    name: make(<identifier-token>,
			       kind: $raw-ordinary-word-token,
			       symbol: #"result")));
      let new-method-parse
	= make(<method-parse>,
	       parameters: make(<parameter-list>),
	       returns: make(<variable-list>, fixed: vector(result-param)),
	       body: make(<funcall-parse>,
			  function: make(<method-ref-parse>,
					 method: method-parse),
			  arguments: #[]));
      let (new-signature, anything-non-constant?)
	= compute-signature(new-method-parse.method-parameters,
			    new-method-parse.method-returns);
      if (anything-non-constant?)
	error("%= shouldn't be able to have anything non-constant in it",
	      new-signature);
      end if;
      let name
	= make(<derived-name>, how: #"init-function", 
	       base: getter-name);
      let init-func-defn
	= make(<init-function-definition>, name: name,
	       library: tlf.tlf-defn.defn-library,
	       signature: new-signature,
	       method-parse: new-method-parse);
      add!(tlf.tlf-init-function-defns, init-func-defn);
      values(init-func-defn.ct-value, #f);
end function   define-constant-init-function;

// class-defn-mumble-function accessors.
// ====================================

// class-defn-defered-evaluations-function {<real-class-definition>}
//    -- method on exported GF.
//
define method class-defn-defered-evaluations-function
    (defn :: <real-class-definition>) => res :: false-or(<ct-function>);
  if (defn.%class-defn-defered-evaluations-function == #"not-computed-yet")
    defn.%class-defn-defered-evaluations-function
      := if (compute-new-evaluations-function?(defn))
	   make(<ct-function>,
		name: make(<derived-name>, how: #"deferred-evaluation",
			   base: defn.defn-name),
		signature: make(<signature>, specializers: #(),
				returns: make-values-ctype(#(), #f)));
	 else
	   #f;
	 end;
  else
    defn.%class-defn-defered-evaluations-function;
  end;
end;


// compute-new-evaluations-function?  -- internal.
//
define function compute-new-evaluations-function?
    (defn :: <real-class-definition>)
 => (create-new-function? :: <boolean>);
  block (return)
    let cclass = ct-value(defn);
    unless (cclass)
      return(#f);
    end;
    // If any of our superclasses have a defered evaluations
    // function, we need one.
    for (super in cclass.direct-superclasses)
      if (super.class-defn.class-defn-defered-evaluations-function)
	return(#t);
      end;
    end;
    // If any of our slots require some defered evaluations,
    // then we need a defered evaluations function.
    for (slot-defn in defn.class-defn-slots)
      let info = slot-defn.slot-defn-info;
      if (instance?(info.slot-type, <unknown-ctype>)
	    | info.slot-init-value == #t
	    | info.slot-init-function == #t)
	return(#t);
      end;
    end;
    // Same for the overrides.
    for (override-defn in defn.class-defn-overrides)
      let info = override-defn.override-defn-info;
      if (info.override-init-value == #t
	    | info.override-init-function == #t)
	return(#t);
      end;
    end;
    // ### inherited each-subclass slots w/ non obvious init
    // values impose the existance of the defered-evaluations
    // function.
  end block;
end function compute-new-evaluations-function?;


// class-defn-maker-function {<real-class-definition>}
//    -- method on exported GF.
//
// Lazily compute the `%class-defn-maker-function' slot of `defn'.
//
define method class-defn-maker-function
    (defn :: <real-class-definition>)
 => res :: false-or(<ct-function>);
  if (defn.%class-defn-maker-function == #"not-computed-yet")
    defn.%class-defn-maker-function
      := compute-class-defn-maker-function(defn);
  else
    defn.%class-defn-maker-function;
  end if;
end method class-defn-maker-function;


// compute-class-defn-maker-function  -- internal.
//
define function compute-class-defn-maker-function
    (defn :: <real-class-definition>)
 => class-defn-maker-function :: false-or(<ct-function>);
  block (return)
    let cclass :: <cclass> = ct-value(defn);
    //
    // If the class is hairy or abstract, no maker.
    if (cclass == #f | cclass.abstract?)
      return(#f);
    end;

    let key-infos = make(<stretchy-vector>);
    for (slot in cclass.all-slot-infos)
      if (instance?(slot, <instance-slot-info>))
	prepare-slot-for-maker-or-return(slot, cclass,
					 key-infos, return)
      end if;
    end for;
    //
    // Okay, we can make a ctv for the maker function.  First,
    // compute some values we will need.
    let name = make(<derived-name>, how: #"maker", base: defn.defn-name);
    let sig = make(<signature>, specializers: #(),
		   keys: as(<list>, key-infos), all-keys: #t,
		   returns: make(<direct-instance-ctype>,
				 base-class: cclass));
    //
    // If this is the maker for an immediate representation class,
    // set up the maker so that it can be inlined.
    let instance-rep = pick-representation(cclass, #"speed");
    let maker-defn
      = if (instance?(instance-rep, <immediate-representation>))
	  make(<maker-function-definition>,
	       name: name,
	       source-location: defn.source-location,
	       library: defn.defn-library,
	       signature: sig,
	       inline-function: maker-inline-expansion,
	       class-defn: defn);
	end if;
    //
    // And make the ctv.
    make(<ct-function>, name: name, signature: sig,
	 definition: maker-defn);
  end block;
end function compute-class-defn-maker-function;


// prepare-slot-for-maker-or-return  -- internal.
//
define function prepare-slot-for-maker-or-return
    (slot :: <slot-info>, cclass :: <cclass>,
     key-infos :: <stretchy-vector>, return :: <function>)
 => ();
  if (instance?(slot.slot-type, <unknown-ctype>))
    //
    // Unknown slot type: no maker.
    return(#f);
  end;
  //
  // Find the active override.
  let override :: false-or(<override-info>)
    = find-active-override(slot, cclass);
  //
  // If there is an init-function, and it isn't a constant, give
  // up.
  // !!! We don't actually use this value!!!
  // Is this correct???
  let init-function :: type-union(<ct-value>, <boolean>)
    = find-init-function-or-return(slot, override, return);
  //
  // If there is an init-value, and it isn't a constant, give up.
  let init-value :: type-union(<ct-value>, <boolean>)
    = find-init-value-or-return(slot, override, return);
  //
  // If the slot is keyword initializable, make a key-info for it.
  make-keyword-info-for-slot(slot, key-infos,
			     override, init-value);
end function prepare-slot-for-maker-or-return;


// find-active-override -- internal.
//
define function find-active-override
    (slot :: <slot-info>, cclass :: <ct-value>)
 => override :: false-or(<override-info>);
  block (found)
    for (override in slot.slot-overrides)
      if (csubtype?(cclass, override.override-introduced-by))
	found(override);
      end;
    finally
      #f;
    end;
  end;
end function find-active-override;


// find-init-function-or-return  -- internal.
//
define function find-init-function-or-return
    (slot :: <slot-info>, override :: false-or(<override-info>),
     return :: <function>)
 => init-function :: type-union(<ct-value>, <boolean>);
  let init-function
    = if (override)
	override.override-init-function;
      else
	slot.slot-init-function;
      end if;
  if (init-function == #t)
    return(#f);
  end if;
  init-function;
end function find-init-function-or-return;


// find-init-value-or-return  -- internal.
//
define function find-init-value-or-return
    (slot :: <slot-info>, override :: false-or(<override-info>),
     return :: <function>)
 => init-value :: type-union(<ct-value>, <boolean>);
  let init-value
    = if (override)
	override.override-init-value;
      else
	slot.slot-init-value;
      end;
  if (init-value == #t)
    return(#f);
  end;
  init-value;
end function find-init-value-or-return;


// make-keyword-info-for-slot  -- internal.
//
define function make-keyword-info-for-slot
    (slot :: <slot-info>, key-infos :: <stretchy-vector>,
     override :: false-or(<override-info>),
     init-value :: type-union(<ct-value>, <boolean>))
 => ();
  let key = slot.slot-init-keyword;
  if (key)
    let type = slot.slot-type;
    let required? = ~override & slot.slot-init-keyword-required?;
    let default-bogus?
      = init-value & ~cinstance?(init-value, type);
    let key-info
      = make(<key-info>, key-name: key, type: type,
	     required: required? | default-bogus?,
	     default: init-value);
    add!(key-infos, key-info);
  end if;
end function make-keyword-info-for-slot;


// maker-inline-expansion
//
define method maker-inline-expansion
    (maker-defn :: <maker-function-definition>)
    => res :: <function-literal>;
  let class-defn = maker-defn.maker-func-defn-class-defn;
  let component = make(<fer-component>);
  let builder = make-builder(component);
  let region = build-maker-function-body(builder, class-defn);
  let leaf = make-function-literal(builder, #f, #"function", #"local",
				   maker-defn.function-defn-signature, region);
  optimize-component(component, simplify-only: #t);
  leaf;
end method maker-inline-expansion;

// Top level form builders.
// =======================
//
// When converting top-level forms we face the same problem as when
// parsing top-level forms: We have large amounts of data to be
// remembered.  Again we introduce some classes to hold that
// information.

// <top-level-form-description>  -- internal.
//
define abstract class <top-level-form-description> (<object>)
end;

// <class-tlf-descr> -- internal.
//
define abstract class <class-tlf-descr> (<top-level-form-description>)
  slot tlf :: <define-class-tlf>,
    required-init-keyword: tlf:;
  slot definition :: <definition>,
    required-init-keyword: definition:;
end class <class-tlf-descr>;


// <simple-class-tlf-descr>  -- internal.
//
define class <simple-class-tlf-descr> (<class-tlf-descr>)
  // Can we restrict the type to <cclass>???
  slot cclass :: <ct-value>,
    required-init-keyword: cclass:;
  slot lexenv :: <lexenv>,
    required-init-keyword: lexenv:;
  slot policy :: <policy>,
    required-init-keyword: policy:;
  slot source :: <source-location>,
    required-init-keyword: source:;
  slot tl-builder :: <fer-builder>,
    required-init-keyword: tl-builder:;
  slot evals-builder :: <flow-builder>,
    required-init-keyword: evals-builder:;
end class <simple-class-tlf-descr>;


// hairy-class-tlf-descr>  -- internal
//
// Actually we don't need this class at the moment, since we abort on
// hairy class tlfs anyways, but it might make their intruduction more
// painless if we already have things set up.  Actually the `cclass'
// slot should be virtual and a method on it shoould be defined that
// always returns false, but d2c doesn't handle virtual slots yet.
//
define class <hairy-class-tlf-descr> (<class-tlf-descr>)
  slot cclass :: <false>,
    required-init-keyword: cclass:;
end class <hairy-class-tlf-descr>;


// make-class-tlf-descr -- internal.
//
// Create either a <simple-class-tlf-descr> or a
// <hairy-class-tlf-descr>.
//
define function make-class-tlf-descr
    (tl-builder :: <fer-builder>, tlf :: <define-class-tlf>)
 => class-tlf-descr :: <class-tlf-descr>;
  let defn = tlf.tlf-defn;
  let cclass = ct-value(defn);
  if (cclass == #f)
    make(<hairy-class-tlf-descr>,
	 tlf: tlf, definition: defn, cclass: cclass);
  else
    let lexenv = make(<lexenv>, method-name: defn.defn-name);
    let policy = lexenv.lexenv-policy;
    let source = defn.source-location;
    let evals-builder = make-builder(tl-builder);
    
    make(<simple-class-tlf-descr>,
	 tlf: tlf, definition: defn, cclass: cclass,
	 lexenv: lexenv, policy: policy, source: source,
	 tl-builder: tl-builder, evals-builder: evals-builder);
  end if;
end function make-class-tlf-descr;


// <slot-defn-descr>  -- internal.
//
define class <slot-defn-descr> (<top-level-form-description>)
  slot slot-definition :: <slot-defn>,
    required-init-keyword: slot-definition:;
  slot slot-info :: <slot-info>,
    required-init-keyword: slot-info:;
  slot getter :: false-or(<variable>),
    required-init-keyword: getter:;
  slot getter-name :: <symbol>,
    required-init-keyword: getter-name:;
  //  slot type :: <ctype>, unused???
  //    required-init-keyword: type:;
  slot type-var :: false-or(<initial-variable>),
    required-init-keyword: type-var:;
  slot allocation :: <slot-allocation>,
    required-init-keyword: allocation:;
  slot init-value :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-value:;
  slot init-function :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-function:;
end class <slot-defn-descr>;


// make-slot-defn-descr  -- internal.
//
define function make-slot-defn-descr
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-defn :: <slot-defn>)
 => slot-defn-descr :: <slot-defn-descr>;
  let slot-info :: <slot-info> = slot-defn.slot-defn-info;
  let getter :: false-or(<variable>) = slot-info.slot-getter;
  let getter-name :: <symbol> = getter.variable-name;
  
  let slot-type :: <ctype> = slot-info.slot-type;
  let type-var :: false-or(<initial-variable>)
    = calculate-type-var (tlf-descr, slot-defn, slot-info,
			  slot-type, getter-name);
  
  let allocation :: <slot-allocation> = slot-defn.slot-defn-allocation;
  
  let init-value :: type-union(<ct-value>, <boolean>)
    = slot-info.slot-init-value;
  let init-function :: type-union(<ct-value>, <boolean>)
    = slot-info.slot-init-function;
  make(<slot-defn-descr>,
       slot-definition: slot-defn, slot-info: slot-info,
       getter: getter, getter-name: getter-name,
       type: type, type-var: type-var,
       allocation: allocation, init-value: init-value,
       init-function: init-function);
end function make-slot-defn-descr;


// calculate-type-var  -- internal.
//
define function calculate-type-var
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-defn :: <slot-defn>, slot-info :: <slot-info>,
     slot-type :: <ctype>, slot-name)
 => type-var :: false-or(<initial-variable>);
  if (instance?(slot-type, <unknown-ctype>))
    let type-expr = slot-defn.slot-defn-type;
    let var :: <initial-variable>
      = make-local-var(tlf-descr.evals-builder,
		       symcat(slot-name, "-type"),
		       specifier-type(#"<type>"));
    fer-convert(tlf-descr.evals-builder, type-expr, tlf-descr.lexenv,
		#"assignment", var);
    build-assignment
      (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
       make-unknown-call
	 (tlf-descr.evals-builder,
	  ref-dylan-defn(tlf-descr.evals-builder, tlf-descr.policy,
			 tlf-descr.source, #"slot-type-setter"),
	  #f,
	  list(var,
	       make-literal-constant(tlf-descr.evals-builder, slot-info))));
    var;
  else
    #f;
  end;
end function calculate-type-var;


// calculate-type-and-type-var  -- internal.
//
/*
define function calculate-type-and-type-var
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-defn :: <slot-defn>, slot-info :: <slot-info>,
     slot-type :: <ctype>, slot-name)
 => (type :: <ctype>, type-var :: false-or(<initial-variable>));
  if (instance?(slot-type, <unknown-ctype>))
    let type-expr = slot-defn.slot-defn-type;
    let var :: <initial-variable>
      = make-local-var(tlf-descr.evals-builder,
		       symcat(slot-name, "-type"),
		       specifier-type(#"<type>"));
    fer-convert(tlf-descr.evals-builder, type-expr, tlf-descr.lexenv,
		#"assignment", var);
    build-assignment
      (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
       make-unknown-call
	 (tlf-descr.evals-builder,
	  ref-dylan-defn(tlf-descr.evals-builder, tlf-descr.policy,
			 tlf-descr.source, #"slot-type-setter"),
	  #f,
	  list(var,
	       make-literal-constant(tlf-descr.evals-builder, slot-info))));
    values(object-ctype(), var);
  else
    values(slot-type, #f);
  end;
end function calculate-type-and-type-var;
unused  */


// <slot-defn-descr>  -- internal.
//
define class <override-defn-descr> (<top-level-form-description>)
  slot override-defn :: <override-defn>,
    required-init-keyword: override-defn:;
  slot override-info :: false-or(<override-info>),
    required-init-keyword: override-info:;
  slot getter :: <variable>,
    required-init-keyword: getter:;
  slot slot-name :: <symbol>,
    required-init-keyword: slot-name:;
  slot init-value :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-value:;
  slot init-function :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-function:;
end class <override-defn-descr>;


// make-override-descr  -- internal.
//
define function make-override-descr
    (tlf-descr :: <simple-class-tlf-descr>,
     override-defn :: <override-defn>)
 => override-descr :: <override-defn-descr>;
  let override-info = override-defn.override-defn-info;
  let getter = override-info.override-getter;
  let slot-name = getter.variable-name;
  let init-value = override-info.override-init-value;
  let init-function = override-info.override-init-function;
  make(<override-defn-descr>,
       override-defn: override-defn, override-info: override-info,
       getter: getter, slot-name: slot-name,
       init-value: init-value, init-function: init-function);
end function make-override-descr;



// Top level form conversion.
// =========================

// convert-top-level-form {<fer-builder>, <define-class-tlf>}
//    -- method on exported GF.
//
define method convert-top-level-form
    (tl-builder :: <fer-builder>, tlf :: <define-class-tlf>) => ();
  let tlf-descr :: <class-tlf-descr>
    = make-class-tlf-descr(tl-builder, tlf);
  convert-top-level-form-using-descr(tlf-descr);
end method convert-top-level-form;


// convert-top-level-form-using-descr  -- internal GF
//
define generic convert-top-level-form-using-descr
    (tlf-descr :: <class-tlf-descr>)
 => ();

// convert-top-level-form-using-descr {<hairy-class-tlf-descr>}
//    -- method on internal GF.
//
// Raise error; not implemented yet.
//
define method convert-top-level-form-using-descr
    (tlf-descr :: <hairy-class-tlf-descr>)
 => ();
    // The class is sufficiently hairy that we can't do anything.
    // Build top-level init code to create the class at runtime.
    error("### Can't deal with hairy classes yet.");
end method convert-top-level-form-using-descr;


// convert-top-level-form-using-descr {<simple-class-tlf-descr>}
//    -- method on internal GF.
//
define method convert-top-level-form-using-descr
    (tlf-descr :: <simple-class-tlf-descr>)
 => ();
  // The construction of the class object and the initialization of
  // the class variable will be handled by the linker.  We just need
  // to build the defered-evaluations, key-defaulter, and maker
  // functions.
  fer-convert-init-function-defns(tlf-descr);
  
  // Do the defered evaluations for any of the superclasses that need it.
  for (super in tlf-descr.cclass.direct-superclasses)
    do-deferred-evaluation-for(tlf-descr, super);
  end for;
  
  // Process slot definitions.
  for (slot-defn in tlf-descr.definition.class-defn-slots,
       index from 0)
    let slot-descr :: <slot-defn-descr> =
      make-slot-defn-descr(tlf-descr, slot-defn);
    if (slot-descr.init-value == #t)
      build-init-value-assignment(tlf-descr, slot-descr);
    elseif (slot-descr.init-function == #t)
      build-init-function-assignment(tlf-descr, slot-descr);
    end;
    build-getter-and-setter(tlf-descr, slot-descr);
  end for;
  
  // Process overrides.
  for (override-defn in tlf-descr.definition.class-defn-overrides,
       index from 0)
    let override-descr :: <override-defn-descr>
      = make-override-descr(tlf-descr, override-defn);
    
    if (override-descr.init-value == #t
	  | override-descr.init-function == #t)
      if (override-descr.init-value)
	build-override-value-assignment(tlf-descr, override-descr);
      else
	build-override-function-assignment(tlf-descr, override-descr);
      end if;
    end if;
  end for;
  
  build-tlf-maker(tlf-descr);
  
  let ctv = tlf-descr.definition.class-defn-defered-evaluations-function;
  if (ctv)
    build-deferred-evaluations-function(tlf-descr, ctv);
  else
    assert(instance?(builder-result(tlf-descr.evals-builder), <empty-region>));
  end if;
end method convert-top-level-form-using-descr;


// do-deferred-evaluation-for-super  -- internal.
//
define function do-deferred-evaluation-for
    (tlf-descr :: <simple-class-tlf-descr>, super :: <cclass>)
 => ();
  if (super.class-defn.class-defn-defered-evaluations-function)
    build-assignment
      (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
       make-unknown-call
	 (tlf-descr.evals-builder,
	  ref-dylan-defn(tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source,
			 #"maybe-do-defered-evaluations"),
	  #f,
	  list(make-literal-constant(tlf-descr.evals-builder, super))));
  end;
end function do-deferred-evaluation-for;


// fer-convert-init-function-defns  -- internal
//
// fer-convert all `init-function-defn's of `tlf'.
//
define function fer-convert-init-function-defns
    (tlf-descr :: <simple-class-tlf-descr>)
 => ();
  for (init-func-defn in tlf-descr.tlf.tlf-init-function-defns)
    let meth = init-func-defn.init-func-defn-method-parse;
    let name = init-func-defn.defn-name;
    fer-convert-method(tlf-descr.tl-builder, meth, name,
		       init-func-defn.ct-value,
		       #"global", tlf-descr.lexenv, tlf-descr.lexenv);
  end for;
end function fer-convert-init-function-defns;


// build-init-value-assignment
//
define function build-init-value-assignment
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-descr :: <slot-defn-descr>)
 => ();
  let var = make-local-var(tlf-descr.evals-builder,
			   symcat(slot-descr.getter-name, "-init-value"),
			   object-ctype());
  fer-convert(tlf-descr.evals-builder,
	      slot-descr.slot-definition.slot-defn-init-value,
	      tlf-descr.lexenv, #"assignment", var);
  build-assignment
    (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
     make-unknown-call
       (tlf-descr.evals-builder,
	ref-dylan-defn(tlf-descr.evals-builder,
		       tlf-descr.policy, tlf-descr.source,
		       #"slot-init-value-setter"),
	#f,
	list(var, make-literal-constant(tlf-descr.evals-builder,
					slot-descr.slot-info))));
end function build-init-value-assignment;


// build-init-function-assignment
//
define function build-init-function-assignment
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-descr :: <slot-defn-descr>)
 => ();
  let leaf
    = convert-init-function
    (tlf-descr.evals-builder,
     slot-descr.slot-info.slot-getter,
     slot-descr.slot-definition.slot-defn-init-function);
  build-assignment
    (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
     make-unknown-call
       (tlf-descr.evals-builder,
	ref-dylan-defn(tlf-descr.evals-builder,
		       tlf-descr.policy, tlf-descr.source,
		       #"slot-init-function-setter"),
	#f,
	list(leaf, make-literal-constant(tlf-descr.evals-builder,
					 slot-descr.slot-info))));
end function build-init-function-assignment;


// build-call  -- internal.
//
define function build-call
    (tlf-descr :: <simple-class-tlf-descr>,
     name, #rest args)
 => local-variable :: <initial-variable>;
  let temp = make-local-var(tlf-descr.evals-builder,
			    name, object-ctype());
  build-assignment
    (tlf-descr.evals-builder, tlf-descr.policy,
     tlf-descr.source, temp,
     make-unknown-call
       (tlf-descr.evals-builder,
	ref-dylan-defn(tlf-descr.evals-builder,
		       tlf-descr.policy, tlf-descr.source, name),
	#f, as(<list>, args)));
  temp;
end function build-call;


// build-add-method  -- internal
//
define function build-add-method
    (tlf-descr :: <simple-class-tlf-descr>,
     gf-name :: <name>,
     method-defn :: <accessor-method-definition>,
     method-leaf :: <initial-variable>)
  => ();
  // We don't use method-defn-of, because that is #f if there
  // is a definition but it isn't a define generic.
  let gf-var = find-variable(gf-name);
  let gf-defn = gf-var & gf-var.variable-definition;
  if (gf-defn)
    let gf-leaf = build-defn-ref(tlf-descr.evals-builder,
				 tlf-descr.policy,
				 tlf-descr.source, gf-defn);
    build-assignment
      (tlf-descr.evals-builder, tlf-descr.policy,
       tlf-descr.source, #(),
       make-unknown-call
	 (tlf-descr.evals-builder,
	  ref-dylan-defn(tlf-descr.evals-builder,
			 tlf-descr.policy, tlf-descr.source,
			 #"add-method"),
	  #f,
	  list(gf-leaf, method-leaf)));
    build-defn-set(tlf-descr.evals-builder,
		   tlf-descr.policy, tlf-descr.source,
		   method-defn, method-leaf);
  else
    compiler-fatal-error-location
      (tlf-descr.tlf,
       "No definition for %s, and can't implicitly define it.",
       gf-name.name-symbol);
  end if;
end function build-add-method;


// build-getter-and-setter -- internal.
//
define function build-getter-and-setter
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-descr :: <slot-defn-descr>)
 => ();          
  unless (slot-descr.allocation == #"virtual")
    if (slot-descr.type-var)
      build-typed-getter-and-setter(tlf-descr, slot-descr);
    else
      build-untyped-getter-and-setter(tlf-descr, slot-descr);
    end if;
  end unless;
end function build-getter-and-setter;


// build-typed-getter-and-setter -- internal.
//
define function build-typed-getter-and-setter
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-descr :: <slot-defn-descr>)
 => ();          
      let results = build-call(tlf-descr, #"list", slot-descr.type-var);
      let cclass-leaf = make-literal-constant(tlf-descr.evals-builder,
					      tlf-descr.cclass);
      let false-leaf
	= make-literal-constant(tlf-descr.evals-builder,
				as(<ct-value>, #f));
      begin
	let getter
	  = build-getter(tlf-descr.evals-builder,
			 #f, slot-descr.slot-definition,
			 slot-descr.slot-info);
	let getter-specializers
	  = build-call(tlf-descr, #"list", cclass-leaf);
	let meth
	  = build-call(tlf-descr, #"%make-method", getter-specializers,
		       results, false-leaf, getter);
	build-add-method(tlf-descr,
			 slot-descr.slot-definition.slot-defn-getter-name,
			 slot-descr.slot-definition.slot-defn-getter,
			 meth);
      end;
      if (slot-descr.slot-definition.slot-defn-setter)
	let setter
	  = build-setter(tlf-descr.evals-builder,
			 #f, slot-descr.slot-definition,
			 slot-descr.slot-info);
	let setter-specializers
	  = build-call(tlf-descr, #"list", slot-descr.type-var,
		       cclass-leaf);
	let meth
	  = build-call(tlf-descr, #"%make-method", setter-specializers,
		       results, false-leaf, setter);
	build-add-method(tlf-descr,
			 slot-descr.slot-definition.slot-defn-setter-name,
			 slot-descr.slot-definition.slot-defn-setter,
			 meth);
      end;
end function build-typed-getter-and-setter;


// build-untyped-getter-and-setter -- internal.
//
define function build-untyped-getter-and-setter
    (tlf-descr :: <simple-class-tlf-descr>,
     slot-descr :: <slot-defn-descr>)
 => ();          
  let getter
    = slot-descr.slot-definition.slot-defn-getter.ct-value;
  let getter-standin
    = slot-accessor-standin(slot-descr.slot-info,
			    #"getter");
  if (getter-standin)
    getter.ct-accessor-standin := getter-standin;
  else
    build-getter(tlf-descr.tl-builder, getter,
		 slot-descr.slot-definition, slot-descr.slot-info);
  end if;
  
  if (slot-descr.slot-definition.slot-defn-setter)
    let setter = slot-descr.slot-definition.slot-defn-setter.ct-value;
    let setter-standin
      = slot-accessor-standin(slot-descr.slot-info,
			      #"setter");
    if (setter-standin)
      setter.ct-accessor-standin := setter-standin;
    else
      build-setter(tlf-descr.tl-builder, setter,
		   slot-descr.slot-definition, slot-descr.slot-info);
    end if;
  end if;
end function build-untyped-getter-and-setter;


// build-override-value-assignment  -- internal.
//
define function build-override-value-assignment
    (tlf-descr :: <simple-class-tlf-descr>,
     override-descr :: <override-defn-descr>)
 => ();
  let descriptor-leaf
    = make-literal-constant(tlf-descr.evals-builder,
			    override-descr.override-info);
  
  let var = make-local-var(tlf-descr.evals-builder,
			   symcat(override-descr.slot-name,
				  "-override-init-value"),
			   object-ctype());
  fer-convert(tlf-descr.evals-builder,
	      override-descr.override-defn.override-defn-init-value,
	      tlf-descr.lexenv, #"assignment", var);
  build-assignment
    (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
     make-unknown-call
       (tlf-descr.evals-builder,
	ref-dylan-defn(tlf-descr.evals-builder,
		       tlf-descr.policy, tlf-descr.source,
		       #"override-init-value-setter"),
	#f,
	list(var, descriptor-leaf)));
end function build-override-value-assignment;


// build-override-function-assignment  -- internal.
//
define function build-override-function-assignment
    (tlf-descr :: <simple-class-tlf-descr>,
     override-descr :: <override-defn-descr>)
 => ();
  let descriptor-leaf
    = make-literal-constant(tlf-descr.evals-builder,
			    override-descr.override-info);
  let leaf
    = convert-init-function(tlf-descr.evals-builder,
			    override-descr.getter,
			    override-descr.override-defn
			      .override-defn-init-function);
  build-assignment
    (tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
     make-unknown-call
       (tlf-descr.evals-builder,
	ref-dylan-defn(tlf-descr.evals-builder,
		       tlf-descr.policy, tlf-descr.source,
		       #"override-init-function-setter"),
	#f,
	list(leaf, descriptor-leaf)));
end function build-override-function-assignment;


// build-tlf-maker  -- internal.
//
define function build-tlf-maker
    (tlf-descr :: <simple-class-tlf-descr>)
 => ();
  unless (tlf-descr.cclass.abstract?)
    //
    // Build the key-defaulter (if concrete)
    // ### Need to write this.
    
    //
    // Build the maker.
    let (maker-region, maker-signature)
      = build-maker-function-body(tlf-descr.tl-builder, tlf-descr.definition);
    
    // Fill in the maker function.
    let ctv = tlf-descr.definition.class-defn-maker-function;
    if (ctv)
      make-function-literal(tlf-descr.tl-builder, ctv, #"function", #"global",
			    maker-signature, maker-region);
    else
      // The maker function isn't a compile-time constant, so add code to
      // the defered evaluations to install it.
      let maker-leaf
	= make-function-literal(tlf-descr.tl-builder,
				#f, #"function", #"local",
				maker-signature, maker-region);
      build-assignment
	(tlf-descr.evals-builder, tlf-descr.policy, tlf-descr.source, #(),
	 make-unknown-call
	   (tlf-descr.evals-builder,
	    ref-dylan-defn(tlf-descr.evals-builder,
			   tlf-descr.policy, tlf-descr.source,
			   #"class-maker-setter"),
	    #f,
	    list(maker-leaf,
		 make-literal-constant(tlf-descr.evals-builder,
				       tlf-descr.cclass))));
    end if;
  end unless;
end function build-tlf-maker;

// build-deferred-evaluations-function  -- internal.
//
define function build-deferred-evaluations-function
    (tlf-descr :: <simple-class-tlf-descr>,
     ctv :: false-or(<ct-function>));
  let func-region
    = build-function-body(tlf-descr.tl-builder, tlf-descr.policy,
			  tlf-descr.source, #f,
			  ctv.ct-function-name, #(),
			  make-values-ctype(#(), #f), #t);
  build-region(tlf-descr.tl-builder,
	       builder-result(tlf-descr.evals-builder));
  
  // ### install the key-defaulter function here?
  
  // Return nothing.
  build-return(tlf-descr.tl-builder, tlf-descr.policy,
	       tlf-descr.source, func-region, #());
  end-body(tlf-descr.tl-builder);
  make-function-literal(tlf-descr.tl-builder, ctv, #"function", #"global",
			ctv.ct-function-signature, func-region);
end function build-deferred-evaluations-function;


// make-descriptors-leaf  -- internal.
//
// not used.
//
/*
define function make-descriptors-leaf
    (builder :: <fer-builder>, policy :: <policy>, source :: <source-location>,
     what :: <byte-string>, for-class :: <cclass>)
    => leaf :: <leaf>;
  let var = make-local-var(builder, symcat(what, "-descriptors"),
			   object-ctype());
  build-assignment
    (builder, policy, source, var,
     make-unknown-call
       (builder,
	ref-dylan-defn(builder, policy, source,
		       symcat("class-", what, "-descriptors")),
	#f,
	list(make-literal-constant(builder, for-class))));
  var;
end;
*/

// Building maker function bodies.
// ==============================

define class <maker-body-descr> (<top-level-form-description>)
  slot definition :: <class-definition>,
    required-init-keyword: definition:;
  slot builder :: <fer-builder>,
    required-init-keyword: builder:;
  slot maker-key-infos :: <stretchy-vector>,
    required-init-keyword: maker-key-infos:;
  slot maker-args :: <stretchy-vector>,
    required-init-keyword: maker-args:;
  slot setup-builder :: <flow-builder>,
    required-init-keyword: setup-builder:;
  slot maker-builder :: <flow-builder>,
    required-init-keyword: maker-builder:;
  slot init-builder :: <flow-builder>,
    required-init-keyword: init-builder:;
  slot cclass :: <cclass>,
    required-init-keyword: cclass:;
  slot direct :: <direct-instance-ctype>,
    required-init-keyword: direct:;
  slot instance-leaf :: <initial-variable>,
    required-init-keyword: instance-leaf:;
//  slot representation :: <representation>,
//    required-init-keyword: representation:;
  slot immediate-rep? :: <boolean>,
    required-init-keyword: immediate-rep?:;
  slot make-immediate-args :: <stretchy-vector>,
    required-init-keyword: make-immediate-args:;
  slot data-word-leaf,
    required-init-keyword: data-word-leaf:;
  slot size-leaf,
    required-init-keyword: size-leaf:;
  slot maker-vector-slot :: false-or(<vector-slot-info>),
    required-init-keyword: maker-vector-slot:;
  slot size-slot :: false-or(<instance-slot-info>),
    required-init-keyword: size-slot:;
  slot policy :: <policy>,
    required-init-keyword: policy:;
  slot source :: <source-location>,
    required-init-keyword: source:;
end class <maker-body-descr>;


define function make-maker-body-descr
    (tl-builder :: <fer-builder>, defn :: <class-definition>)
 => body-descr :: <maker-body-descr>;
  let maker-key-infos = make(<stretchy-vector>);
  let maker-args = make(<stretchy-vector>);
  let setup-builder = make-builder(tl-builder);
  let maker-builder = make-builder(tl-builder);
  let init-builder = make-builder(tl-builder);
  let cclass :: <cclass> = defn.ct-value;
  let direct = make(<direct-instance-ctype>, base-class: cclass);
  let instance-leaf = make-local-var(init-builder, #"instance", direct);
  let representation = pick-representation(direct, #"speed");
  let immediate-rep?
    = instance?(representation, <immediate-representation>);
  let make-immediate-args = make(<stretchy-vector>);
  let data-word-leaf = #f;
  let size-leaf = #f;
  let maker-vector-slot = cclass.vector-slot;
  let size-slot = maker-vector-slot & maker-vector-slot.slot-size-slot;

  let policy = $Default-Policy;
  let source = defn.source-location;
  make(<maker-body-descr>,
       definition: defn, builder: tl-builder,
       maker-key-infos: maker-key-infos, maker-args: maker-args,
       setup-builder: setup-builder, maker-builder: maker-builder,
       init-builder: init-builder, cclass: cclass,
       direct: direct, instance-leaf: instance-leaf,
       // representation: representation,
       immediate-rep?: immediate-rep?,
       make-immediate-args: make-immediate-args,
       data-word-leaf: data-word-leaf, size-leaf: size-leaf,
       maker-vector-slot: maker-vector-slot, size-slot: size-slot,
       policy: policy, source: source);
end function make-maker-body-descr;


// build-maker-function-body  -- internal.
//
define function build-maker-function-body
    (tl-builder :: <fer-builder>, defn :: <class-definition>)
    => (maker-region :: <fer-function-region>,
	signature :: <signature>);
  let maker-descr :: <maker-body-descr>
    = make-maker-body-descr(tl-builder, defn);
  for (slot in maker-descr.cclass.all-slot-infos, index from 0)
    build-maker-slots(maker-descr, slot);
  end for;
  
  let name = make(<derived-name>, how: #"maker",
		  base: maker-descr.definition.defn-name);
  let maker-region
    = build-function-body(maker-descr.builder, maker-descr.policy,
			  maker-descr.source, #f, name,
			  as(<list>, maker-descr.maker-args),
			  maker-descr.cclass, #t);
  build-region(maker-descr.builder,
	       builder-result(maker-descr.setup-builder));
  build-region(maker-descr.builder,
	       builder-result(maker-descr.maker-builder));
  let bytes = maker-descr.cclass.instance-slots-layout.layout-length;
  let base-len
    = make-literal-constant(maker-descr.builder, as(<ct-value>, bytes));
  let len-leaf
    = if (maker-descr.maker-vector-slot)
	let fi = specifier-type(#"<integer>");
	let elsize
	  = maker-descr.maker-vector-slot.slot-representation.representation-size;
	let extra
	  = if (elsize == 1)
	      maker-descr.size-leaf;
	    else
	      let var = make-local-var(maker-descr.builder, #"extra", fi);
	      let elsize-leaf
		= make-literal-constant(maker-descr.builder,
					as(<ct-value>, elsize));
	      build-assignment
		(maker-descr.builder,
		 maker-descr.policy, maker-descr.source, var,
		 make-unknown-call
		   (maker-descr.builder,
		    ref-dylan-defn(maker-descr.builder, maker-descr.policy,
				   maker-descr.source, #"*"),
		    #f,
		    list(maker-descr.size-leaf, elsize-leaf)));
	      var;
	    end;
	let var = make-local-var(maker-descr.builder, #"bytes", fi);
	build-assignment
	  (maker-descr.builder, maker-descr.policy,
	   maker-descr.source, var,
	   make-unknown-call
	     (maker-descr.builder,
	      ref-dylan-defn(maker-descr.builder, maker-descr.policy,
			     maker-descr.source, #"+"),
	      #f,
	      list(base-len, extra)));
	var;
      else
	base-len;
      end;
  build-assignment
    (maker-descr.builder, maker-descr.policy, maker-descr.source,
     maker-descr.instance-leaf,
     if (maker-descr.immediate-rep?)
       make-operation
	 (maker-descr.builder, <primitive>, as(<list>,
				      maker-descr.make-immediate-args),
	  name: #"make-immediate",
	  derived-type: maker-descr.direct.ctype-extent);
     elseif (maker-descr.data-word-leaf)
       make-operation
	 (maker-descr.builder, <primitive>,
	  list(make-literal-constant(maker-descr.builder,
				     maker-descr.cclass),
	       len-leaf, data-word-leaf),
	  name: #"allocate-with-data-word",
	  derived-type: maker-descr.direct.ctype-extent);
     else
       make-operation
	 (maker-descr.builder, <primitive>,
	  list(make-literal-constant(maker-descr.builder,
				     maker-descr.cclass),
	       len-leaf),
	  name: #"allocate", derived-type: maker-descr.direct.ctype-extent);
     end if);
  build-region(maker-descr.builder,
	       builder-result(maker-descr.init-builder));
  build-return(maker-descr.builder, maker-descr.policy,
	       maker-descr.source, maker-region,
	       list(maker-descr.instance-leaf));
  end-body(maker-descr.builder);
  values(maker-region,
	 make(<signature>, specializers: #(),
	      keys: as(<list>, maker-descr.maker-key-infos),
	      all-keys: #t,
	      returns: maker-descr.direct));
end function build-maker-function-body;


// calculate-init-value-and-function  -- internal.
//
define function calculate-init-value-and-function
    (override :: false-or(<override-info>),
     slot :: <instance-slot-info>)
 => (init-value :: type-union(<ct-value>, <boolean>),
     init-function :: type-union(<ct-value>, <boolean>));
  if (override)
    values(override.override-init-value,
	   override.override-init-function);
  else
    values(slot.slot-init-value, slot.slot-init-function);
  end;
end function calculate-init-value-and-function;


define class <maker-slot-descr> (<top-level-form-description>)
  slot instance-slot-info :: <instance-slot-info>,
    required-init-keyword: instance-slot-info:;
  slot slot-name :: false-or(<symbol>),
    required-init-keyword: slot-name:;
//  slot slot-ctype :: <ctype>
//    required-init-keyword: slot-ctype:;
  slot type :: <ctype>,
    required-init-keyword: type:;
  slot type-var :: false-or(<initial-variable>),
    required-init-keyword: type-var:;
  slot override :: false-or(<override-info>),
    required-init-keyword: override:;
  slot init-value :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-value:;
  slot init-function :: type-union(<ct-value>, <boolean>),
    required-init-keyword: init-function:;
end class <maker-slot-descr>;

define function make-maker-slot-descr
    (maker-descr :: <maker-body-descr>,
     slot :: <instance-slot-info>)
 => (slot-descr :: <maker-slot-descr>);
    let slot-name :: false-or(<symbol>)
      = slot.slot-getter & slot.slot-getter.variable-name;
    //
    // Get ahold of the type.
    let slot-type :: <ctype> = slot.slot-type;
    let (type, type-var)
      = if (instance?(slot-type, <unknown-ctype>))
	  let var :: <initial-variable>
	    = make-local-var(maker-descr.maker-builder,
			     symcat(slot-name, "-type"),
			     specifier-type(#"<type>"));
	  build-assignment
	    (maker-descr.maker-builder,
	     maker-descr.policy,
	     maker-descr.source,
	     var,
	     make-unknown-call
	       (maker-descr.maker-builder,
		ref-dylan-defn(maker-descr.maker-builder,
			       maker-descr.policy,
			       maker-descr.source,
			       #"slot-type"),
		#f,
		list(make-literal-constant(maker-descr.maker-builder,
					   slot))));
	  values(object-ctype(), var);
	else
	  values(slot-type, #f);
	end;
    //
    // Find the active override if there is one. 
    let override :: false-or(<override-info>)
      = find-active-override(slot, maker-descr.cclass);
    //	      block (return)
    //		for (override in slot.slot-overrides)
    //		  if (csubtype?(maker-descr.cclass,
    //				override.override-introduced-by))
    //		    return(override);
    //		  end;
    //		finally
    //		  #f;
    //		end;
    //	      end;
    //
    // Get the init-value or init-function, either from the
    // active override or from the slot itself if there is no
    // active override.
    let (init-value :: type-union(<ct-value>, <boolean>),
	 init-function :: type-union(<ct-value>, <boolean>))
      = calculate-init-value-and-function(override, slot);
  make(<maker-slot-descr>,
       instance-slot-info: slot,
       slot-name: slot-name, // slot-ctype: slot-ctype,
       type: type, type-var: type-var, 
       override: override, init-value: init-value,
       init-function: init-function);
end function make-maker-slot-descr;


// build-maker-slots  {<instance-slot-info>}
//    -- method on internal GF.
//
define method build-maker-slots
    (maker-descr :: <maker-body-descr>,
     slot :: <instance-slot-info>)
 => ();
  //
  // If there isn't a getter, this is a bound? slot.  Bound? slots
  // are initialized along with the regular slot.
  if (slot.slot-getter)
    let slot-descr :: <maker-slot-descr>
      = make-maker-slot-descr(maker-descr, slot);
    
    let key = slot.slot-init-keyword;
    if (key)
      let required? = ~slot-descr.override & slot.slot-init-keyword-required?;
      let default = ~(slot-descr.init-value == #t) & slot-descr.init-value;
      let default-bogus? = default & ~cinstance?(default, slot-descr.type);
      let key-info = make(<key-info>, key-name: key, type: slot-descr.type,
			  required: required? | default-bogus?,
			  default: default);
      let init-value-var :: <initial-variable>
	= make-local-var(maker-descr.maker-builder,
			 symcat(slot-descr.slot-name, "-init-value"),
			 slot-descr.type);
      add!(maker-descr.maker-key-infos, key-info);
      if (default)
	add!(maker-descr.maker-args, init-value-var);
	build-slot-init(maker-descr, slot, init-value-var);
	build-slot-init(maker-descr, slot.slot-initialized?-slot,
			make-literal-constant(maker-descr.init-builder,
					      as(<ct-value>, #t)));
      else
	let arg :: <initial-variable>
	  = make-local-var(maker-descr.maker-builder, key, slot-descr.type);
	add!(maker-descr.maker-args, arg);
	let supplied?-arg :: <initial-variable>
	  = make-local-var(maker-descr.maker-builder,
			   symcat(key, "-supplied?"),
			   specifier-type(#"<boolean>"));
	if (key-info.key-needs-supplied?-var)
	  add!(maker-descr.maker-args, supplied?-arg);
	else
	  build-assignment
	    (maker-descr.maker-builder, maker-descr.policy,
	     maker-descr.source, supplied?-arg,
	     make-operation(maker-descr.maker-builder,
			    <primitive>, list(arg),
			    name: #"initialized?"));
	end;
	build-if-body(maker-descr.maker-builder,
		      maker-descr.policy, maker-descr.source,
		      supplied?-arg);
	build-assignment(maker-descr.maker-builder,
			 maker-descr.policy, maker-descr.source,
			 init-value-var, arg);
	build-else(maker-descr.maker-builder, maker-descr.policy,
		   maker-descr.source);
	if (slot-descr.init-value)
	  extract-init-value(maker-descr, slot-descr, init-value-var);
	elseif (slot-descr.init-function)
	  call-init-function(maker-descr, slot-descr, init-value-var);
	elseif (slot.slot-init-keyword-required?)
	  build-assignment
	    (maker-descr.maker-builder, maker-descr.policy,
	     maker-descr.source, #(),
	     make-error-operation
	       (maker-descr.maker-builder,
		maker-descr.policy, maker-descr.source,
		#"missing-required-init-keyword-error",
		make-literal-constant
		  (maker-descr.maker-builder, as(<ct-value>, key)),
		make-literal-constant(maker-descr.maker-builder,
				      maker-descr.cclass)));
	else
	  build-assignment(maker-descr.maker-builder,
			   maker-descr.policy, maker-descr.source,
			   init-value-var,
			   make(<uninitialized-value>,
				derived-type: slot-descr.type.ctype-extent));
	end;
	end-body(maker-descr.maker-builder);
	build-slot-init(maker-descr, slot, init-value-var);
	build-slot-init(maker-descr, slot.slot-initialized?-slot,
			if (slot-descr.init-value | slot-descr.init-function)
			  make-literal-constant(maker-descr.init-builder,
						as(<ct-value>, #t));
			else
			  supplied?-arg;
			end);
      end;
    else
      if (slot-descr.init-value | slot-descr.init-function)
	let init-value-var :: <initial-variable>
	  = make-local-var(maker-descr.maker-builder,
			   symcat(slot-descr.slot-name, "-init-value"),
			   slot-descr.type);
	if (slot-descr.init-value)
	  extract-init-value(maker-descr, slot-descr, init-value-var);
	else
	  call-init-function(maker-descr, slot-descr, init-value-var);
	end;
	build-slot-init(maker-descr, slot, init-value-var);
	build-slot-init(maker-descr, slot.slot-initialized?-slot,
			make-literal-constant(maker-descr.init-builder,
					      as(<ct-value>, #t)));
      else
	build-slot-init
	  (maker-descr, slot, make(<uninitialized-value>,
		      derived-type: slot-descr.type.ctype-extent));
	build-slot-init
	  (maker-descr, slot.slot-initialized?-slot,
	   make-literal-constant(maker-descr.init-builder,
				 as(<ct-value>, #f)));
      end if;
    end if;
  end if;
end method build-maker-slots;


// build-slot-init  -- internal.
//
define function build-slot-init
    (maker-descr :: <maker-body-descr>,
     slot :: false-or(<slot-info>), leaf :: <leaf>) => ();
  if (slot)
    if (maker-descr.immediate-rep?)
      add!(maker-descr.make-immediate-args, leaf);
    else
      let posn
	= get-direct-position(slot.slot-positions,
			      maker-descr.cclass);
      unless (posn)
	error("Couldn't find the position for %s",
	      slot.slot-getter.variable-name);
      end unless;
      if (posn == #"data-word")
	maker-descr.data-word-leaf := leaf;
      else
	let posn-leaf
	  = make-literal-constant(maker-descr.init-builder,
				  as(<ct-value>, posn));
	if (instance?(slot, <vector-slot-info>))
	  // We need to build a loop to initialize every
	  // element.
	  let block-region
	    = build-block-body(maker-descr.init-builder,
			       maker-descr.policy,
			       maker-descr.source);
	  let index :: <initial-variable>
	    = make-local-var(maker-descr.init-builder, #"index",
			     specifier-type(#"<integer>"));
	  build-assignment
	    (maker-descr.init-builder,
	     maker-descr.policy, maker-descr.source, index,
	     make-literal-constant
	       (maker-descr.init-builder, as(<ct-value>, 0)));
	  build-loop-body(maker-descr.init-builder,
			  maker-descr.policy,
			  maker-descr.source);
	  let more? :: <initial-variable>
	    = make-local-var(maker-descr.init-builder, #"more?",
			     specifier-type(#"<boolean>"));
	  build-assignment
	    (maker-descr.init-builder,
	     maker-descr.policy, maker-descr.source, more?,
	     make-unknown-call
	       (maker-descr.init-builder,
		ref-dylan-defn(maker-descr.init-builder,
			       maker-descr.policy,
			       maker-descr.source,
			       #"<"),
		#f,
		list(index, maker-descr.size-leaf)));
	  build-if-body(maker-descr.init-builder,
			maker-descr.policy,
			maker-descr.source, more?);
	  build-assignment
	    (maker-descr.init-builder, maker-descr.policy,
	     maker-descr.source, #(),
	     make-operation
	       (maker-descr.init-builder, <heap-slot-set>,
		list(leaf, maker-descr.instance-leaf,
		     posn-leaf, index),
		slot-info: slot));
	  build-assignment
	    (maker-descr.init-builder, maker-descr.policy,
	     maker-descr.source, index,
	     make-unknown-call
	       (maker-descr.init-builder,
		ref-dylan-defn(maker-descr.init-builder,
			       maker-descr.policy,
			       maker-descr.source,
			       #"+"),
		#f,
		list(index,
		     make-literal-constant
		       (maker-descr.init-builder,
			as(<ct-value>, 1)))));
	  build-else(maker-descr.init-builder,
		     maker-descr.policy, maker-descr.source);
	  build-exit
	    (maker-descr.init-builder, maker-descr.policy,
	     maker-descr.source, block-region);
	  end-body(maker-descr.init-builder);
	  end-body(maker-descr.init-builder);
	  end-body(maker-descr.init-builder);
	else
	  build-assignment
	    (maker-descr.init-builder, maker-descr.policy,
	     maker-descr.source, #(),
	     make-operation
	       (maker-descr.init-builder, <heap-slot-set>,
		list(leaf, maker-descr.instance-leaf, posn-leaf),
		slot-info: slot));
	  if (slot == maker-descr.size-slot)
	    maker-descr.size-leaf := leaf;
	  end if;
	end if;
      end if;
    end if;
  end if;
end function build-slot-init;


// extract-init-value  -- internal.
//
define function extract-init-value
    (maker-descr :: <maker-body-descr>,
     slot-descr :: <maker-slot-descr>,
     init-value-var) => ();
  if (slot-descr.init-value == #t)
    if (slot-descr.override)
      build-assignment
	(maker-descr.maker-builder, maker-descr.policy,
	 maker-descr.source,
	 init-value-var,
	 make-unknown-call
	   (maker-descr.maker-builder,
	    ref-dylan-defn(maker-descr.maker-builder,
			   maker-descr.policy, maker-descr.source,
			   #"override-init-value"),
	    #f,
	    list(make-literal-constant(maker-descr.maker-builder,
				       slot-descr.override))));
    else
      build-assignment
	(maker-descr.maker-builder, maker-descr.policy,
	 maker-descr.source,
	 init-value-var,
	 make-unknown-call
	   (maker-descr.maker-builder,
	    ref-dylan-defn(maker-descr.maker-builder,
			   maker-descr.policy,
			   maker-descr.source,
			   #"slot-init-value"),
	    #f,
	    list(make-literal-constant(maker-descr.maker-builder,
				       slot-descr.instance-slot-info))));
    end if;
  elseif (slot-descr.init-value)
    build-assignment
      (maker-descr.maker-builder, maker-descr.policy,
       maker-descr.source, init-value-var,
       make-literal-constant(maker-descr.maker-builder,
			     slot-descr.init-value));
  else
    error("shouldn't have called extract-init-value "
	    "when init-value is false");
  end;
  
  if (slot-descr.type-var)
    build-assignment
      (maker-descr.maker-builder, maker-descr.policy,
       maker-descr.source, init-value-var,
       make-check-type-operation
	 (maker-descr.maker-builder, maker-descr.policy,
	  maker-descr.source,
	  init-value-var, slot-descr.type-var));
  end;
end function extract-init-value;


define function call-init-function
    (maker-descr :: <maker-body-descr>,
     slot-descr :: <maker-slot-descr>,
     init-value-var) => ();
  if (slot-descr.init-function == #t)
    let init-function-var :: <initial-variable>
      = make-local-var(maker-descr.maker-builder,
		       symcat(slot-descr.slot-name, "-init-function"),
		       function-ctype());
    if (slot-descr.override)
      build-assignment
	(maker-descr.maker-builder, maker-descr.policy,
	 maker-descr.source,
	 init-function-var,
	 make-unknown-call
	   (maker-descr.maker-builder,
	    ref-dylan-defn(maker-descr.maker-builder,
			   maker-descr.policy, maker-descr.source,
			   #"override-init-function"),
	    #f,
	    list(make-literal-constant(maker-descr.maker-builder,
				       slot-descr.override))));
    else
      build-assignment
	(maker-descr.maker-builder, maker-descr.policy,
	 maker-descr.source,
	 init-function-var,
	 make-unknown-call
	   (maker-descr.maker-builder,
	    ref-dylan-defn(maker-descr.maker-builder,
			   maker-descr.policy, maker-descr.source,
			   #"slot-init-function"),
	    #f,
	    list(make-literal-constant(maker-descr.maker-builder,
				       slot-descr.instance-slot-info))));
    end;
    build-assignment
      (maker-descr.maker-builder, maker-descr.policy,
       maker-descr.source, init-value-var,
       make-unknown-call(maker-descr.maker-builder,
			 init-function-var,
			 #f, #()));
  elseif (slot-descr.init-function)
    let init-func-leaf
      = make-literal-constant(maker-descr.maker-builder,
			      slot-descr.init-function);
    build-assignment
      (maker-descr.maker-builder, maker-descr.policy,
       maker-descr.source, init-value-var,
       make-unknown-call(maker-descr.maker-builder,
			 init-func-leaf, #f,
			 #()));
  else
    error("shouldn't have called call-init-function "
	    "when init-function is false");
  end;
  
  if (slot-descr.type-var)
    build-assignment
      (maker-descr.maker-builder, maker-descr.policy,
       maker-descr.source, init-value-var,
       make-check-type-operation
	 (maker-descr.maker-builder, maker-descr.policy,
	  maker-descr.source,
	  init-value-var, slot-descr.type-var));
  end;
end function call-init-function;

// build-maker-slots  {<each-subclass-slot-info>}
//    -- method on internal GF.
//
define method build-maker-slots
    (maker-descr :: <maker-body-descr>,
     slot :: <each-subclass-slot-info>)
 => ();
  // ### Add stuff to the derived-evaluations function to init the
  // slot.  If the slot is keyword-initializable, add stuff to the
  // maker to check for that keyword and change the each-subclass
  // slot.
  error("Can't deal with each-subclass slots yet.");
end method build-maker-slots;


// build-maker-slots  {<class-slot-info>}
//    -- method on internal GF.
//
define method build-maker-slots
    (maker-descr :: <maker-body-descr>,
     slot :: <class-slot-info>)
 => ();
  // ### If the slot is keyword-initializable, add stuff to the maker
  // to check for that keyword and change the class slot.
  error("Can't deal with class slots yet.");
end method build-maker-slots;
  

// build-maker-slots  {<each-subclass-slot-info>}
//    -- method on internal GF.
//
define method build-maker-slots
    (maker-descr :: <maker-body-descr>,
     slot :: <virtual-slot-info>)
 => ();
  // Don't need to do anything for virtual slots.
  #f;
end method build-maker-slots;

// convert-init-function  -- internal.
//
define function convert-init-function
    (builder :: <fer-builder>, getter :: <variable>,
     init-function :: <expression-parse>)
    => res :: <leaf>;
  let slot-name = getter.variable-name;
  let fun-name = make(<derived-name>,
  		      base: make(<basic-name>, symbol: slot-name,
		      		 module: getter.variable-home),
		      how: #"init-function");
  let lexenv = make(<lexenv>, method-name: fun-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let var = make-lexical-var(builder, symcat(slot-name, "-init-function"),
			     source, function-ctype());
  fer-convert(builder, init-function, lexenv, #"let", var);

  let func-region
    = build-function-body(builder, policy, source, #t,
    			  fun-name, #(),
			  object-ctype(), #f);
  let temp = make-local-var(builder, #"result", object-ctype());
  build-assignment(builder, policy, source, temp,
		   make-unknown-call(builder, var, #f, #()));
  build-return(builder, policy, source, func-region, temp);
  end-body(builder);
  make-function-literal(builder, #f, #"function", #"local",
			make(<signature>, specializers: #()),
			func-region);
end function convert-init-function;


// slot-accessor-standin  -- internal.
//
define function slot-accessor-standin
    (slot :: <instance-slot-info>, kind :: one-of(#"getter", #"setter"))
    => standin :: false-or(<ct-function>);
  if (instance?(slot, <vector-slot-info>))
    #f;
  elseif (find-slot-offset(slot, slot.slot-introduced-by))
    let rep = slot.slot-representation;
    let standin-name :: false-or(<symbol>)
      = if (rep == *general-rep*)
	  symcat("general-rep-", kind);
	elseif (rep == *heap-rep*)
	  symcat("heap-rep-", kind);
	else
	  #f;
	end if;
    if (standin-name)
      let defn = dylan-defn(standin-name);
      if (defn)
	defn.ct-value;
      else
	#f;
      end if;
    else
      #f;
    end if;
  end if;
end function slot-accessor-standin;


// might-be-in-data-word  -- internal.
//
define function might-be-in-data-word?
    (slot :: <slot-info>) => res :: <boolean>;
  //
  // For a slot to ever be in the data-word, it must be in the data-word of
  // the class that introduced it.
  slot.slot-introduced-by.data-word-slot == slot;
end function might-be-in-data-word?;


// build-getter  -- internal.
//
define function build-getter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let getter-name
      = make(<derived-name>, how: #"getter",
     	     base: defn.slot-defn-getter.defn-name);
  let lexenv = make(<lexenv>, method-name: getter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = if (instance?(slot, <vector-slot-info>))
		make-lexical-var(builder, #"index", source,
				 specifier-type(#"<integer>"));
	      else
		#f;
	      end if;
  let type = slot.slot-type;
  let region = build-function-body
    (builder, policy, source, #f,
     getter-name,
     if (index)
       list(instance, index);
     else
       list(instance);
     end,
     type, #t);
  let result = make-local-var(builder, #"result", type);
  local
    method get (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let temp = make-local-var(builder, #"initialized?",
				  specifier-type(#"<boolean>"));
	build-assignment
	  (builder, policy, source, temp,
	   make-operation
	     (builder, <heap-slot-ref>,
	      list(instance, init?-offset),
	      derived-type: init?-slot.slot-type.ctype-extent,
	      slot-info: init?-slot));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
      let maybe-data-word? = slot.might-be-in-data-word?;
      if (maybe-data-word?)
	assert(~init?-offset);
	assert(~index);
	let temp = make-local-var(builder, #"data-word?",
				  specifier-type(#"<boolean>"));
	build-assignment
	  (builder, policy, source, temp,
	   make-unknown-call
	     (builder, ref-dylan-defn(builder, policy, source, #"=="), #f,
	      list(offset,
		   make-literal-constant
		     (builder, as(<ct-value>, #"data-word")))));
	build-if-body(builder, policy, source, temp);
	build-assignment
	  (builder, policy, source, result,
	   make-operation
	     (builder, <data-word-ref>, list(instance),
	      derived-type: slot.slot-type.ctype-extent, slot-info: slot));
	build-else(builder, policy, source);
      end if;
      build-assignment
	(builder, policy, source, result,
	 make-operation
	   (builder, <heap-slot-ref>,
	    if (index)
	      list(instance, offset, index);
	    else
	      list(instance, offset);
	    end,
	    derived-type: slot.slot-type.ctype-extent,
	    slot-info: slot));
      if (maybe-data-word?)
	end-body(builder);
      end if;
      unless (init?-offset | slot-guaranteed-initialized?(slot, cclass))
	let temp = make-local-var(builder, #"initialized?", object-ctype());
	build-assignment(builder, policy, source, temp,
			 make-operation(builder, <primitive>, list(result),
					name: #"initialized?"));
	build-if-body(builder, policy, source, temp);
	build-else(builder, policy, source);
	build-assignment
	  (builder, policy, source, #(),
	   make-error-operation
	     (builder, policy, source, #"uninitialized-slot-error",
	      make-literal-constant(builder, slot), instance));
	end-body(builder);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, get);
  build-return(builder, policy, source, region, result);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(cclass, specifier-type(#"<integer>"));
	    else
	      list(cclass);
	    end,
	  returns: type),
     region);
end function build-getter;


// build-setter
//
define function build-setter
    (builder :: <fer-builder>, ctv :: false-or(<ct-method>),
     defn :: <slot-defn>, slot :: <instance-slot-info>)
    => res :: <method-literal>;
  let setter-name
    = make(<derived-name>, how: #"setter",
     	   base: defn.slot-defn-setter.defn-name);
  let init?-slot = slot.slot-initialized?-slot;
  let lexenv = make(<lexenv>, method-name: setter-name);
  let policy = lexenv.lexenv-policy;
  let source = make(<source-location>);
  let type = slot.slot-type;
  let new = make-lexical-var(builder, #"new-value", source, type);
  let cclass = slot.slot-introduced-by;
  let instance = make-lexical-var(builder, #"object", source, cclass);
  let index = if (instance?(slot, <vector-slot-info>))
		let fi = specifier-type(#"<integer>");
		let index = make-lexical-var(builder, #"index", source, fi);
		index;
	      else
		#f;
	      end if;
  let region = build-function-body
    (builder, policy, source, #f,
     setter-name,
     if (index)
       list(new, instance, index);
     else
       list(new, instance);
     end,
     type, #t);
  let result = make-local-var(builder, #"result", type);
  local
    method set (offset :: <leaf>, init?-offset :: false-or(<leaf>)) => ();
      build-assignment(builder, policy, source, #(),
		       make-operation(builder, <heap-slot-set>,
				      if (index)
					list(new, instance, offset, index);
				      else
					list(new, instance, offset);
				      end if,
				      slot-info: slot));
      if (init?-offset)
	let init?-slot = slot.slot-initialized?-slot;
	let true-leaf = make-literal-constant(builder, make(<literal-true>));
	let init-op = make-operation
	  (builder, <heap-slot-set>, list(true-leaf, instance, init?-offset),
	   slot-info: init?-slot);
	build-assignment(builder, policy, source, #(), init-op);
      end;
    end;
  build-slot-posn-dispatch(builder, slot, instance, set);
  build-return(builder, policy, source, region, new);
  end-body(builder);
  make-function-literal
    (builder, ctv, #"method", if (ctv) #"global" else #"local" end,
     make(<signature>,
	  specializers:
	    if (index)
	      list(type, cclass, specifier-type(#"<integer>"));
	    else
	      list(type, cclass);
	    end,
	  returns: type),
     region);
end function build-setter;


// build-slot-posn-dispatch  -- internal.
//
define function build-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let cclass = slot.slot-introduced-by;
  if (cclass.sealed? | cclass.primary?)
    // We don't have to do a runtime slot-position lookup, so make us a static
    // slot accessor method.
    let new-thunk
      = method (offset :: <slot-position>,
		init?-offset :: false-or(<slot-position>))
	    => ();
	  thunk(make-literal-constant(builder, as(<ct-value>, offset)),
		init?-offset
		  & make-literal-constant(builder,
					  as(<ct-value>, init?-offset)));
	end method;
    let position = get-universal-position(slot.slot-positions);
    let init?-slot = slot.slot-initialized?-slot;
    let init?-position
      = (init?-slot & get-universal-position(init?-slot.slot-positions));
    if (position & (init?-slot == #f | init?-position))
      // The slot only ever shows up at one place.  So just use that one
      // place.
      new-thunk(position, init?-position);
    else
      // The slot shows up at multiple positions.  This had better only happen
      // when the class is sealed because we are only supposed to try making
      // a static posn-dispatch when the class is sealed or primary and if the
      // class were primary, then there should only be one possible position
      // for each slot.
      assert(cclass.sealed?);
      
      if (every?(disjoin(abstract?, unique-id), cclass.subclasses))
	// All the concrete subclasses have unique-id's, so we can compute a
	// direct mapping from instance.object-class.unique-id to offset.
	build-unique-id-slot-posn-dispatch
	  (builder, slot, instance-leaf, new-thunk);
      else
	// One or more concrete subclass doesn't have a unique-id so we have
	// to build an instance? tree.
	build-instance?-slot-posn-dispatch
	  (builder, slot, instance-leaf, new-thunk);
      end if;
    end if;
  else
    // Open non-primary class.
    build-runtime-slot-posn-dispatch(builder, slot, instance-leaf, thunk);
  end if;
end function build-slot-posn-dispatch;


// build-unique-id-slot-posn-dispatch  -- internal.
//
define function build-unique-id-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let positions = slot.slot-positions;
  let init?-positions
    = (slot.slot-initialized?-slot
	 & slot.slot-initialized?-slot.slot-positions);
  let ranges = #();
  let prev = #f;
  for (entry in sort!(map(method (subclass)
			    let id = subclass.unique-id;
			    vector(id, id,
				   get-direct-position(positions, subclass),
				   init?-positions
				     & get-direct-position(init?-positions,
							   subclass));
			  end,
			  find-direct-classes(cclass)),
		      test: method (entry1, entry2)
			      entry1[0] < entry2[0];
			    end))
    if (prev == #f)
      ranges := list(entry);
      prev := ranges;
    elseif (prev.head[2] == entry[2] & prev.head[3] == entry[3])
      prev.head[1] := entry[1];
    else
      let new = list(entry);
      prev.tail := new;
      prev := new;
    end;
  finally
    let ranges = as(<simple-object-vector>, ranges);
    let less-then = ref-dylan-defn(builder, policy, source, #"<");
    //
    // Extract the unique id for this argument.
    let class-temp = make-local-var(builder, #"class", object-ctype());
    let obj-class-leaf
      = ref-dylan-defn(builder, policy, source, #"%object-class");
    build-assignment(builder, policy, source, class-temp,
		     make-unknown-call(builder, obj-class-leaf, #f,
				       list(instance-leaf)));
    let id-temp = make-local-var(builder, #"id", object-ctype());
    let unique-id-leaf
      = ref-dylan-defn(builder, policy, source, #"unique-id");
    build-assignment(builder, policy, source, id-temp,
		     make-unknown-call(builder, unique-id-leaf, #f,
				       list(class-temp)));
    local
      method split-range (min, max)
	if (min == max)
	  let entry :: <simple-object-vector> = ranges[min];
	  thunk(entry[2], entry[3]);
	else
	  let half-way-point = ash(min + max, -1);
	  let cond-temp = make-local-var(builder, #"cond", object-ctype());
	  let ctv = as(<ct-value>, ranges[half-way-point][1] + 1);
	  let bound = make-literal-constant(builder, ctv);
	  build-assignment
	    (builder, policy, source, cond-temp,
	     make-unknown-call(builder, less-then, #f,
			       list(id-temp, bound)));
	  build-if-body(builder, policy, source, cond-temp);
	  split-range(min, half-way-point);
	  build-else(builder, policy, source);
	  split-range(half-way-point + 1, max);
	  end-body(builder);
	end;
      end;
    split-range(0, ranges.size - 1);
  end;
end function build-unique-id-slot-posn-dispatch;


// build-instance?-slot-posn-dispatch  -- internal.
//
define function build-instance?-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  break();

  let policy = $Default-Policy;
  let source = make(<source-location>);
  let cclass = slot.slot-introduced-by;
  let positions = as(<list>, slot.slot-positions);
  let init?-positions
    = (slot.slot-initialized?-slot
	 & as(<list>, slot.slot-initialized?-slot.slot-positions));
  local
    method split (classes :: <list>, possible-splits :: <list>)
	=> ();
      let best-test = #f;
      let best-yes-classes = #f;
      let best-yes-count = #f;
      let best-no-classes = #f;
      let best-no-count = #f;
      let best-weight = 0;

      for (split :: <cclass> in possible-splits)
	let yes-classes = #();
	let no-classes = #();
	for (class in classes)
	  if (csubtype?(class, split))
	    yes-classes := pair(class, yes-classes);
	  else
	    no-classes := pair(class, no-classes);
	  end if;
	end for;
	let yes-count
	  = count-distinct-positions(yes-classes, positions, init?-positions);
	let no-count
	  = count-distinct-positions(no-classes, positions, init?-positions);
	let weight = yes-count * no-count;
	if (weight > best-weight)
	  best-test := split;
	  best-yes-classes := yes-classes;
	  best-yes-count := yes-count;
	  best-no-classes := no-classes;
	  best-no-count := no-count;
	  best-weight := weight;
	end if;
      end for;

      let cond-temp = make-local-var(builder, #"cond", object-ctype());
      let type-leaf = make-literal-constant(builder, best-test);
      let instance?-leaf
	= ref-dylan-defn(builder, policy, source, #"instance?");
      build-assignment
	(builder, policy, source, cond-temp,
	 make-unknown-call
	   (builder, instance?-leaf, #f,
	    list(instance-leaf, type-leaf)));
      build-if-body(builder, policy, source, cond-temp);

      if (best-yes-count == 1)
	let characteristic-class = best-yes-classes.first;
	thunk(lookup-position(characteristic-class, positions),
	      lookup-position(characteristic-class, init?-positions));
      else
	split(best-yes-classes,
	      restrict-splits(possible-splits, best-test, #t));
      end if;

      build-else(builder, policy, source);

      if (best-no-count == 1)
	let characteristic-class = best-no-classes.first;
	thunk(lookup-position(characteristic-class, positions),
	      lookup-position(characteristic-class, init?-positions));
      else
	split(best-no-classes,
	      restrict-splits(possible-splits, best-test, #f));
      end if;

      end-body(builder);
    end method split;

  let initial-splits = map(head, positions);
  if (init?-positions)
    for (entry in init?-positions)
      let split :: <cclass> = entry.head;
      unless (member?(split, initial-splits))
	initial-splits := pair(split, initial-splits);
      end unless;
    end for;
  end if;
  split(find-direct-classes(cclass),
	restrict-splits(initial-splits, cclass, #t));
end function build-instance?-slot-posn-dispatch;


// lookup-position  -- internal GF.
//
define generic lookup-position
    (class :: <cclass>, positions :: false-or(<list>))
 => position :: false-or(<integer>);

// lookup-position  {<list>}
//    -- method on internal GF.
//
define method lookup-position (class :: <cclass>, positions :: <list>)
    => res :: false-or(<integer>);
  block (return)
    for (entry in positions)
      if (csubtype?(class, entry.head))
	return(entry.tail);
      end if;
    end for;
    #f;
  end block;
end method lookup-position;


// lookup-position {<false>}
//    -- method on internal GF.
//
define method lookup-position (class :: <cclass>, positions :: <false>)
    => res :: false-or(<integer>);
  #f;
end method lookup-position;


// restrict-splits  -- internal.
//
define function restrict-splits
    (splits :: <list>, class :: <cclass>, if-yes? :: <boolean>)
    => res :: <list>;
  choose(method (split :: <cclass>) => res :: <boolean>;
	   split ~== class & csubtype?(split, class) == if-yes?;
	 end method,
	 splits);
end function restrict-splits;


// count-distinct-positions  -- internal.
//
define function count-distinct-positions
    (classes :: <list>, positions :: <list>,
     init?-positions :: false-or(<list>))
    => res :: <integer>;
  let entries = #();
  for (class in classes)
    let offset = lookup-position(class, positions);
    let init?-offset = lookup-position(class, init?-positions);
    block (next)
      for (entry :: <pair> in entries)
	if (entry.head == offset & entry.tail == init?-offset)
	  next();
	end if;
      end for;
      entries := pair(pair(offset, init?-offset), entries);
    end block;
  end for;
  entries.size;
end function count-distinct-positions;


// build-runtime-slot-posn-dispatch  -- internal.
//
define function build-runtime-slot-posn-dispatch
    (builder :: <fer-builder>, slot :: <instance-slot-info>,
     instance-leaf :: <leaf>, thunk :: <function>)
    => ();
  let policy = $Default-Policy;
  let source = make(<source-location>);

  let class-temp = make-local-var(builder, #"class", object-ctype());
  let obj-class-leaf
    = ref-dylan-defn(builder, policy, source, #"%object-class");
  build-assignment(builder, policy, source, class-temp,
		   make-unknown-call(builder, obj-class-leaf, #f,
				     list(instance-leaf)));

  local
    method make-offset-var
	(name :: <symbol>, slot :: false-or(<instance-slot-info>))
	=> var :: false-or(<abstract-variable>);
      if (slot)
	let var = make-local-var(builder, name,
				 if (slot.might-be-in-data-word?)
				   specifier-type
				     (#(union:, #"<integer>",
					#(singleton:, #"data-word")));
				 else
				   specifier-type(#"<integer>");
				 end if);
	build-assignment
	  (builder, policy, source, var,
	   make-unknown-call
	     (builder,
	      ref-dylan-defn(builder, policy, source, #"find-slot-offset"),
	      #f,
	      list(class-temp, make-literal-constant(builder, slot))));
	var;
      else
	#f;
      end if;
    end method make-offset-var;
  thunk(make-offset-var(#"offset", slot),
	make-offset-var(#"init?-offset", slot.slot-initialized?-slot));
end function build-runtime-slot-posn-dispatch;


// Dumping stuff.
// =============

// dump-od{<define-class-tlf>}
//    -- method on imported GF.
//
// We dump the a define-binding-tlf to establish the name of the
// <real-class-definition>.  Then we dump all the accessor method definitions
// to make sure they get re-instantiated.
//
define method dump-od (tlf :: <define-class-tlf>, state :: <dump-state>) => ();
  let defn = tlf.tlf-defn;
  dump-simple-object(#"define-binding-tlf", state, defn);
  for (slot in defn.class-defn-slots)
    let sealed? = slot.slot-defn-sealed?;
    let getter = slot.slot-defn-getter;
    if (getter.method-defn-of & name-inherited-or-exported?(getter.defn-name))
      dump-od(slot.slot-defn-getter, state);
      if (sealed? & getter.method-defn-of.defn-library ~== defn.defn-library)
	dump-simple-object(#"sealed-domain", state,
			   getter.method-defn-of,
			   defn.defn-library,
			   getter.function-defn-signature.specializers);
      end if;
    end;
    let setter = slot.slot-defn-setter;
    if (setter & setter.method-defn-of
	  & name-inherited-or-exported?(setter.defn-name))
      dump-od(setter, state);
      if (sealed? & setter.method-defn-of.defn-library ~== defn.defn-library)
	dump-simple-object
	  (#"sealed-domain", state, setter.method-defn-of, defn.defn-library,
	   // We don't use the setter specializers, because the first
	   // specializer will be the slot type, not <object>.
	   pair(object-ctype(), getter.function-defn-signature.specializers));
      end if;
    end if;
  end for;
end method dump-od;


// These functions act like getters/setters on the <real-class-definition>, but
// really get/set slots in the cclass.  They are used so that we can dump
// cclass objects without having to reference non-type things.

// class-defn-new-slot-infos  -- internal.
//
define function class-defn-new-slot-infos
    (defn :: <real-class-definition>)
 => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.new-slot-infos;
end function class-defn-new-slot-infos;

// class-defn-new-slot-infos-setter  -- internal.
//
define function class-defn-new-slot-infos-setter
    (vec :: false-or(<simple-object-vector>),
     defn :: <real-class-definition>)
 => ();
  if (vec)
    defn.class-defn-cclass.new-slot-infos := vec;
  end;
end function class-defn-new-slot-infos-setter;


// class-defn-all-slot-infos  -- internal.
//
define function class-defn-all-slot-infos
    (defn :: <real-class-definition>)
 => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.all-slot-infos;
end function class-defn-all-slot-infos;

// class-defn-all-slot-infos-setter  -- internal.
//
define function class-defn-all-slot-infos-setter
    (vec :: false-or(<simple-object-vector>),
     defn :: <real-class-definition>)
 => ();
  if (vec)
    defn.class-defn-cclass.all-slot-infos := vec;
  end;
end function class-defn-all-slot-infos-setter;

// class-defn-override-infos  -- internal.
//
define function class-defn-override-infos
    (defn :: <real-class-definition>) => res :: <simple-object-vector>;
  let class = defn.class-defn-cclass;
  class & class.override-infos;
end function class-defn-override-infos;

// class-defn-override-infos-setter -- internal.
//
define function class-defn-override-infos-setter
    (vec :: false-or(<simple-object-vector>), defn :: <real-class-definition>)
    => ();
  if (vec)
    defn.class-defn-cclass.override-infos := vec;
  end;
end function class-defn-override-infos-setter;

// class-defn-vector-slot  -- internal.
//
define function class-defn-vector-slot
    (defn :: <real-class-definition>) => res :: false-or(<vector-slot-info>);
  let class = defn.class-defn-cclass;
  class & class.vector-slot;
end function class-defn-vector-slot;

// class-defn-vector-slot-setter  -- internal.
//
define function class-defn-vector-slot-setter
    (info :: false-or(<vector-slot-info>), defn :: <real-class-definition>)
    => ();
  let class = defn.class-defn-cclass;
  if (class)
    class.vector-slot := info;
  end;
end function class-defn-vector-slot-setter;

// $class-definition-slots
//
define constant $class-definition-slots
  = concatenate($definition-slots,
		list(class-defn-cclass, class:, #f,
		     %class-defn-defered-evaluations-function, #f,
		       %class-defn-defered-evaluations-function-setter,
		     %class-defn-maker-function, #f,
		       %class-defn-maker-function-setter,
		     class-defn-new-slot-infos, #f,
		       class-defn-new-slot-infos-setter,
    /* ### -- currently recomputed, so we don't really need to dump them.
		     class-defn-all-slot-infos, #f,
		       class-defn-all-slot-infos-setter, */
		     class-defn-override-infos, #f,
		       class-defn-override-infos-setter
    /* ### -- currently recomputed, so we don't really need to dump them.
		     , class-defn-vector-slot, #f,
		       class-defn-vector-slot-setter */));


// class-definition
//
add-make-dumper(#"class-definition", *compiler-dispatcher*,
		<real-class-definition>, $class-definition-slots,
		load-external: #t,
		load-side-effect:
		  method (defn :: <real-class-definition>) => ();
		    let class = defn.class-defn-cclass;
		    if (class)
		      class.class-defn := defn;
		    end;
		  end);


// class-definition
//
add-make-dumper(#"class-definition", *compiler-dispatcher*,
		<local-class-definition>, $class-definition-slots,
		dumper-only: #t);


// init-function-definition
//
add-make-dumper(#"init-function-definition", *compiler-dispatcher*,
		<init-function-definition>,
		$abstract-method-definition-slots,
		load-external: #t);


// maker-function-definition
//
add-make-dumper
  (#"maker-function-definition", *compiler-dispatcher*,
   <maker-function-definition>,
   concatenate
     ($abstract-method-definition-slots,
      list(maker-func-defn-class-defn, class-defn:,
	     maker-func-defn-class-defn-setter)),
   load-external: #t);

// Seals for file compiler/convert/defclass.dylan
// ==============================================

// <define-class-parse> -- subclass of <definition-parse>
define sealed domain make(singleton(<define-class-parse>));
define sealed domain initialize(<define-class-parse>);
// <slot-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<slot-parse>));
// <inherited-slot-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<inherited-slot-parse>));
// <init-arg-parse> -- subclass of <abstract-slot-parse>
define sealed domain make(singleton(<init-arg-parse>));
// <real-class-definition> -- subclass of <class-definition>
define sealed domain make(singleton(<real-class-definition>));
define sealed domain initialize(<real-class-definition>);
// <local-class-definition> -- subclass of <real-class-definition>
define sealed domain make(singleton(<local-class-definition>));
// <slot-defn> -- subclass of <object>
define sealed domain make(singleton(<slot-defn>));
define sealed domain initialize(<slot-defn>);
// <override-defn> -- subclass of <object>
define sealed domain make(singleton(<override-defn>));
define sealed domain initialize(<override-defn>);
// <init-function-definition> -- subclass of <abstract-method-definition>
define sealed domain make(singleton(<init-function-definition>));
define sealed domain initialize(<init-function-definition>);
