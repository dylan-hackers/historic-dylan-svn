Module: front
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/front/front.dylan,v 1.24 1995/05/03 07:15:09 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.

/*

assignment
    fer-assignment {abstract}
        set-assignment
	let-assignment

operation
    primitive
    abstract-call [annotatable] {abstract}
        local-call
        known-call
        unknown-call
	error-call
	mv-call
    prologue
    catcher
    set
    self-tail-call
    slot-access {abstract}
	slot-ref
	slot-set
    truly-the


variable-info
    module-var-info
	module-almost-constant-var-info
    debug-named-info {abstract}
        lambda-var-info [source-location-mixin]
	values-cluster-info
	local-var-info

leaf
    constant
	literal-constant
	definition-constant-leaf

    function-literal
	method-literal
	    lambda [method-region, dependent-mixin, annotatable]
	    hairy-method-literal [source-location-mixin]
        exit-function

component
    fer-component

block-region
    fer-block-region

exit
    pitcher

object
    environment [annotatable]
    tail-set [annotatable]

*/


// Basic front end (FE) SSA classes:

// The <fer-assignment> adds policy information.  This can't be associated
// with expressions, since different uses of the expression can have different
// policies.
//
define abstract class <fer-assignment> (<assignment>)
  //
  // The policy environment this <operation> was converted in.
  slot policy :: <policy>, required-init-keyword: policy:;
end class;

// Marks a create-point for a lexical variable.  We allow there to be more than
// one (useful for representing loops.)  Let-assignments should only be used on
// lexical variables.
//
define class <let-assignment> (<fer-assignment>)
  //
  // The next link in the chain of lets in this component.
  slot let-next :: false-or(<let-assignment>), required-init-keyword: next:;
end class;

// An assignment that doesn't create a new "variable" (that would side-effect
// any indirect value cell.)  Set-assignments can be used on temporary and
// global variables as well as lexical ones.
//
define class <set-assignment> (<fer-assignment>)
end class;


// The <primitive> operation represents some built in primitive operation.
//
define class <primitive> (<operation>)
  slot name :: <symbol>, required-init-keyword: name:;
end;


// The <abstract-call> operation represents any function call.
// In Operands, the called function is first, followed by the args.
//
define abstract class <abstract-call> (<operation>, <annotatable>)
end class;

// A syntactically correct call where the function is a fixed-arg method
// literal.  Variable arg calls can be converted into fixed-arg local calls
// (e.g. to the main entry of a function with keywords.)  These calls can
// will be let-converted if there is only one reference.
// 
define class <local-call> (<abstract-call>)
end class;

// A syntactically correct call to a known global definition (or literal
// extracted from one).  These calls will never be let-converted (but might
// be inlined, which is equivalent).
//
define class <known-call> (<abstract-call>)
end class;

// An arbitrary function call where the function call might be computed and the
// argument syntax might be incorrect.
//
define class <unknown-call> (<abstract-call>)
end class;

// A call that is known to be in error.  Basically, the same as <unknown-call>
// but we've given up trying to optimize it.
//
define class <error-call> (<abstract-call>)
end;

// In a MV-Call, there is one argument which must be a values cluster.  This
// values cluster is spread out to form the actual arguments to the called
// function.  If the resulting actual parameters are not syntactically legal
// (e.g. not enough required args, etc.), then an error will be signalled.
//
define class <mv-call> (<abstract-call>)
end class;

// A prologue is used to represent the incomming arguments to a function.
// 
define class <prologue> (<operation>)
  slot lambda :: <lambda>, required-init-keyword: lambda:;
end;

// A catcher is used to receive the values from an exit-function.
// 
define class <catcher> (<operation>)
  //
  // A catcher depends on nothing.
  inherited slot depends-on, init-value: #f;
  //
  // If there is an exit function that jumps to this block, then this is it.
  // If false, then all potential exits are explicitly represented by
  // <exit> regions.
  slot exit-function :: false-or(<exit-function>), init-value: #f;
  //
  // The block-region this catcher is for.
  slot target-region :: <fer-exit-block-region>,
    required-init-keyword: target-region:;
end;

// A set operation is used to change a global variable.
// 
define class <set> (<operation>)
  //
  // Set operations return nothing.
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
  //
  // The definition for the variable being set.
  slot variable :: <definition>, required-init-keyword: var:;
end;

// A self-tail-call is used to represent the rebinding of the arguments
// once we have converted a tail-call of ourselves into a loop.
//
define class <self-tail-call> (<operation>)
  //
  // Self tail calls return nothing.
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
  //
  // The function we are self tail calling.
  slot self-tail-call-of :: <lambda>,
    required-init-keyword: of:;
  //
  // The next self tail call in this method.
  slot next-self-tail-call :: false-or(<self-tail-call>),
    required-init-keyword: next-self-tail-call:;
end;

define abstract class <slot-access> (<operation>)
  slot slot-info :: <slot-info>, required-init-keyword: slot-info:;
end;

define class <slot-ref> (<slot-access>)
end;

define class <slot-set> (<slot-access>)
  inherited slot derived-type,
    init-function: curry(make-values-ctype, #(), #f);
end;

define class <truly-the> (<operation>)
  slot guaranteed-type :: <ctype>, required-init-keyword: derived-type:;
end;



// Constants and variables:

// <Constant> objects represent known constant values.
//
define abstract class <constant> (<leaf>)
end class;

// <Literal-Constant> is a constant with a manifest compile-time value.
//
define class <literal-constant> (<constant>)

  // The value of the constant.
  slot value :: <ct-value>, required-init-keyword: value:;
end class;

// Represents a constant module var.  We point to the <module-var> for the
// actual info, and only represent the references for the data-flow framework.
//
define class <definition-constant-leaf> (<constant>)
  slot const-defn :: <definition>, required-init-keyword: const-defn:;
end class;


// Assignable variables:

// We don't have FER-specific subclasses of <abstract-variable>, instead we
// have subclasses of <variable-info>.

// This "variable" represents an arbitrary number of values which are the
// result an expression in a tail position or where multiple values are
// expected.
//

define abstract class <debug-named-info> (<variable-info>)
  slot debug-name :: <symbol>, required-init-keyword: debug-name:;
end class;

define class <values-cluster-info> (<debug-named-info>)
end class;

define class <local-var-info> (<debug-named-info>)
end class;

define class <lexical-var-info> (<debug-named-info>, <source-location-mixin>)
// ??? stuff to handle set & closure vars?
end class;

define class <module-var-info> (<variable-info>)
  slot var-defn :: <definition>, required-init-keyword: var-defn:;
end class;
  
define class <module-almost-constant-var-info> (<module-var-info>)
end class;
  



/// Function literals:

define constant <function-visibility>
  = one-of(#"global", #"local", #"deleted");

define abstract class <function-literal> (<leaf>)

  inherited slot derived-type, init-function: function-ctype;

  // An indication of what kind of references to this function can exist:
  //
  // Local: all references are explicit dependents of the leaf.
  // Global: some other references might exist, so assume the worst.
  // Deleted: no references will ever exist from now on.
  //
  slot visibility :: <function-visibility>, init-value: #"local";

  // This is the general-case used when we can't statically analyze a call, due
  // to either the function or the arguments not being sufficiently constant.
  // It checks the argument syntax and type restrictions, then calls the main
  // entry.  No general entry is needed if we never generate a user-visible
  // Dylan object for this function, which happens with local functions and
  // sealed methods.  This is also false in methods that are themselves
  // entry-points.
  // 
  slot general-entry :: false-or(<function-literal>), init-value: #f;

  // ??? arg documentation?
end class;


define abstract class <method-literal> (<function-literal>)

  // The generic entry is somewhat similar to the general-entry, but doesn't
  // have to deal with error cases that have been picked off by the generic
  // dispatch mechanism.  It takes the required arguments followed by
  // next-method info, followed by an argument context pointer and an argument
  // count.
  //
  // The required arguments are guaranteed to be of the correct type,
  // and more args will only be supplied if they are syntactically legal.
  // Keyword arg legality has already been done; unrecognized keywords are
  // quietly ignored.  This can also be false when not needed.
  //
  slot generic-entry :: false-or(<lambda>), init-value: #f;

  // ??? inlinep/inline expansion?  An <expression>?  convert environment?
end class;


// The Lambda only deals with required arguments.  Keyword and rest arguments
// are represented by special helper lambdas and <hairy-method-literal>
// objects.
//
define class <lambda> (<method-literal>, <method-region>, <annotatable>)

  slot name :: <byte-string>, init-value: "<unknown>", init-keyword: name:;

  // List of lexical varibles for args.
  slot prologue :: <prologue>;

  // List of the argument types.
  slot argument-types :: <list>, required-init-keyword: argument-types:;

  // The result type of this function.
  slot result-type :: <values-ctype>, init-function: wild-ctype;

  // A list of all the functions directly called from this function
  // using a non-let-converted local call.  May include deleted functions
  // because nobody bothers to clear them out.
  slot calls :: <list>, init-value: #();

  // The block self tail calls should exit to.  #f if we haven't inserted it
  // yet (i.e. haven't found any self tail calls yet).
  slot self-call-block :: false-or(<block-region>), init-value: #f;

  // Chain of all the self tail calls in this lambda.  Linked via
  // next-self-tail-call.
  slot self-tail-calls :: false-or(<self-tail-call>), init-value: #f;

  // The structure which represents the environment that this Function's
  // variables are allocated in.
  slot environment :: <environment>,
    init-function: curry(make, <environment>);

  // If this function is or ever was an entry-point for some other function,
  // then this is that function.
  slot entry-for :: false-or(<function-literal>), init-value: #f;
end class;

define method initialize (lambda :: <lambda>, #key) => ();
  lambda.prologue
    := make(<prologue>, lambda: lambda, depends-on: #f,
	    // ### The depends-on: shouldn't be needed, but Mindy is broken.
	    derived-type: make-values-ctype(lambda.argument-types, #f));
end;

// The <Hairy-Method-Literal> leaf is used to represent hairy methods.  The
// value returned by the function is the value which results from calling the
// <Hairy-Method-Literal>.
// 
// Local call analysis parses the arguments to calls with only fixed
// arguments or recognizable keyword arguments, and turns it into a call to
// the main entry.
//
define class <hairy-method-literal> (<method-literal>, <source-location-mixin>)

  // The signature for this function.
  slot signature :: <signature>, required-init-keyword: signature:;

  // The main entry-point into the function, which takes all arguments
  // including keywords as fixed arguments.  The format of the arguments must
  // be determined by examining the signature.  This may be used by callers
  // that supply the required arguments and know how to default any others.
  slot main-entry :: <lambda>, required-init-keyword: main-entry:;
end class;


// An <exit-function> is a magical function literal that represents
// the exit-function for a block in situations where a non-local exit is
// possible.
//
define class <exit-function> (<function-literal>)
  //
  // The region that this exit function exits to.
  slot catcher :: <catcher>,
    required-init-keyword: catcher:
end class;


// FE region classes:

define class <fer-component> (<component>)
  //
  // List of <function-literal>s for functions that are newly introduced or
  // that have new references to them.
  slot reanalyze-functions :: <list>, init-value: #();

  // Chain of all the lets (though let-next) in this component.  Used by
  // environment analysis.  Deleted lets are left in this chain, so beware.
  slot all-lets :: false-or(<let-assignment>), init-value: #f;

  // String that is some sort of name for the code in this component.
  slot name :: <byte-string>, init-value: "<unknown>";

end class;


// The FER-Block-Region is a subclass of Block-Region which holds information
// related to handling of non-local exits.
//
define abstract class <fer-block-region> (<block-region>)
end class;

// FER-Exit-Block-Region represents a user-level exit procedure target.
//
define class <fer-exit-block-region> (<fer-block-region>)
  //
  // The catcher operation for this block.  #f if we haven't created one
  // yet or if the catcher has been optimized away.
  slot catcher :: union(<false>, <catcher>), init-value: #f;
end class;

// <pitcher> -- an exit that throws some values also.
// 
define class <pitcher> (<exit>, <dependent-mixin>)
  //
  // The type being pitched.
  slot pitched-type :: <values-ctype>, init-function: wild-ctype;
end;

// FER-Cleanup-Block-Region represents a block/cleanup clause.  Somehow...
//
define class <fer-cleanup-block-region> (<fer-block-region>)
end class;


// Misc drek:

// The Environment structure represents the result of Environment analysis.
//
define class <environment> (<annotatable>)
  slot closure-vars :: false-or(<closure-var>), init-value: #f;
end class;

define class <closure-var> (<object>)
  slot original-var :: <ssa-variable>, required-init-keyword: original:;
  slot copy-var :: <ssa-variable>, required-init-keyword: copy:;
  slot closure-next :: false-or(<closure-var>), required-init-keyword: next:;
end;


// The Tail-Set structure is used to accmumlate information about
// tail-recursive local calls.  The "tail set" is effectively the transitive
// closure of the "is called tail-recursively by" relation.
// 
// All functions in the same tail set share the same <Tail-Set> structure.
// When optimization discovers new TR calls, dinstinct tail sets will be
// merged.  The tail set is somewhat approximate, because it is too early to be
// sure which calls will be TR.  Any call that *might* end up TR causes
// tail-set merging.
//
define class <tail-set> (<annotatable>)

  // A list of all the lambdas in this tail set.
  slot lambdas :: <list>, required-init-keyword: lambdas:;

  // Our current best guess of the type returned by these functions.  This is
  // the union across all the functions of the return <operation>'s Result-Type.
  // excluding local calls.
  slot type :: <values-ctype>, init-function: wild-ctype;
end class;
