RCS-Header: $Header: /scm/cvs/src/d2c/compiler/optimize/limopt.dylan,v 1.1.2.2 2000/06/25 20:59:54 emk Exp $
module: cheese
Copyright: See below.
Synopsis: Optimizer support for limited collections.

//-------------------------------------------------------------------------
// Copyright (C) 1999 Eric Kidd
// (Please add your name and the year you released your changes here.)
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of the author(s) shall not
// be used in advertising or otherwise to promote the sale, use or other
// dealings in this Software without prior written authorization from the
// author(s).
//-------------------------------------------------------------------------


//=========================================================================
//  find-limited-collection-implementation
//=========================================================================
//  Map a limited collection type to the class we'll use to implement it
//  in the runtime. The ctype module uses this information to improve the
//  results of some type operations.
//
//  Return #f if there's no implementation class (as in the case of
//  uninstantiable types), or if you're too lazy to tell the optimizer
//  about it. But don't supply the *wrong* implementation class, or
//  Bad Things will happen.
//
//  Limited types are memoized, so the result of this function is
//  effectively cached.

define method find-limited-collection-implementation
    (type :: <limited-collection-ctype>)
 => (cclass :: false-or(<cclass>))
  select (type.base-class)
    
    // limited(<table>, of: ...)
    // limited(<object-table>, of: ...)
    specifier-type(#"<table>"), specifier-type(#"<object-table>") =>
      if (type.element-type == specifier-type(#"<object>"))
	specifier-type(#"<simple-object-table>");
      else
	specifier-type(#"<limited-object-table>");
      end if;
      
    // limited(<vector>, of: ...)
    // limited(<simple-vector>, of: ...)
    specifier-type(#"<vector>"), specifier-type(#"<simple-vector>") =>
      select (type.element-type)
	specifier-type(#"<integer>") =>
	  specifier-type(#"<simple-integer-vector>");
	otherwise =>
	  #f
      end select;

    otherwise =>
      #f;
  end select;
end method find-limited-collection-implementation;

// Hang our method on the ctype module's hook.
*find-limited-collection-implementation* :=
  find-limited-collection-implementation;


//=========================================================================
//  make-limited-collection-transformer
//=========================================================================
//  We optimize make(<limited-collection>) to call make-limited-collection
//  directly. This allows the compiler to select the appropriate method
//  at compile time.
//
//  We get called (indirectly) by optimize(<component>,<known-call>).
//
//  make(type :: <limited-collection>, #rest args)
//    => apply(make-limited-collection,
//             type.base-class, type.element-type, type, args);

define method make-limited-collection-transformer
    (component :: <component>, call :: <known-call>)
 => (did-anything? :: <boolean>)
  dformat("\nWell, we're making a valiant attempt, anyway.\n");
  block (return)
    local method give-up () return (#f) end;
    
    // Fetch our arguments.
    let (okay?, type, init-keywords)
      = extract-args(call, 1, #f, #t, #f);
    unless (okay?) give-up() end;
    
    // Examine our first argument to see if we can do anything with it.
    let ctype = extract-constant-type(type);
    unless (ctype & instance?(ctype, <limited-collection-ctype>))
      give-up();
    end unless;
    
    dformat("Looks promising!\n");

    // Fire up our builder.
    let builder = make-builder(component);
    let assign = call.dependents.dependent;
    let policy = assign.policy;
    let source = assign.source-location;

    // Create our first two arguments by ripping apart the ctype.
    let base = make-ssa-var(builder, #"base-class",
			    specifier-type(#"<class>"));
    build-assignment(builder, policy, source, base,
		     make-literal-constant(builder, ctype.base-class));
    let contains = make-ssa-var(builder, #"element-type",
				specifier-type(#"<type>"));
    build-assignment(builder, policy, source, contains,
		     make-literal-constant(builder, ctype.element-type));
    
    // Now, build our a call to make-limited-collection.
    let new-call
      = make-unknown-call(builder,
			  ref-dylan-defn(builder, policy, source,
					 #"make-limited-collection"),
			  #f, pair(base, pair(contains,
					      pair(type, init-keywords))));
    insert-before(component, assign, builder-result(builder));
    replace-expression(component, call.dependents, new-call);

    #t;
  end block;
end method make-limited-collection-transformer;

define-transformer(#"make", #(#"<limited-collection>"),
		   make-limited-collection-transformer);
