Module:   analyze-generics
Synopsis: Research code for improving generic function dispatch.
Author:   Eric Kidd <eric.kidd@pobox.com>


//=========================================================================
//  Collection Types
//=========================================================================
//  To speed up processing (and reduce memory usage), we declare several
//  types of limited collections. This code uses the old-style limited
//  collections provided by d2c, not the full-fledged Dylan ones.

define limited-collection <int-vector> (<vector>) of <integer> = 0;
define limited-collection <int-vector-vector> (<vector>) of <int-vector> =
  make(<int-vector>, size: 0);


//=========================================================================
//  compute-generic-info
//=========================================================================
//  Build a <generic-info> structure for the given <generic-definition>.

define function compute-generic-info (gf :: <generic-definition>)
 => (info :: <generic-info>)
  let arg-count = gf.function-defn-signature.specializers.size;
  let arg-info = make(<stretchy-vector>);
  for (i from 0 below arg-count)
    add!(arg-info, make(<argument-info>, generic-defn: gf, position: i));
  end for;
  make(<generic-info>, defn: gf, arguments: arg-info);
end function compute-generic-info;


//=========================================================================
//  <generic-info>
//=========================================================================
//  We need to collect information about every generic function.

define class <generic-info> (<object>)

  // Our underlying <generic-definition>. For now, we don't duplicate any
  // data from this definition.
  slot generic-defn :: <generic-definition>,
    required-init-keyword: defn:;

  // Information about each argument position.
  slot generic-arguments :: <stretchy-vector>,
    required-init-keyword: arguments:;

end class <generic-info>;


//=========================================================================
//  <argument-info>
//=========================================================================
//  We need to collect various information about each argument position of
//  every generic function.

define class <argument-info> (<object>)

  // The generic function and argument position of this argument.
  slot argument-generic-defn :: <generic-definition>,
    required-init-keyword: generic-defn:;
  slot argument-position :: <integer>,
    required-init-keyword: position:;

  // Some argument positions will resist compile-time analysis for one
  // reason or another. We borrow some terminology from d2c and
  // refer to these as 'hairy'. 
  slot argument-hairy? :: <boolean> = #t;

  // Do all (compile-time) methods specialize on the same cclass in this
  // argument position? (Not defined for hairy arguments.)
  slot argument-specializers-identical? :: <boolean>;

  // The set of <cclass>es on which arguments specialize.
  // (Not defined for hairy arguments.)
  slot argument-specializer-cclasses :: <stretchy-vector>;

  // Can this argument position ever affect which method gets selected?
  // (Not defined for hairy arguments.)
  slot argument-may-affect-dispatch? :: <boolean>;

  // Our dispatch table.
  // (Not defined for hairy arguments.)
  slot argument-dispatch-table :: <int-vector>;

end class <argument-info>;


define method initialize
    (arginfo :: <argument-info>, #key, #all-keys) => ()

  next-method();

  let gf = arginfo.argument-generic-defn;
  let pos = arginfo.argument-position;

  // Get our specializers, and do some simple analysis.
  let specializer-cclasses = compute-specializer-cclasses(gf, pos);

  if (specializer-cclasses)

    // Set our boolean flags appropriately.
    arginfo.argument-hairy? := #f;
    arginfo.argument-specializers-identical? :=
      (specializer-cclasses.size <= 1);
    arginfo.argument-may-affect-dispatch? :=
      (~arginfo.argument-specializers-identical?
	 | ~gf.generic-defn-sealed?
	 | gf.function-defn-hairy?);

    // Record our specializers.
    arginfo.argument-specializer-cclasses := specializer-cclasses;

  end if;

end method;


//=========================================================================
//  compute-specializer-cclasses
//=========================================================================
//  Given a generic function and an argument position, return the set of
//  <cclasses> on which known methods specialize. If this is impossible for
//  some reason (perhaps one of the methods is hairly, or specializes on
//  a non-class type), return #f instead.

define function compute-specializer-cclasses
    (gf :: <generic-definition>, pos :: <integer>)
 => (cclasses :: false-or(<stretchy-vector>))
  block (return)

    // If something goes wrong, we call this function to perform a
    // non-local exit.
    local method give-up-if (condition) => ()
	    if (condition)
	      return(#f);
	    end if;
	  end method give-up-if;

    // Collect the set of <cclass>es from each method. We use an
    // <object-table> to eliminate duplicate values.
    let cclass-set = make(<object-table>);
    for (meth in gf.generic-defn-methods)
      let specializer = meth.function-defn-signature.specializers[pos];
      give-up-if(meth.function-defn-hairy?);
      give-up-if(~meth.method-defn-congruent?);
      give-up-if(~instance?(specializer, <cclass>));
      cclass-set[specializer] := specializer;
    end for;

    // Convert the elements of our table into a stretchy vector.
    as(<stretchy-vector>, cclass-set);
  end block;
end function compute-specializer-cclasses;


//=========================================================================
//  Class IDs & Superclass IDs
//=========================================================================
//  

define variable *cclass-to-id-map* :: <object-table> = make(<object-table>);
define variable *superclass-ids* :: <int-vector-vector> =
  make(<int-vector-vector>, size: 0);

define function compute-cclass-information () => ()

  // Assign sequential unique IDs to every class. We do a preorder
  // traversal through our subclasses' primary direct superclasses.
  local method assign-ids (class :: <cclass>, this-id :: <integer>)
	 => (next-id :: <integer>)
	  *cclass-to-id-map*[this-id] := class;
	  let next-id = this-id + 1;
	  for (subclass in class.direct-subclasses)
	    if (class == subclass.direct-superclasses.first)
	      next-id := assign-ids(subclass, next-id);
	    end if;
	  end for;
	  next-id;
	end method;
  *cclass-to-id-map* := make(<object-table>);
  assign-ids(dylan-value(#"<object>"), 0);
  
  let cclasses = all-cclass-objects();
  *superclass-ids* := make(<int-vector-vector>, size: cclasses.size);

  for (cclass in sorted-cclasses)
    *cclass-to-id-map*[cclass] := id;
  end for;

end function;


//=========================================================================
//  Main Program
//=========================================================================

define function analyze-generics () => ()
  format(*debug-output*, "Analyzing generic functions and classes.\n");

  // Fetch our raw data.
  let classes = all-cclass-objects();
  let generics = all-generic-defintion-objects();

  // Find all generic functions with more than one method. (This step is
  // necessary because the compiler treats all functions as generics, even
  // if they only have one method.
  let real-generics = choose(method (gf)
			       gf.generic-defn-methods.size > 1
			     end,
			     generics);

  // Open up our output file.
  let analysis = make(<file-stream>,
		      locator: "gf-analysis.txt",
		      direction: #"output",
		      if-exists: #"replace");

  block ()

    format(analysis, "Found %d classes, %d real generics (%d total).\n\n",
	   classes.size, real-generics.size, generics.size);
    
    // Summarize each generic function (and count the total number of
    // methods and argument positions).
    let total-arg-positions = 0;
    let total-methods = 0;
    for (gf in real-generics)

      let gf-info = compute-generic-info(gf);
      let args = gf-info.generic-arguments;

      let active-arg-position-count =
	if (any?(argument-hairy?, args)) 
	  -1; // Magic value for our output.
	else
	  size(choose(argument-may-affect-dispatch?, args));
	end if;

      let name = gf.defn-name;
      let arg-position-count = gf.function-defn-signature.specializers.size;
      let method-count = gf.generic-defn-methods.size;
      let sealed? = gf.generic-defn-sealed?;
      let exported? = gf.defn-name.name-inherited-or-exported?;
      format(analysis, "%-6s %-8s %2d %2d %3d %s\n",
	     if (sealed?) "sealed" else "open" end,
	     if (exported?) "exported" else "private" end,
	     arg-position-count, active-arg-position-count, method-count,
	     name);
      total-arg-positions := total-arg-positions + arg-position-count;
      total-methods := total-methods + method-count;

      for (arg in args)
	unless (arg.argument-hairy?)
	  format(analysis, "  %d:", arg.argument-position);
	  for (specializer in arg.argument-specializer-cclasses)
	    format(analysis, " %s", specializer)
	  end for;
	  format(analysis, "\n");
	end unless;
      end for;
      format(analysis, "\n");
    end for;
    
    format(analysis, "Found %d argument positions, %d methods.\n",
	   total-arg-positions, total-methods);

  cleanup
    close(analysis);
  end block;
end;


//=========================================================================
//  Main Program
//=========================================================================

define function analyze-generics () => ()
  format(*debug-output*, "Analyzing generic functions and classes.\n");

  // Fetch our raw data.
  let classes = all-cclass-objects();
  let generics = all-generic-defintion-objects();

  // Find all generic functions with more than one method. (This step is
  // necessary because the compiler treats all functions as generics, even
  // if they only have one method.
  let real-generics = choose(method (gf)
			       gf.generic-defn-methods.size > 1
			     end,
			     generics);

  // Open up our output file.
  let analysis = make(<file-stream>,
		      locator: "gf-analysis.txt",
		      direction: #"output",
		      if-exists: #"replace");

  block ()

    format(analysis, "Found %d classes, %d real generics (%d total).\n\n",
	   classes.size, real-generics.size, generics.size);
    
    // Summarize each generic function (and count the total number of
    // methods and argument positions).
    let total-arg-positions = 0;
    let total-methods = 0;
    for (gf in real-generics)

      let gf-info = compute-generic-info(gf);
      let args = gf-info.generic-arguments;

      let active-arg-position-count =
	if (any?(argument-hairy?, args)) 
	  -1; // Magic value for our output.
	else
	  size(choose(argument-may-affect-dispatch?, args));
	end if;

      let name = gf.defn-name;
      let arg-position-count = gf.function-defn-signature.specializers.size;
      let method-count = gf.generic-defn-methods.size;
      let sealed? = gf.generic-defn-sealed?;
      let exported? = gf.defn-name.name-inherited-or-exported?;
      format(analysis, "%-6s %-8s %2d %2d %3d %s\n",
	     if (sealed?) "sealed" else "open" end,
	     if (exported?) "exported" else "private" end,
	     arg-position-count, active-arg-position-count, method-count,
	     name);
      total-arg-positions := total-arg-positions + arg-position-count;
      total-methods := total-methods + method-count;

      for (arg in args)
	unless (arg.argument-hairy?)
	  format(analysis, "  %d:", arg.argument-position);
	  for (specializer in arg.argument-specializer-cclasses)
	    format(analysis, " %s", specializer)
	  end for;
	  format(analysis, "\n");
	end unless;
      end for;
      format(analysis, "\n");
    end for;
    
    format(analysis, "Found %d argument positions, %d methods.\n",
	   total-arg-positions, total-methods);

  cleanup
    close(analysis);
  end block;
end;
