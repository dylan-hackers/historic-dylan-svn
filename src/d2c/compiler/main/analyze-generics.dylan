Module:   analyze-generics
Author:   Eric Kidd <eric.kidd@pobox.com>
Synopsis: Research code for improving generic function dispatch.
          Heavily based on paper by Dujardin, et al.

/*

TODO - 

/ Assign class IDs using reversed post-order traversal
/ Assign pole numbers in sequence
  Calculate minimal pole sets
  Fix pole counts

*/


//=========================================================================
//  Collection Types
//=========================================================================
//  To speed up processing (and reduce memory usage), we declare several
//  types of limited collections. This code uses the old-style limited
//  collections provided by d2c, not the full-fledged Dylan ones.

define limited-collection <int-vector> (<vector>) of <integer> = 0;
define limited-collection <int-vector-vector> (<vector>) of <int-vector> =
  make(<int-vector>, size: 0);
define limited-collection <stretchy-int-vector> (<stretchy-vector>)
  of <integer> = 0;
define limited-collection <cclass-vector> (<vector>) of <cclass>;


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

  // Our argument array (part of our dispatch table).
  // (Not defined for hairy arguments.)
  slot argument-array :: <int-vector>;

  // The type of each pole in the array.
  slot argument-pole-type-array :: <vector>;

  // Some summary statistics about our argument array.
  // (Not defined for hairy arguments.)
  slot argument-total-i-poles :: <integer>;
  slot argument-artificial-i-poles :: <integer>;

end class <argument-info>;

define method initialize
    (arginfo :: <argument-info>, #key, #all-keys) => ()

  next-method();

  let gf = arginfo.argument-generic-defn;
  let pos = arginfo.argument-position;

  // Get our specializers, and do some simple analysis.
  let specializer-cclasses = compute-specializer-cclasses(gf, pos);

  if (specializer-cclasses)

    // Record our specializers.
    arginfo.argument-specializer-cclasses := specializer-cclasses;

    // Set our boolean flags appropriately.
    arginfo.argument-hairy? := #f;
    arginfo.argument-specializers-identical? :=
      (specializer-cclasses.size <= 1);
    arginfo.argument-may-affect-dispatch? :=
      (~arginfo.argument-specializers-identical?
	 | ~gf.generic-defn-sealed?
	 | gf.function-defn-hairy?);

    // Prepare to build our dispatch table.
    let pole-table = make(<int-vector>, size: cclass-count(), fill: 0);
    let pole-type-table = make(<vector>, size: cclass-count(), fill: " ");

    // Mark each specializer, so we can give it a primary i-pole.
    for (specializer in specializer-cclasses)
      pole-table[specializer.cclass-id] := -1;
    end for;

    // Support code for creating new poles.
    let next-pole-id = 0;
    let pole-cclass-ids = make(<stretchy-int-vector>);
    local method create-pole (cclass-id :: <integer>, ptype :: <string>) => ()
	    pole-table[cclass-id] := next-pole-id;
	    pole-type-table[cclass-id] := ptype;
	    add!(pole-cclass-ids, cclass-id);
	    next-pole-id := next-pole-id + 1;
	  end method;

    // Assign an i-pole to <object>.
    if (pole-table[0] == -1)
      // We have an ordinary primary pole for <object>.
      create-pole(0, "P");
    else
      // No methods specialize on <object>, so insert an error pole.
      create-pole(0, "E");
    end if;

    // Assign i-poles to the rest of the classes (skipping <object>).
    for (i from 1 below cclass-count())
      if (pole-table[i] == -1)
	// This is a primary i-pole.
	create-pole(i, "P");
      else
	let supers = superclass-ids(i);
	let closest = single-closet-pole(pole-table, pole-cclass-ids, supers);

	// Verify our closest pole computation.
	//let check = single-closet-pole-check(pole-table, pole-cclass-ids, i);
	//unless (closest == check)
	//  error("single-closest-pole failed check");
	//end unless;

	if (closest)
	  // Copy down closest i-pole.
	  pole-table[i] := closest;
	else
	  // Our superclasses have different i-poles, so assign a new one.
	  create-pole(i, "S");
	end if;
      end if;
    end for;
    
    // Record our argument array & relevant statistics.
    arginfo.argument-array := pole-table;
    arginfo.argument-pole-type-array := pole-type-table;
    arginfo.argument-total-i-poles := next-pole-id;
    arginfo.argument-artificial-i-poles :=
      next-pole-id - specializer-cclasses.size;

  end if;
end method;


//=========================================================================
//  single-closet-pole
//=========================================================================
//  Determine whether or not we have one i-pole which is closer than all
//  the others. This will only be true if all of our superclasses have the
//  the same i-pole, or if one of our i-poles is a subclass of all the
//  others.
//
//  This code is based on the pseudo-closest-poles algorithm from Dujardin,
//  et al. It's fairly clever, and relies heavily on the fact that our
//  i-pole numbers are assigned in ascending order over a sorted class
//  list.
//
//  Details:
//
//  If we have a single pole closer than all the others, it must be a
//  subclass of all our other poles. And if it is the subclass of all our
//  other poles, it will have the largest ID, thanks to the efforts of
//  topologically-sorted-cclasses and our i-pole assignment algorithm.
//  So we find the largest i-pole, and *then* do *one* set of subtype
//  checks.

define function single-closet-pole
    (pole-table :: <int-vector>,
     pole-cclass-ids :: <stretchy-int-vector>,
     supers :: <int-vector>)
 => (closest :: false-or(<integer>))
  if (supers.size == 1)
    pole-table[supers.first];
  else
    block (return)
      
      // Scan over our superclasses, checking to see if our i-poles
      // are identical, and finding our largest i-pole.
      let first-pole = pole-table[supers.first];
      let all-identical? = #t;
      let current-largest = first-pole;
      for (super in supers)
	let pole = pole-table[super];
	if (pole ~= first-pole)
	  all-identical? := #f;
	end if;
	if (pole > current-largest)
	  current-largest := pole;
	end if;
      end for;

      // If all our i-poles are identical, we're OK.
      if (all-identical?)
	return(first-pole);
      end if;

      // Return #f if our largest i-pole doesn't hide all the rest.
      let candidate = pole-cclass-ids[current-largest].id-cclass;
      for (super in supers)
	let pole-cclass-id = pole-cclass-ids[pole-table[super]];
	unless (fast-subclass?(candidate, pole-cclass-id.id-cclass))
	  return(#f);
	end unless;
      end for;

      // Our largest i-pole hid all the rest.
      current-largest;
    end block;
  end if;
end function single-closet-pole;

// Use the fast subclass check algorithm from Vitek, et al.
// The type inclusion matrix has already been computed by the compiler.
//
define inline function fast-subclass? (c1 :: <cclass>, c2 :: <cclass>)
 => (subclass? :: <boolean>)
  let buck = c2.bucket;
  c1.row[buck] == c2.row[buck];
end function fast-subclass?;

// Compute the closet poles, but in a different fashion, so
// we can check correctness.
//
define function single-closet-pole-check
    (pole-table :: <int-vector>,
     pole-cclass-ids :: <stretchy-int-vector>,
     id :: <integer>)
 => (closest :: false-or(<integer>))
  let super-ids = id.superclass-ids;
  let pole-ids = reverse(remove-duplicates(map(method (id) pole-table[id] end,
					       super-ids)));
  if (pole-ids.size == 1)
    pole-ids.first;
  else
    let pole-cclasses = map-as(<vector>,
			       method (pole)
				 pole-cclass-ids[pole].id-cclass;
			       end,
			       pole-ids);
    block (return)
      for (candidate in pole-cclasses)
	block (fail)
	  for (cclass in pole-cclasses)
	    let (sub?, precise?) = csubtype?(candidate, cclass);
	    unless (precise?)
	      error("Can't compute csubtype???");
	    end unless;
	    unless (sub?)
	      fail();
	    end unless;
	  end for;
	  return(candidate.cclass-id);
	end block;
      end for;
      #f;
    end block;
  end if;
end function single-closet-pole-check;

				     
//=========================================================================
//  describe-argument-array
//=========================================================================
//  Describe an argument array in gruesome detail.

define function describe-argument-array
    (out :: <stream>, arginfo :: <argument-info>)
 => ()
  
  format(out, "  Argument %d of %s\n",
	 arginfo.argument-position,
	 arginfo.argument-generic-defn.defn-name);

  if (arginfo.argument-hairy?)
    format(out, "    Argument is too hairy to optimize.\n");
  else

    // Print some summary statistics.
    format(out, "    Specializers:  %3d\n",
	   arginfo.argument-specializer-cclasses.size);
    format(out, "    Other i-poles: %3d\n",
	   arginfo.argument-artificial-i-poles);
    format(out, "    Total i-poles: %3d\n\n",
	   arginfo.argument-total-i-poles);

    // Print our specializers.
    for (specializer in arginfo.argument-specializer-cclasses)
      format(out, "    %s\n", specializer);
    end for;
    format(out, "\n");

    // Dump the argument array itself.
    dump-argument-array(out, arginfo);
  end if;
end function describe-argument-array;

define function dump-argument-array
    (out :: <stream>, arginfo :: <argument-info>)
 => ()
  let array = arginfo.argument-array;
  let pole-types = arginfo.argument-pole-type-array;

  // Most classes are associated with the same pole as <object>.
  // As we print out the argument array, we compress runs of these
  // classes into a single line.
  let default-pole = array[0];
  let skipped-classes :: <integer> = 0;
  local method flush-skipped-classes () => ()
	  if (skipped-classes > 0)
	    format(out, "          ((( %d classes under default pole )))\n",
		   skipped-classes);
	    skipped-classes := 0;
	  end if;
	end method;

  // Print an entry for <object>.
  format(out, "    %s %3d <object>\n", pole-types[0], array[0]);

  // Print an entry for each of the other classes.
  for (id from 1 below cclass-count())
    let pole = array[id];
    if (pole == default-pole)
      skipped-classes := skipped-classes + 1;
    else
      flush-skipped-classes();
      format(out, "    %s %3d %s :",
	     pole-types[id], pole, id.id-cclass.cclass-name);
      for (super-id in id.superclass-ids)
	let super-pole = array[super-id];
	format(out, " %s(%d%s)",
	       super-id.id-cclass.cclass-name,
	       super-pole,
	       if (super-pole == default-pole) "*" else "" end);
      end for;
      format(out, "\n");
    end if;
  end for;
  flush-skipped-classes();
end function dump-argument-array;


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
//  topologically-sorted-cclasses
//=========================================================================
//  Return all the known cclass objects in topologically sorted order.
//  This order must conform to several constraints:
//
//    * Each class MUST appear before all of its subclasses.
//      This is used to optimize computation of the set of closest poles,
//      as per Dujardin, et al.
//    * Related classes SHOULD be grouped together.
//      This helps to arrange the argument arrays in a fashion convenient
//      for compression. The exact definitions of "related" and
//      "together" are subject to interpretation and refinement.

define constant $UNVISITED_MARK = 0;
define constant $VISITED_MARK = 1;

define function topologically-sorted-cclasses ()
 => (cclasses :: <cclass-vector>)

  // Mark each of the cclasses as unvisited.
  let cclasses :: <stretchy-object-vector> = all-cclass-objects();
  for (cclass in cclasses)
    cclass.cclass-mark := $UNVISITED_MARK;
  end for;

  // Get some useful values.
  let cclass-count = cclasses.size;
  let object-cclass = dylan-value(#"<object>");

  // Perform a post-order traversal of the classes.
  let postorder = make(<stretchy-object-vector>);
  local method visit (cclass :: <cclass>) => ()
	  cclass.cclass-mark := $VISITED_MARK;
	  for (subclass in cclass.direct-subclasses)
	    if (subclass.cclass-mark == $UNVISITED_MARK)
	      visit(subclass);
	    end if;
	  end for;
	  add!(postorder, cclass);
	end method visit;
  visit(object-cclass);

  // Reverse the order of the list and copy in into a high-performance
  // vector class.
  let sorted = make(<cclass-vector>, size: cclass-count,
		    fill: dylan-value(#"<object>"));
  for (i from 0 below cclass-count)
    sorted[i] := postorder[cclass-count - 1 - i];
  end for;
  sorted;
end function topologically-sorted-cclasses;


//=========================================================================
//  Class IDs & Superclass IDs
//=========================================================================
//  To build the argument arrays, we'll need to be able to map each cclass
//  to a unique integer ID, and a given ID to the IDs of its superclasses.

define variable *cclass-count* :: <integer> = 0;
define variable *cclass-to-id-map* :: <object-table> = make(<object-table>);
define variable *id-to-cclass-map* :: false-or(<cclass-vector>) = #f;
define variable *superclass-ids* :: <int-vector-vector> =
  make(<int-vector-vector>, size: 0);

define inline function cclass-count () => (count :: <integer>)
  *cclass-count*;
end function;

define function cclass-id (cclass :: <cclass>) => (id :: <integer>)
  *cclass-to-id-map*[cclass];
end function;

define function id-cclass (id :: <integer>) => (cclass :: <cclass>)
  *id-to-cclass-map*[id];
end function;

define inline function superclass-ids (cclass-id :: <integer>)
 => (ids :: <int-vector>)
  *superclass-ids*[cclass-id];
end function;

define function compute-cclass-information () => ()
  
  // Assign sequential unique IDs to every class.
  let sorted = topologically-sorted-cclasses();
  *id-to-cclass-map* := sorted;
  *cclass-count* := sorted.size;
  *cclass-to-id-map* := make(<object-table>);
  for (i from 0 below *cclass-count*)
    *cclass-to-id-map*[sorted[i]] := i;
  end for;

  // Build a vector containing superclass IDs.
  let cclasses :: <stretchy-vector> = all-cclass-objects();
  *superclass-ids* := make(<int-vector-vector>, size: cclasses.size);
  for (cclass :: <cclass> in cclasses)
    let id = *cclass-to-id-map*[cclass];
    let supers = cclass.direct-superclasses;
    let ids = make(<int-vector>, size: supers.size);
    for (super in supers,
	 i from 0)
      ids[i] := *cclass-to-id-map*[super];
    end for;
    *superclass-ids*[id] := ids;
  end for;
  
end function compute-cclass-information;


//=========================================================================
//  Computing Table Sizes
//=========================================================================
//  

/*
define function compute-argument-array-size
    (arginfo :: <argument-info>,
     chunk-size :: <integer>)
 => (sz :: <integer>)

  let array = arginfo.argument-array;
  let whole-chunks = array.size / chunk-size;
  let partial-chunk? = (array.size % chunk-size ~= 0);
end function;
*/


//=========================================================================
//  Main Program
//=========================================================================

define function analyze-generics () => ()
  format(*debug-output*, "Analyzing <cclass> hierarchy.\n");
  compute-cclass-information();

  let collection-cclass = dylan-value(#"<collection>");
  let sequence-cclass = dylan-value(#"<sequence>");
  format(*debug-output*, "<sequence> %s a subclass of <collection>\n",
	 if (fast-subclass?(sequence-cclass, collection-cclass))
	   "is"
	 else
	   "is not"
	 end if);
  format(*debug-output*, "<collection> %s a subclass of <sequence>\n",
	 if (fast-subclass?(collection-cclass, sequence-cclass))
	   "is"
	 else
	   "is not"
	 end if);

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

      format(analysis, "\n");
      for (arg in args)

	describe-argument-array(analysis, arg);
	format(analysis, "\n");

	/*
	unless (arg.argument-hairy?)
	  format(analysis, "  %d:", arg.argument-position);
	  //for (specializer in arg.argument-specializer-cclasses)
	  //  format(analysis, " %s", specializer)
	  //end for;
	  for (i-pole in arg.argument-array)
	    format(analysis, " %d", i-pole);
	  end for;
	  format(analysis, "\n");
	end unless;
	*/

      end for;
    end for;
    
    format(analysis, "Found %d argument positions, %d methods.\n",
	   total-arg-positions, total-methods);
    
  cleanup
    close(analysis);
  end block;
end function analyze-generics;
