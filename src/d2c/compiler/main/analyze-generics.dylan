Module:   analyze-generics
Author:   Eric Kidd <eric.kidd@pobox.com>
Synopsis: Research code for improving generic function dispatch.
          Heavily based on paper by Dujardin, et al.

// This code refers to argument arrays and pole tables interchangeably...


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

  // Information about each argument position.
  slot %generic-dispatching-arguments :: false-or(<stretchy-vector>) = #f;
end class <generic-info>;

// Compute and cache the dispatching arguments of this function.
define function generic-dispatching-arguments
    (gf :: <generic-info>) => (args :: <stretchy-vector>)
  unless (gf.%generic-dispatching-arguments)
    gf.%generic-dispatching-arguments :=
      choose(argument-non-hairy-and-active?,
	     gf.generic-arguments);
  end unless;
  gf.%generic-dispatching-arguments;
end function generic-dispatching-arguments;

// Does this function use single dispatch or multiple dispatch?
// We consider hairy arguments to be non-dispatching in this case.
define function gf-dispatch-type
    (gf :: <generic-info>) => (type :: <symbol>)
  select (size(gf.generic-dispatching-arguments))
    0 => #"none";
    1 => #"single";
    otherwise => #"multiple";
  end select;
end function gf-dispatch-type;

// Estimate the number of entries in our dispatch matrix.
// We ignore dimensions corresponding to hairy arguments.
define function gf-dispatch-matrix-entry-count
    (gf :: <generic-info>) => (entries :: <integer>)
  select (gf.gf-dispatch-type by \==)
    #"single" =>
      0;
    #"multiple" =>
      let entries = 1;
      for (arg in gf.generic-dispatching-arguments)
	entries := entries * arg.argument-total-i-poles;
      end for;
      entries;
    otherwise =>
      error("Can't compute matrix size for non-dispatching function");
  end select;
end function gf-dispatch-matrix-entry-count;

// How many bytes are needed to specify an offset into our pole table?
define function gf-argument-array-entry-width
    (gf :: <generic-info>) => (width :: <integer>)
  let dispatch = gf.gf-dispatch-type;
  let entries =
    select (dispatch by \==)
      #"single" =>
	// Optimization: Omit dispatch matrix, and store method numbers
	// directly in argument array. Increment number of potential
	// entries by two so we can support no-applicable-method error
	// (and ambiguous-method-error, which probably isn't possible here?).
	// See DAS98.
	let dispatching-arg = gf.generic-dispatching-arguments[0];
	dispatching-arg.argument-total-i-poles + 2;
      #"multiple" =>
	gf.gf-dispatch-matrix-entry-count;
      otherwise =>
	error("Can't compute entry width for non-dispatching function");
    end select;
  case
    (0 < entries & entries <= 256)     => 1;
    (256 < entries & entries <= 65536) => 2;
    otherwise => error("Illegal and unexpected pole count");
  end case;
end function gf-argument-array-entry-width;

// Calculate the number of entries in the uncompressed arrays.
define function gf-uncompressed-pole-table-entry-count
    (gf :: <generic-info>) => (entries :: <integer>)
  gf.generic-dispatching-arguments.size * cclass-count();
end function gf-uncompressed-pole-table-entry-count;

// Figure out how big all our pole tables will be.
define function gf-uncompressed-pole-table-size
    (gf :: <generic-info>) => (size :: <integer>)
  gf.gf-argument-array-entry-width * gf.gf-uncompressed-pole-table-entry-count;
end function gf-uncompressed-pole-table-size;

// Calculate size of span-compressed pole tables.
define function gf-span-compressed-pole-table-size
    (gf :: <generic-info>) => (size :: <integer>)
  let entries = 0;
  for (arg in gf.generic-dispatching-arguments)
    entries := entries + arg.argument-array-span-size;
  end for;
  entries * gf.gf-argument-array-entry-width;
end function gf-span-compressed-pole-table-size;

// Calculate size of chunk-compressed pole tables.
define function gf-chunk-compressed-pole-table-size
    (gf :: <generic-info>) => (size :: <integer>)
  let dispatching = gf.generic-dispatching-arguments;
  let chunks = 0;
  for (arg in dispatching)
    chunks := chunks + arg.argument-array-chunks-required;
  end for;
  let unique-chunk-size =
    chunks * $chunk-size * gf.gf-argument-array-entry-width;
  let first-tier-size =
    ceiling/(cclass-count(), $chunk-size) * $chunk-id-width * dispatching.size;
  unique-chunk-size + first-tier-size;
end function gf-chunk-compressed-pole-table-size;


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

  //-----------------------------------------------------------------------
  // Compression statistics.
  //-----------------------------------------------------------------------
  // (Not defined for hairy arguments.)

  // Compression using a baseline technique suggested by Professor Cormen:
  // Remove "useless" entries from the start and end of the array. This
  // slot contains the size of the span remaining in the middle.
  slot argument-array-span-size :: <integer>;

  // Compression using fixed-size chunks and a two-tier argument array.
  // This slot contains the number of chunks required.
  slot argument-array-chunks-required :: <integer>;

  // Conflict set computed for coloring compression.
  slot argument-conflict-set :: <table>;
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
	let closest = single-closest-pole(pole-table, pole-cclass-ids, supers);

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

    // Compute statistics for several different kinds of compression.
    compute-compression-statistics(arginfo);
  end if;
end method initialize;

// Can we analyze this argument, and is it interesting?
define function argument-non-hairy-and-active?
  (arginfo :: <argument-info>) => (non-hairy-and-active? :: <boolean>)
  ~arginfo.argument-hairy? & arginfo.argument-may-affect-dispatch?;
end function argument-non-hairy-and-active?;

// Find our whether there's a real method defined on <object>, or just
// a fake error selector.
define function argument-has-selector-on-object?
  (arginfo :: <argument-info>) => (has-selector? :: <boolean>)
  arginfo.argument-pole-type-array[0] ~= "E";
end function argument-has-selector-on-object?;


//=========================================================================
//  compute-compression-statistics
//=========================================================================
//  These method should be called for all non-hairy generic functions.

// Caculate each of the different kinds of statistics we'll need for
// each argument array.
define function compute-compression-statistics
    (arginfo :: <argument-info>) => ()
  compute-span-compressed-size(arginfo);
  compute-chunk-compressed-size(arginfo);
end function;

// Professor Cormen suggested that "useful" dispatch entries tended to
// clump together somewhere in the middle of an array. (In this context,
// we define "useful" to mean "having a different i-pole number than
// the class <object>".) We perform simple compression by discarding these
// entries.
define function compute-span-compressed-size
    (arginfo :: <argument-info>) => ()

  // Get the necessary goodies to perform our iteration.
  let array = arginfo.argument-array;
  let count = array.size;

  // Get the pole ID for <object>.
  let default :: <integer> = array[0];

  // Walk up from the bottom, counting entries to throw away.
  // We assume that we can always throw away the entry for <object>.
  let strip-from-bottom = 1;
  for (i from 1 below count, while: array[i] == default)
    strip-from-bottom := strip-from-bottom + 1;
  end for;

  // Repeat the process, but this time walking downwards.
  // But remember, we can't strip anything from the top if we stripped
  // the whole array from the bottom.
  let strip-from-top = 0;
  if (strip-from-bottom < count)
    for (i from (count - 1) above 0 by -1, while: array[i] == default)
      strip-from-top := strip-from-top + 1;
    end for;
  end if;

  // Calculate the span size in bytes.
  arginfo.argument-array-span-size :=
    (count - strip-from-bottom - strip-from-top);
end function compute-span-compressed-size;

// For now, we only support a single chunk size.
define constant $chunk-size = 32;

// We assume that each entry in our chunk table must be four bytes wide,
// which corresponds to a pointer on a 32-bit architecture. We can, of course,
// narrow these entries by adding *another* level of indirection, but we'd
// prefer to avoid that.
define constant $chunk-id-width = 4;

// Right now, we only eliminate chunks which consist entirely of zeros.
define constant $chunk-default-value = 0;

// Perform "chunked" compression. We break the argument array into
// fixed-size chunks, and discard all chunks in which all entries are
// identical. (In practice, we would replace these with pre-allocated chunks
// from the compiler's runtime library.)
//
// Once this is done, we need to access the entries through a two-tier
// structure. The first tier is indexed by the (class ID div chunk size),
// and the second tier is indexed by the (class ID mod chunk size).
//
// To calculate the size of the resulting structure, we need to calculate
// the total size of all the remaining chunks, and add in the size of the
// first-tier array. We ignore the size of pre-allocated chunks in the
// runtime library; there won't be many of these.
define function compute-chunk-compressed-size
    (arginfo :: <argument-info>)
 => (sz :: <integer>)

  let array = arginfo.argument-array;

  let whole-chunks = floor/(array.size, $chunk-size);
  let partial-chunk? = modulo(array.size, $chunk-size) ~= 0;

  // Figure how out many real chunks we'll need.
  let chunks-needed = 0;
  for (i from 0 below whole-chunks)
    let first-index :: <integer> = i * $chunk-size; 
    //let first-value :: <integer> = array[first-index];
    let interesting? :: <boolean> = #f;
    for (j from 1 below $chunk-size)
      if (array[first-index + j] ~= $chunk-default-value)
	interesting? := #t;
      end if;
    end for;
    if (interesting?)
      chunks-needed := chunks-needed + 1;
    end if;
  end for;

  // Add on an extra chunk if we have any leftovers.
  if (partial-chunk?)
    chunks-needed := chunks-needed + 1;
  end if;

  // Calculate sizes.
  arginfo.argument-array-chunks-required := chunks-needed;
end function compute-chunk-compressed-size;


//=========================================================================
//  compute-coloring-compression-statistics
//=========================================================================
//  Compute how much space we'd save using the graph-coloring algorithm
//  from DAS98.
//
//    - For each arg-info in arg-infos
//      - If arg-info has a non-error selector on object, allocate a full
//        argument array (special case).
//      - Else:
//        - Add each of the terminal classes to the appropriate selector
//          conflict set.
//    - For scs in selector-conflict-sets
//      - Add each selector in the set to all of the other's argument conflict
//        sets.
//    - Allocate an empty list of colors
//    - For each arg-info in arg-infos, sorted by size of argument conflict
//      - For each allocated color
//        - If none of the assigned arg-infos have an argument conflict,
//          assign the arg-info to this color and go to next arg-info.
//      - Allocate a new color.
//
//  Return the number of colors allocated + the number of arg-infos with
//  non-error selectors on object.

define function compute-coloring-compression-statistics
    (gf-infos)
 => (arg-info-count :: <integer>, colors-needed :: <integer>)

  // Collect our argument information vectors, and allocate colors for
  // argument positions with total coverage.
  let arg-infos :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let color-count :: <integer> = 0;
  let arg-info-count :: <integer> = 0;
  for (gf-info in gf-infos)
    for (arg-info in gf-info.generic-arguments)
      case
	(~arg-info.argument-non-hairy-and-active?) =>
	  // We can't analyze this one.
	  #f;
	(arg-info.argument-has-selector-on-object?) =>
	  // We can analyze this one in our sleep.
	  color-count := color-count + 1;
	  arg-info-count := arg-info-count + 1;
	otherwise =>
	  // We need to handle this one the hard way.  *sigh*
	  add!(arg-infos, arg-info);
	  arg-info-count := arg-info-count + 1;
      end case;
    end for;
  end for;

  // Build selector conflict sets.
  let selector-conflict-sets = make(<simple-object-vector>,
				     size: cclass-count());
  for (i from 0 below selector-conflict-sets.size)
    selector-conflict-sets[i] := make(<table>);
  end for;
  for (arg-info in arg-infos)
    for (i from 0 below selector-conflict-sets.size)
      // Only process leaf classes, as described in DAS98.
      if (i.id-cclass.direct-subclasses.size = 0)
	// If this argument/selector combination corresponds to a non-error
	// method, add it to the conflict set.  Since we already filtered
	// out all the entries where pole #0 was a real method, anything
	// pointing to pole #0 must be an error.
	if (arg-info.argument-array[i] ~= 0)
	  selector-conflict-sets[i][arg-info] := arg-info;
	end if;
      end if;
    end for;
  end for;
  
  // Build argument conflict sets.
  for (arg-info in arg-infos)
    arg-info.argument-conflict-set := make(<table>);
  end for;
  for (scs in selector-conflict-sets)
    // Add each argument to the argument conflict set of each of the others.
    for (a1 in scs)
      for (a2 in scs)
	if (a1 ~== a2)
	  a1.argument-conflict-set[a2] := a2;
	end if;
      end for;
    end for;
  end for;

  // Sort the arguments by the size of their conflict sets.  Use a stable
  // sort so the order of our results are defined (by the language) to
  // be deterministic and repeatable.
  arg-infos := sort!(arg-infos,
		     test: method (a, b)
			     (a.argument-conflict-set.size >
				b.argument-conflict-set.size);
			   end method,
		     stable: #t);

  // Allocate colors.
  let colors :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  for (arg-info in arg-infos)
    block (next-arg-info)

      // Try to assign this arg-info to an existing color.
      for (color in colors)
	block (next-color)
	  
	  // See if we're allowed to assign the argument to this color.
	  for (assigned-arg-info in color)
	    // If we have a conflict, move on to the next color.
	    if (key-exists?(assigned-arg-info.argument-conflict-set, arg-info))
	      next-color();
	    end if;
	  end for;
	  
	  // Assign the argument to this color.
	  add!(color, arg-info);
	  next-arg-info();
	end block;
      end for;
	
      // Create a new color for this arg-info.
      let new-color = make(<stretchy-object-vector>);
      color-count := color-count + 1;
      add!(new-color, arg-info);
      add!(colors, new-color);
    end block;
  end for;

  // Return the total number of argument positions we processed, and the
  // number of colors we needed.
  values(arg-info-count, color-count);
end function compute-coloring-compression-statistics;


//=========================================================================
//  Single-closest-pole
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

// These counters divide poles into mutually exclusive categories.
define variable *one-parent-count* :: <integer> = 0;
define variable *identical-poles-count* :: <integer> = 0;
define variable *secondary-i-pole-count* :: <integer> = 0;
define variable *single-closest-pole-count* :: <integer> = 0;

define function single-closest-pole
    (pole-table :: <int-vector>,
     pole-cclass-ids :: <stretchy-int-vector>,
     supers :: <int-vector>)
 => (closest :: false-or(<integer>))
  if (supers.size == 1)
    *one-parent-count* := *one-parent-count* + 1;
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
	*identical-poles-count* := *identical-poles-count* + 1;
	return(first-pole);
      end if;

      // Return #f if our largest i-pole doesn't hide all the rest.
      let candidate = pole-cclass-ids[current-largest].id-cclass;
      for (super in supers)
	let pole-cclass-id = pole-cclass-ids[pole-table[super]];
	unless (fast-subclass?(candidate, pole-cclass-id.id-cclass))
	  *secondary-i-pole-count* := *secondary-i-pole-count* + 1;
	  return(#f);
	end unless;
      end for;

      // Our largest i-pole hid all the rest.
      *single-closest-pole-count* := *single-closest-pole-count* + 1;
      current-largest;
    end block;
  end if;
end function single-closest-pole;

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
//  describe-generic
//=========================================================================
//  Describe a complete generic function in gruesome detail.
//  TODO - Update to dump new statistics, too.

define function describe-generic
    (analysis :: <stream>, gf-info :: <generic-info>)
 => ()

  let gf = gf-info.generic-defn;
  let args = gf-info.generic-arguments;

  let active-arg-position-count =
    if (any?(argument-hairy?, args)) 
      -1; // Magic value for our output.
    else
      size(choose(argument-may-affect-dispatch?, args));
    end if;

  // Summarize the generic function.
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

  // Print some descriptive data about each argument.
  format(analysis, "\n");
  for (arg in args)
    describe-argument-array(analysis, arg);
    format(analysis, "\n");
  end for;
end function describe-generic;


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
//
//  Topological sort algorithm from CLR...

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
//  Main Program
//=========================================================================

define function analyze-generics () => ()
  format(*debug-output*, "Analyzing <cclass> hierarchy.\n");
  compute-cclass-information();

  // Some smoke-test code, just to make sure our primitives work.
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

    // Perform our basic analysis, build our tables, and "compress" them.
    let all-gf-info = map(compute-generic-info, real-generics);

    // Discard generic functions which have no analyzable arguments.
    let all-analyzable-gf-info =
      choose(method (gf)
	       gf.gf-dispatch-type ~== #"none"
	     end,
	     all-gf-info);
    
    format(*debug-output*, "Dumping analysis file.\n");
    format(analysis,
	   "Found %d classes, %d real generics (%d analyzable, %d total).\n\n",
	   classes.size, real-generics.size, all-analyzable-gf-info.size,
	   generics.size);

    // Describe all our generic functions. This is mostly for debugging
    // and inspirational purposes. We also count hairy arguments now, because
    // we may not see them later.
    let total-arg-positions = 0;
    let total-hairy-arg-positions = 0;
    let total-methods = 0;
    for (gf-info in all-gf-info)
      describe-generic(analysis, gf-info);

      // Collect some statistics.
      let gf = gf-info.generic-defn;
      let arg-position-count = gf.function-defn-signature.specializers.size;
      total-arg-positions := total-arg-positions + arg-position-count;
      let method-count = gf.generic-defn-methods.size;
      total-methods := total-methods + method-count;
      for (arg in gf-info.generic-arguments)
	if (arg.argument-hairy?)
	  total-hairy-arg-positions := total-hairy-arg-positions + 1;
	end if;
      end for;
    end for;

    // Keep running totals of compression information.
    // sd = 'single dispatch', md = 'multiple dispatch'.
    let sd-uncompressed-entries = 0;
    let md-uncompressed-entries = 0;
    let sd-uncompressed-size = 0;
    let md-uncompressed-size = 0;
    let sd-span-compressed-size = 0;
    let md-span-compressed-size = 0;
    let sd-chunk-compressed-size = 0;
    let md-chunk-compressed-size = 0;

    // Collect our statistics.
    let total-active-arg-positions = 0;
    let total-estimated-matrix-entries = 0;
    let total-sd-gfs = 0;
    let total-md-gfs = 0;
    for (gf-info in all-analyzable-gf-info)

      let args = gf-info.generic-arguments;

      total-active-arg-positions :=
	(total-active-arg-positions +
	   gf-info.generic-dispatching-arguments.size);

      // Collect our compression statistics based on whether we're
      // a single or multiple dispatch function.
      select (gf-info.gf-dispatch-type by \==)
	#"single" =>
	  total-sd-gfs := total-sd-gfs + 1;
	  sd-uncompressed-entries :=
	    (sd-uncompressed-entries +
	       gf-info.gf-uncompressed-pole-table-entry-count);
	  sd-uncompressed-size :=
	    (sd-uncompressed-size +
	       gf-info.gf-uncompressed-pole-table-size);
	  sd-span-compressed-size :=
	    (sd-span-compressed-size +
	       gf-info.gf-span-compressed-pole-table-size);
	  sd-chunk-compressed-size :=
	    (sd-chunk-compressed-size +
	       gf-info.gf-chunk-compressed-pole-table-size);
	#"multiple" =>
	  total-md-gfs := total-md-gfs + 1;
	  md-uncompressed-entries :=
	    (md-uncompressed-entries +
	       gf-info.gf-uncompressed-pole-table-entry-count);
	  md-uncompressed-size :=
	    (md-uncompressed-size +
	       gf-info.gf-uncompressed-pole-table-size);
	  md-span-compressed-size :=
	    (md-span-compressed-size +
	       gf-info.gf-span-compressed-pole-table-size);
	  md-chunk-compressed-size :=
	    (md-chunk-compressed-size +
	       gf-info.gf-chunk-compressed-pole-table-size);
	otherwise =>
	  error("panic!");
      end select;

      // Update our total number of estimated dispatch matrix entries.
      // This does the right thing for single-dispatch functions.
      total-estimated-matrix-entries :=
	(total-estimated-matrix-entries + 
	   gf-info.gf-dispatch-matrix-entry-count);
    end for;

    format(analysis,
	   "%d argument positions (%d active, %d hairy), %d methods.\n",
	   total-arg-positions, total-active-arg-positions,
	   total-hairy-arg-positions, total-methods);
    format(analysis, "Estimated dispatch matrix entires: %d\n\n",
	   total-estimated-matrix-entries);

    dump-compression-analysis
      (analysis, "TOTAL",
       total-sd-gfs + total-md-gfs,
       sd-uncompressed-entries + md-uncompressed-entries,
       sd-uncompressed-size + md-uncompressed-size,
       sd-span-compressed-size + md-span-compressed-size,
       sd-chunk-compressed-size + md-chunk-compressed-size);

    dump-compression-analysis
      (analysis, "SINGLE DISPATCH",
       total-sd-gfs,
       sd-uncompressed-entries,
       sd-uncompressed-size,
       sd-span-compressed-size,
       sd-chunk-compressed-size);

    dump-compression-analysis
      (analysis, "MULTIPLE DISPATCH",
       total-md-gfs,
       md-uncompressed-entries,
       md-uncompressed-size,
       md-span-compressed-size,
       md-chunk-compressed-size);

    format(analysis, "One parent: %d\n", *one-parent-count*);
    format(analysis, "Identical poles: %d\n", *identical-poles-count*);
    format(analysis, "Secondary i-poles: %d\n", *secondary-i-pole-count*);
    format(analysis, "Single closest: %d\n\n", *single-closest-pole-count*);

    // Calculate sizes using coloring compression.
    format(*debug-output*, "Analyzing coloring compression.\n");
    local method has-width? (gf, width) => (hw? :: <boolean>)
	    gf.gf-argument-array-entry-width == width;
	  end method has-width?;
    let 8-bit-gf-info = choose(rcurry(has-width?, 1), all-analyzable-gf-info);
    let 16-bit-gf-info = choose(rcurry(has-width?, 2), all-analyzable-gf-info);
    let (total-8-bit-arrays, colored-8-bit-arrays) =
      compute-coloring-compression-statistics(8-bit-gf-info);
    let (total-16-bit-arrays, colored-16-bit-arrays) =
      compute-coloring-compression-statistics(16-bit-gf-info);
    let uncompressed-size = ((total-8-bit-arrays + total-16-bit-arrays * 2)
			       * cclass-count());
    let compressed-size = ((colored-8-bit-arrays + colored-16-bit-arrays * 2)
			     * cclass-count());
    format(analysis, "  Coloring: %dKb => %dKb, saving %d%%\n",
	   round/(uncompressed-size, 1024),
	   round/(compressed-size, 1024),
	   100 - round/(100 * compressed-size, uncompressed-size));

  cleanup
    close(analysis);
  end block;
end function analyze-generics;

define function dump-compression-analysis
    (analysis, label, gf-count, uncompressed-entries, uncompressed-size,
     span-compressed-size, chunk-compressed-size)
 => ()
  format(analysis, "Compression analysis: %s\n\n", label);
  format(analysis, "  Generic functions: %d\n", gf-count);
  format(analysis, "  Argument array size: %d entries, %dKb uncompressed.\n",
	 uncompressed-entries,
	 round/(uncompressed-size, 1024));
  format(analysis, "  Using span compression: %dKb, saving %d%%\n",
	 round/(span-compressed-size, 1024),
	 100 - round/(100 * span-compressed-size, uncompressed-size));
  format(analysis, "  Using chunked compression: %dKb, saving %d%%\n\n",
	 round/(chunk-compressed-size, 1024),
	 100 - round/(100 * chunk-compressed-size, uncompressed-size));
end function dump-compression-analysis;
