module: classes
rcs-header: $Header: /scm/cvs/src/d2c/compiler/base/cclass.dylan,v 1.3.4.1 1998/09/23 01:25:40 anoncvs Exp $
copyright: Copyright (c) 1995  Carnegie Mellon University
	   All rights reserved.

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
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

// <cclass> (<ctype>, <eql-ct-value>)
//   <defined-cclass>
//   <limited-cclass>
//
// <slot-info> (<eql-ct-value>, <identity-preserving-mixin>)
//   <instance-slot-info> 
//     <vector-slot-info> 
//   <class-slot-info>
//   <each-subclass-slot-info>
//   <virtual-slot-info>
//
// <override-info> (<eql-ct-value>, <identity-preserving-mixin>)
//
// <class-precedence-description>
//
// <layout-table>
//
// <position-table>
//
// <pt-entry>
//
// <direct-instance-ctype> (<limited-ctype>, <ct-value>)
//
// <subclass-ctype> (<limited-ctype>, <ct-value>,
//                   <identity-preserving-mixin>)
//
// <proxy> (<ct-value>, <identity-preserving-mixin>)


// $All-Classes -- internal.
//
// Holds all the classes allocated.  We can't make any guarantees about 
//
define variable *All-Classes* :: <stretchy-vector> = make(<stretchy-vector>);


// <cclass>
//
// The compile-time representation of classes.
//
define abstract class <cclass> (<ctype>, <eql-ct-value>)
  //
  // The name, for printing purposes.
  slot cclass-name :: <name>, required-init-keyword: name:;

  slot loaded? :: <boolean>,
    init-value: #t, init-keyword: loading:;

  // List of the direct superclasses of this class.
  slot direct-superclasses :: <list>,
    required-init-keyword: direct-superclasses:;

  // Closest primary superclass.
  slot closest-primary-superclass :: <cclass>;

  // True when this class can't and none of its subclasses can be functional.
  // I.e. when the class is concrete and ~functional?, has a writable slot, or
  // one of its superclasses is not-functional?.
  slot not-functional? :: <boolean>,
    init-keyword: not-functional:, init-value: #f;

  // True when class is functional, sealed, abstract, and/or primary.
  slot functional? :: <boolean>, init-keyword: functional:, init-value: #f;
  slot sealed? :: <boolean>, init-keyword: sealed:, init-value: #f;
  slot abstract? :: <boolean>, init-keyword: abstract:, init-value: #f;
  slot primary? :: <boolean>, init-keyword: primary:, init-value: #f;

  // The direct-instance type for direct instances of this class, or #f
  // if we haven't made one yet.
  slot %direct-type :: false-or(<direct-instance-ctype>), init-value: #f;

  // The <subclass-ctype> for subclasses of this class, or #f if we haven't
  // allocated it yet.
  slot subclass-ctype :: false-or(<subclass-ctype>) = #f;

  // class precedence list of all classes inherited, including this class and
  // indirectly inherited classes.  Unbound if not yet computed.
  slot precedence-list :: <list>, init-keyword: precedence-list:;

  // List of the direct subclasses.
  slot direct-subclasses :: <list>, init-value: #();

  // List of all known subclasses (including this class and indirect
  // subclasses).  If sealed, then this is all of 'em.
  slot subclasses :: <list>, init-value: #();

  // The unique id number associated with this class (only if concrete,
  // though.)
  slot unique-id :: false-or(<integer>), init-value: #f,
    init-keyword: unique-id:;
  //
  // The range of ids that cover all the subclasses of this class and
  // only the subclasses of this class, if such a range exists.  That
  // range will exist if this class is never mixed in with any other
  // class.  And if this class is sealed.
  slot subclass-id-range-min :: false-or(<integer>), init-value: #f,
    init-keyword: subclass-id-range-min:;
  slot subclass-id-range-max :: false-or(<integer>), init-value: #f,
    init-keyword: subclass-id-range-max:;
  //
  // The representation of instances of this class or #f if we haven't
  // picked them yet.  Also #f if this class is abstract, because we never
  // pick representations for abstract classes.
  slot direct-speed-representation :: false-or(<representation>),
    init-value: #f, init-keyword: direct-speed-representation:;
  slot direct-space-representation :: false-or(<representation>),
    init-value: #f, init-keyword: direct-space-representation:;
  //
  // A memo of the representation to use for general instances of this class.
  // Used by pick-representation.
  slot general-speed-representation :: false-or(<representation>),
    init-value: #f, init-keyword: general-speed-representation:;
  slot general-space-representation :: false-or(<representation>),
    init-value: #f, init-keyword: general-space-representation:;
  //
  // Vector of <slot-info>s for the slots introduced by this class.
  slot new-slot-infos :: <simple-object-vector> = #[],
    init-keyword: slots:;
  //
  // Vector of all the slots in instances of this class, in no particular
  // order. Filled in when the slot layouts are computed.
  slot all-slot-infos :: <vector> = #[],
    init-keyword: all-slot-infos:;
  //
  // Vector of <override-info>s for the overrides introduced by this class.
  slot override-infos :: <simple-object-vector> = #[],
    init-keyword: overrides:;
  //
  // #t if we've computed the layout, #"computing" if we are working on it,
  // and #f until then.
  slot layout-computed? :: one-of(#t, #"computing", #f) = #f;
  //
  // Layout of the instance slots.  Filled in when the slot layouts are
  // computed.
  slot instance-slots-layout :: <layout-table>,
    init-keyword: instance-slots-layout:;
  //
  // The trailing vector slot, if any.
  slot vector-slot :: false-or(<vector-slot-info>) = #f,
    init-keyword: vector-slot:;
  //
  // The slot allocated in the data-word, if any.
  slot data-word-slot :: false-or(<instance-slot-info>) = #f,
    init-keyword: data-word-slot:;
  //
  // Count of the number of each-subclass slots.
  slot each-subclass-slots-count :: <integer>,
    init-keyword: each-subclass-slots-count:;
  //
  // Used by the heap builder.
  slot class-heap-fields :: false-or(<simple-object-vector>),
    init-value: #f;
end class;

define sealed domain make (singleton(<cclass>));
define sealed domain initialize (<cclass>);

// initialize -- gf method.
//
define method initialize
    (class :: <cclass>, #next next-method,
     #key loading: loading? = #t, precedence-list, slots, overrides)
    => ();
  next-method();
  
  // Add this class to *All-Classes*.
  add!(*All-Classes*, class);

  // Add us to all our direct superclasses direct-subclass lists.
  let supers = class.direct-superclasses;
  for (super in supers)
    add-class-to-direct-superclass(class, super);
  end for;

  // Compute the cpl if it wasn't already handed to us.
  let cpl = (precedence-list
	       | (class.precedence-list := compute-cpl(class, supers)));

  // Add us to all our superclasses subclass lists.
  for (super in cpl)
    add-class-to-general-superclass(class, super);
  end;

  // Find the closest primary superclass.  Note: we don't have to do
  // any error checking, because that is done for us in
  // defclass.dylan.  If we are loading this class, the loader will
  // set all this up for us, so we can skip it here.
  unless (loading?)
    set-closest-primary-superclass(class);
    
    // Fill in introduced-by for the slots and overrides.
    set-introduced-by-and-overrides(class, slots, overrides);
  end;
end;


// add-class-to-direct-superclass -- internal.
//
// Add `class' to the slot `super.direct-subclasses', requesting a
// backpatch if necessary.
//
define inline function add-class-to-direct-superclass
    (class :: <cclass>, super :: type-union(<cclass>, <forward-ref>))
 => ();
  if (super.obj-resolved?)
    super.direct-subclasses := pair(class, super.direct-subclasses);
  else
    request-backpatch
      (super,
       method (actual)
	 actual.direct-subclasses := pair(class, actual.direct-subclasses);
       end method);
  end if;
end function;


// add-class-to-general-superclass -- internal.
//
// Add `class' to the slot `super.subclasses', requesting a backpatch
// if necessary.
//
define inline function add-class-to-general-superclass
    (class :: <cclass>, super :: type-union(<cclass>, <forward-ref>))
 => ();
  if (super.obj-resolved?)
    super.subclasses := pair(class, super.subclasses);
  else
    request-backpatch(super,
		      method (actual)
			actual.subclasses := pair(class, actual.subclasses);
		      end);
  end;
end function;


// set-closest-primary-superclass -- internal.
//
define inline function set-closest-primary-superclass
    (class :: <cclass>)
 => ();
  if (class.primary?)
    class.closest-primary-superclass := class;
  else
    let closest = #f;
    for (super in class.direct-superclasses)
      let primary-super = super.closest-primary-superclass;
      if (~closest | csubtype?(primary-super, closest))
	closest := primary-super;
      end;
    end;
    class.closest-primary-superclass := closest;
  end;
end function;


// set-introduced-by-and-overrides -- internal.
//
define inline function set-introduced-by-and-overrides
    (class :: <cclass>, slots :: <collection>, overrides :: <collection>)
 => ();
  for (slot in slots)
    slot.slot-introduced-by := class;
  end;
  for (override in overrides)
    override.override-introduced-by := class;
  end;
end function;


// print-object {<cclass>}
//    -- method on exported GF.
//
define method print-object (cclass :: <cclass>, stream :: <stream>) => ();
  pprint-fields(cclass, stream, name: cclass.cclass-name);
end;


// print-message {<cclass>}
//    -- method on exported GF.
//
define method print-message (cclass :: <cclass>, stream :: <stream>) => ();
  write(stream, as(<string>, cclass.cclass-name.name-symbol));
end;


// <slot-allocation>  -- exported.
//
define constant <slot-allocation>
  = one-of(#"instance", #"class", #"each-subclass", #"virtual");


// <slot-info>  -- exported.
//
define abstract class <slot-info> (<eql-ct-value>, <identity-preserving-mixin>)
  //
  // The cclass that introduces this slot.  Not required, because we have to
  // make the regular slots before we can make the cclass that defines them.
  slot slot-introduced-by :: <cclass>,
    init-keyword: introduced-by:;
  //
  // The type we've decided to use for this slot.  Either the declared type,
  // or <object> if we can't figure out what the declared type is at
  // compile-time.
  slot slot-type :: <ctype>, init-keyword: type:;
  //
  // The getter generic function definition.  Used for slot identity.  If #f,
  // that means that the slot is an auxiliary slot hung off some other slot,
  // and therefore doesn't need additional identity information.
  slot slot-getter :: false-or(<variable>),
    required-init-keyword: getter:;
  //
  // True if the slot is read-only (i.e. no setter), False otherwise.
  slot slot-read-only? :: <boolean>,
    init-value: #f, init-keyword: read-only:;
  //
  // The initial value.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot slot-init-value :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot slot-init-function :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-function:;
  //
  // The init-keyword, or #f if there isn't one.
  slot slot-init-keyword :: false-or(<symbol>),
    init-value: #f, init-keyword: init-keyword:;
  //
  // True if the init-keyword is required, False if not.
  slot slot-init-keyword-required? :: <boolean>,
    init-value: #f, init-keyword: init-keyword-required:;
  //
  // List of all the overrides for this slot.  Filled in when the overrides
  // for some class are processed.  Each override is a <variable>.
  slot slot-overrides :: <list>, init-value: #();
end;

define sealed domain make (singleton(<slot-info>));
define sealed domain initialize (<slot-info>);


// print-message {<slot-info>}
//    -- method on exported GF.
//
define method print-message
    (lit :: <slot-info>, stream :: <stream>) => ();
  format(stream, "{<slot-descriptor> for %s introduced by %s}",
	 if (lit.slot-getter)
	   lit.slot-getter.variable-name;
	 else
	   "???";
	 end if,
	 lit.slot-introduced-by);
end;


define method make (class == <slot-info>, #rest keys, #key allocation)
    => res :: <slot-info>;
  apply(make,
	select (allocation)
	  #"instance" => <instance-slot-info>;
	  #"class" => <class-slot-info>;
	  #"each-subclass" => <each-subclass-slot-info>;
	  #"virtual" => <virtual-slot-info>;
	end,
	keys);
end;


// initialize -- gf method.
//
// This method's only purpose is to make allocation be an acceptable keyword
// for the various subclasses of <slot-info> so we don't have to remove it from
// the set of keys passed in when we make the particular kind of slot-info
// above.
// 
define method initialize
    (info :: <slot-info>, #next next-method, #key allocation) => ();
  next-method();
end;


// <instance-slot-info>  -- exported.
//
define class <instance-slot-info> (<slot-info>)
  constant slot slot-positions :: <position-table> = make(<position-table>);
  slot slot-representation :: false-or(<representation>) = #f,
    init-keyword: slot-representation:;
  slot slot-initialized?-slot :: false-or(<instance-slot-info>) = #f,
    init-keyword: slot-initialized?-slot:;
end;

define sealed domain make (singleton(<instance-slot-info>));


// <vector-slot-info>  -- exported.
//
define class <vector-slot-info> (<instance-slot-info>)
  slot slot-size-slot :: <instance-slot-info>,
    init-keyword: size-slot:;
end;

define sealed domain make (singleton(<vector-slot-info>));


// <class-slot-info>  -- exported.
//
define class <class-slot-info> (<slot-info>)
end;

define sealed domain make (singleton(<class-slot-info>));
define sealed domain initialize (<class-slot-info>);


// <each-subclass-slot-info>  -- exported.
//
define class <each-subclass-slot-info> (<slot-info>)
  constant slot slot-positions :: <position-table> = make(<position-table>);
end;

define sealed domain make (singleton(<each-subclass-slot-info>));
define sealed domain initialize (<each-subclass-slot-info>);


// <virtual-slot-info>  -- exported.
//
define class <virtual-slot-info> (<slot-info>)
end;

define sealed domain make (singleton(<virtual-slot-info>));
define sealed domain initialize (<virtual-slot-info>);


// <override-info>  -- exported.
//
define class <override-info> (<eql-ct-value>, <identity-preserving-mixin>)
  //
  // The cclass that introduces this override.  Filled in when the cclass that
  // introduces this override is initialized.
  slot override-introduced-by :: <cclass>,
    init-keyword: introduced-by:;
  //
  // The getter generic function definition.  Used for slot identity.
  slot override-getter :: <variable>,
    required-init-keyword: getter:;
  //
  // The slot-info this override is overriding.  Filled in when overrides are
  // inherited.
  slot override-slot :: <slot-info>;
  //
  // The initial value.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot override-init-value :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-value:;
  //
  // The init-function.  A <ct-value> if we can figure one out, #t if there is
  // one but we can't tell what it is, and #f if there isn't one.
  slot override-init-function :: type-union(<ct-value>, <boolean>),
    init-value: #f, init-keyword: init-function:;
end;

define sealed domain make (singleton(<override-info>));
define sealed domain initialize (<override-info>);


// print-message {<override-info>}
//    -- method on exported GF.
//
define method print-message
    (override :: <override-info>, stream :: <stream>) => ();
  format(stream, "{<override-descriptor> for %s at %s}",
	 override.override-getter.variable-name,
	 override.override-introduced-by);
end method print-message;


// ct-value-cclass {<cclass>}  -- method on exported GF.
//
define method ct-value-cclass (object :: <cclass>) => res :: <cclass>;
  dylan-value(#"<class>");
end;


// ct-value-cclass {<slot-info>}  -- method on exported GF.
//
define method ct-value-cclass (object :: <slot-info>) => res :: <cclass>;
  dylan-value(#"<slot-descriptor>");
end;


// ct-value-cclass {<override-info>}  -- method on exported GF.
//
define method ct-value-cclass (object :: <override-info>) => res :: <cclass>;
  dylan-value(#"<override-descriptor>");
end;

// Ctype operations.
// ================

// csubtype-dispatch{<cclass>,<cclass>}
//    -- method on exported GF.
//
// Check the class precedence list.
// 
define method csubtype-dispatch (type1 :: <cclass>, type2 :: <cclass>)
    => result :: <boolean>;
  member?(type2, type1.precedence-list);
end method;

// csubtype-dispatch{<limited-ctype>,<cclass>}
//    -- method on exported GF.
//
// A limited type is a subtype of a class iff the base class is a subtype
// of that class.
// 
define method csubtype-dispatch(type1 :: <limited-ctype>, type2 :: <cclass>)
    => result :: <boolean>;
  csubtype?(type1.base-class, type2);
end method;

// ctype-intersection-dispatch {<cclass>, <cclass>}
//    -- method on exported GF.
//
define method ctype-intersection-dispatch(type1 :: <cclass>, type2 :: <cclass>)
    => (result :: <ctype>, precise :: <boolean>);
  if (type1.sealed?)
    values(reduce(ctype-union, empty-ctype(),
		  choose(rcurry(csubtype?, type2), type1.subclasses)),
	   #t);
  elseif (type2.sealed?)
    values(reduce(ctype-union, empty-ctype(),
		  choose(rcurry(csubtype?, type1), type2.subclasses)),
	   #t);
  else
    let primary1 = type1.closest-primary-superclass;
    let primary2 = type2.closest-primary-superclass;
    if (csubtype?(primary1, primary2) | csubtype?(primary2, primary1))
      // The closest primary superclasses are not inconsistent.  Therefore,
      // someone could make a new subclass that inherits from both.
      values(type1, #f);
    else
      values(empty-ctype(), #t);
    end;
  end;
end method;


// find-direct-classes  {<cclass>}
//    -- method on exported GF.
//
// If the class is sealed, return all of the concrete subclass of it.
// Otherwise, return #f because we can't tell at compile time what all
// the possible direct classes are.
//
define method find-direct-classes (type :: <cclass>)
    => res :: false-or(<list>);
  if (type.sealed?)
    choose(complement(abstract?), type.subclasses);
  else
    #f;
  end;
end method;


// ctype-extent-dispatch  {<cclass>}
//    -- method on exported GF.
//
// If the class is sealed, make a union out of the extents of each possible
// direct class.  Otherwise, just stick with the class.
// 
define method ctype-extent-dispatch (class :: <cclass>)
    => res :: <ctype>;
  if (class.sealed?)
    let result = empty-ctype();
    for (subclass in class.subclasses)
      unless (subclass.abstract?)
	let direct = make(<direct-instance-ctype>, base-class: subclass);
	result := ctype-union(result, direct.ctype-extent);
      end unless;
    end for;
    result;
  else
    class;
  end if;
end method ctype-extent-dispatch;



// Class Precedence List computation.
// =================================

// This class is a temporary data structure used during CPL computation.
define class <class-precedence-description> (<object>)
  //
  // The class this cpd describes the precedence of.
  slot cpd-class :: <cclass>, required-init-keyword: class:;
  //
  // List of cpd's for the direct superclasses.
  slot cpd-supers :: <list>, init-value: #();
  //
  // List of cpd's for classes that have to follow this class.
  slot cpd-after :: <list>, init-value: #();
  //
  // Count of times this cpd appeards in some other cpd's after list.
  slot cpd-count :: <integer>, init-value: 0;
end class;

define sealed domain make (singleton(<class-precedence-description>));
define sealed domain initialize (<class-precedence-description>);

// compute-cpl
//
// Compute the class precedence list.  If `class' has only one direct
// superclass we can simply return the superclass' cpl with `class'
// tacked on front, otherwise we have to call `slow-comput-cpl' to run
// the full algorithm.
//
define inline function compute-cpl
    (cl :: <cclass>, superclasses :: <list>)
 => (class-precedence-list :: <list>);
  case
    superclasses == #() =>
      list(cl);

    superclasses.tail == #() =>
      pair(cl, superclasses.head.precedence-list);

    otherwise =>
      slow-compute-cpl(cl, superclasses);
  end;
end function;

// slow-compute-cpl  -- internal.
//
// Find CPL when there are multiple direct superclasses.  I have
// defined this as inline function since it has only one call site.
//
// Inlined because only one call site.
//
define inline function slow-compute-cpl
    (cl :: <cclass>, superclasses :: <list>)
 => (class-precedence-list :: <list>);
  let cpds = #();
  let class-count = 0;
  local
    // find CPD for a class, making a new one if necessary.
    method find-cpd (cl)
      block (return)
	for (x in cpds)
	  if (x.cpd-class == cl)
	    return(x);
	  end;
	end;
	compute-cpd(cl, cl.direct-superclasses);
      end;
    end method,

    method compute-cpd (cl, supers)
      let cpd = make(<class-precedence-description>, class: cl);
      cpds := pair(cpd, cpds);
      class-count := class-count + 1;
      unless (supers == #())
        let prev-super-cpd = find-cpd(supers.head);
	cpd.cpd-supers := pair(prev-super-cpd, cpd.cpd-supers);
	cpd.cpd-after := pair(prev-super-cpd, cpd.cpd-after);
	prev-super-cpd.cpd-count := prev-super-cpd.cpd-count + 1;
	for (super in supers.tail)
	  let super-cpd = find-cpd(super);
	  cpd.cpd-supers := pair(super-cpd, cpd.cpd-supers);
	  cpd.cpd-after := pair(super-cpd, cpd.cpd-after);
	  prev-super-cpd.cpd-after
	    := pair(super-cpd, prev-super-cpd.cpd-after);
	  super-cpd.cpd-count := super-cpd.cpd-count + 2;
	  prev-super-cpd := super-cpd;
	end;
      end unless;
      cpd;
    end method;
      
  let candidates = list(compute-cpd(cl, superclasses));
  let rcpl = #();

  for (index from 0 below class-count)
    if (candidates == #())
      error("Inconsistent CPL");
    end;

    local
      handle (cpd)
        candidates := remove!(candidates, cpd);
	rcpl := pair(cpd.cpd-class, rcpl);
	for (after in cpd.cpd-after)
	  if (zero?(after.cpd-count := after.cpd-count - 1))
	    candidates := pair(after, candidates);
	  end;
	end;
      end method;

    if (candidates.tail == #())
      handle(candidates.head);
    else
      // There is more than one candidate, so pick one.
      block (tie-breaker)
	for (c in rcpl)
	  let supers = c.direct-superclasses;
	  for (candidate in candidates)
	    if (member?(candidate.cpd-class, supers))
	      handle(candidate);
	      tie-breaker();
	    end if;
	  end for;
	end for;
	error("Can't happen.");
      end block;
    end if;
  end for;

  reverse!(rcpl);
end function slow-compute-cpl;

// Slot inheritance.
// ================

// inherit-slots -- exported.
//
// Populate each class with complete slot information by inhereting whatever
// is necessary.
//
define function inherit-slots () => ();
  //
  // The first thing we do is sort *All-Classes* to guarantee that superclasses
  // preceed their subclasses.
  *All-Classes*
    := sort!(*All-Classes*,
	     test: method (class1 :: <cclass>, class2 :: <cclass>)
		       => res :: <boolean>;
		     class1.precedence-list.size < class2.precedence-list.size;
		   end method);
  //
  // Now propagate slots down to each subclass.
  do(inherit-slots-for, *All-Classes*);
end function inherit-slots;


// inherit-slots-for  -- internal.
//
define function inherit-slots-for (class :: <cclass>) => ();
  let processed = #();
  let supers = class.direct-superclasses;
  if (empty?(supers))
    class.all-slot-infos := make(<stretchy-vector>, size: 0);
  else
    let first-super = supers.first;
    processed := first-super.precedence-list;
    class.all-slot-infos :=
      map-as(<stretchy-vector>, identity, first-super.all-slot-infos);
  end;
  local
    method process (super :: <cclass>)
      unless (member?(super, processed))
	//
	// Mark this super as processed.
	processed := pair(super, processed);
	//
	// Process the super's superclasses.
	do(process, super.direct-superclasses);
	//
	// Inherit the slots.
	for (slot in super.new-slot-infos)
	  add-slot(slot, class);
	end for;
      end unless;
    end method;
  do(process, supers);
  for (slot in class.new-slot-infos)
    reset-slot(slot);
    add-slot(slot, class);
  end;
end function inherit-slots-for;


// reset-slot  -- internal GF.
//
define generic reset-slot (slot :: <slot-info>) => ();


// reset-slot {<slot-info>}  -- method on internal GF.
//
// Clears all slot overrides.
//
define method reset-slot (slot :: <slot-info>) => ();
  slot.slot-overrides := #();
end;

// reset-slot {<slot-info>}  -- method on internal GF.
//
// Clears all slot overrides, reset's the `slot''s representation,
// position and `slot-initialized?' slot.
//
define method reset-slot (slot :: <instance-slot-info>) => ();
  slot.slot-overrides := #();
  slot.slot-representation := #f;
  clear-positions(slot.slot-positions);
  slot.slot-initialized?-slot := #f;
end;


// add-slot
//
// Ensures that `slot''s getter is unique and then adds `slot' to the
// `class''s `all-slot-info'.
//
define function add-slot
    (slot :: <slot-info>, class :: <cclass>) => ();
  //
  // Make sure the slot doesn't clash with some other slot with the same
  // getter.
  if (slot.slot-getter)
    for (other-slot in class.all-slot-infos)
      if (slot.slot-getter == other-slot.slot-getter)
	compiler-fatal-error
	  ("Class %s can't combine two different %s slots, "
	     "one introduced by %s and the other by %s",
	   class, slot.slot-getter.variable-name,
	   slot.slot-introduced-by, other-slot.slot-introduced-by);
      end if;
    end for;
  end if;
  //
  // Add the slot to the all-slot-infos.
  add!(class.all-slot-infos, slot);
end function add-slot;

// Override inheritance.
// ====================

// inherit-overrides  -- exported.
//
// Sets the `slot-overrides' slot in all instances of <slot-info> and
// the `override-slot' in all overrides.
//
define function inherit-overrides ()
  for (cclass in *All-Classes*)
    set-slot-overrides(cclass);
    check-conflicting-overrides(cclass);
  end for;
end function inherit-overrides;


// set-slot-overrides -- internal.
//
// Does the bulk of the work for `inherit-overrides', i.e., actually
// sets the `slot-overrides' slot in all instances of <slot-info> and
// the `override-slot' in all overrides.
//
define inline function set-slot-overrides (cclass :: <cclass>) => ();
  for (override in cclass.override-infos)
    block (next-override)
      for (slot in cclass.all-slot-infos)
	if (override.override-getter == slot.slot-getter)
	  check-correct-slot-allocation-for-override(cclass, slot);
	  slot.slot-overrides := pair(override, slot.slot-overrides);
	  override.override-slot := slot;
	  next-override();
	end if;
      end for;
      compiler-fatal-error
	("Class %s can't override slot %s, because is doesn't "
	   "have that slot.",
	 cclass, override.override-getter.variable-name);
    end block;			// next-override()
  end for;
end function;


// check-correct-slot-allocation-for-override -- internal.
//
// Checks whether `slot' was introduced by superclass of `cclass' and
// whether it has instance or virtual allocation.
//
// Inlined because only one call site.
//
define inline function check-correct-slot-allocation-for-override
    (cclass :: <cclass>, slot :: <slot-info>)
 => ();
  if (slot.slot-introduced-by == cclass)
    compiler-fatal-error
      ("Class %s can't both introduce and override slot %s",
       cclass, slot.slot-getter.variable-name);
  end if;
  if (instance?(slot, <class-slot-info>))
    compiler-fatal-error("Can't override class allocation slots");
  end if;
  if (instance?(slot, <virtual-slot-info>))
    compiler-fatal-error("Can't override virtual slots");
  end if;
end function;


// check-conflicting-overrides -- internal.
//
// Checks for conflicting overrides.
//
// Inlined because only one call site.
//
define inline function check-conflicting-overrides
    (cclass :: <cclass>) => ();
  for (slot in cclass.all-slot-infos)
    let active-overrides = #();
    for (override in slot.slot-overrides)
      if (csubtype?(cclass, override.override-introduced-by))
	//
	// The current override is intruduced by a superclass of
	// `cclass'...
	local
	  method introduced-by-a-csubtype-of-override (other)
	    csubtype?(other.override-introduced-by,
		      override.override-introduced-by);
	  end;
	unless (any?(introduced-by-a-csubtype-of-override,
		     active-overrides))
	  //
	  // ... and the former override is not introduced by a
	  // sublcass of the class that introduced the current
	  // override.
	  //
	  // Set `active-overrides' to the pair consisting of the
	  // current override and
	  // * the former contents if override was not introduced
	  //   by a class which is a subclass of the class that
	  //   introduced the former override, i.e., if the former
	  //   override is not intriduced by a superclass of the
	  //   class that introduced the current override, or to
	  // * #() otherwise.
	  local
	    method override-not-introduced-by-a-csubtype-of(other)
	      ~csubtype?(override.override-introduced-by,
			 other.override-introduced-by);
	    end;
	  active-overrides
	    := pair(override,
		    choose(override-not-introduced-by-a-csubtype-of,
			   active-overrides));
	end unless;
	//
	// If we have modified `active-overrides' here its tail is
	// either empty or consists of an override that was introduced
	// by a class which is neither a subclass nor a superclass of
	// the class that introduced the current override...
	unless (active-overrides.tail == #())
	  //
	  // ... and we barf if there is such an override.
	  compiler-fatal-error
	    ("Class %s must override slot %s itself to resolve "
	       "the conflict in inheriting overrides from each "
	       "of %=",
	     cclass, slot.slot-getter.variable-name,
	     map(override-introduced-by, active-overrides));
	end unless;
      end if;
    end for;
  end for;
end function check-conflicting-overrides;

// Unique ID assignment.
// ====================

// $class-for-id  -- internal.
//
define constant $class-for-id = make(<object-table>);


// set-and-record-unique-id  -- exported.
//
// Sets the `unique-id' slot of `class' to `id' and reports an error
// if this id was previously assigned.
//
define function set-and-record-unique-id
    (id :: false-or(<integer>), class :: <cclass>) => ();
  if (id)
    really-set-and-record-unique-id(id, class);
  end;
end function set-and-record-unique-id;

// really-set-and-record-unique-id -- internal.
//
// Does the work for `set-and-record-unique-id' but requires an
// <integer> id.  Also useful for `assign-unique-ids'.
//
define inline function really-set-and-record-unique-id
    (id :: <integer>, class :: <cclass>) => ();
  let clash = element($class-for-id, id, default: #f);
  if (clash)
    compiler-fatal-error
      ("Can't give both %= and %= unique id %d, because then "
	 "it wouldn't be unique.  You should try to pick a"
	 "different unique-id-base.",
       clash, class, id);
  end;
  $class-for-id[id] := class;
  class.unique-id := id;
end function;


// assign-unique-ids  -- exported.
//
// Assign unique ids to all classes.
//
define function assign-unique-ids (base :: <integer>) => ();
  local
    method grovel (class :: <cclass>, this-id :: <integer>)
	=> (next-id :: <integer>);
      let next-id = this-id;
      if (class.loaded?)
	unless (class.sealed?)
	  for (sub in class.direct-subclasses)
	    if (sub.direct-superclasses.first == class)
	      next-id := grovel(sub, next-id);
	    end if;
	  end for;
	end unless;
      else
	unless (class.abstract?)
	  really-set-and-record-unique-id(next-id, class);
	  next-id := next-id + 1;
	end unless;
	for (sub in class.direct-subclasses)
	  if (sub.direct-superclasses.first == class)
	    next-id := grovel(sub, next-id);
	  end if;
	end for;
	set-id-range(class, this-id, next-id);
      end if;
      next-id;
    end method grovel;
  grovel(dylan-value(#"<object>"), base);
end function assign-unique-ids;


// set-id-range -- internal.
//
// If `class' is sealed, it is possible to determine the unique ids
// assigned to all subclasses at compile time.  This function
// determines this range and sets the `subclass-id-range-min' and
// `subclass-id-range-max' slots of `class' if possible.
//
define inline function set-id-range
    (class :: <cclass>, this-id :: <integer>, next-id :: <integer>)
 => ();
  if (class.sealed?)
    block (return)
      for (sub in class.subclasses)
	unless (sub.abstract?
		  | (sub.unique-id & this-id <= sub.unique-id))
	  return();
	end unless;
      end for;
      class.subclass-id-range-min := this-id;
      class.subclass-id-range-max := next-id - 1;
    end block;
  end if;
end function set-id-range;

// Layout tables.
// =============


// <layout-table>  -- exported.
//
define class <layout-table> (<object>)
  // size of the runtime object in bytes???
  slot layout-length :: <integer>,
    init-value: 0, init-keyword: length:;
  slot layout-holes :: <list>,
    init-value: #(), init-keyword: holes:;
end class <layout-table>;

define sealed domain make (singleton(<layout-table>));
define sealed domain initialize (<layout-table>);


// copy-layout-table  -- internal.
//
// Copies a <layout-table>.
//
define inline function copy-layout-table (layout :: <layout-table>)
  make(<layout-table>,
       length: layout.layout-length,
       holes: shallow-copy(layout.layout-holes));
end function copy-layout-table;


// find-position  -- internal.
//
// find the position `bytes' bytes before the end of
// `layout.layout-length' after aligning `layout' according to
// `alignment'???
//
// Called from `layout-slot'.
//
define function find-position (layout :: <layout-table>,
			     bytes :: <integer>,
			     alignment :: <integer>)
    => offset :: <integer>;
  block (return)
    for (prev = #f then remaining,
	 remaining = layout.layout-holes then remaining.tail,
	 until: remaining == #())
      unless (zero?(bytes))
	let hole = remaining.head;
	let posn = hole.head;
	let aligned = ceiling/(posn, alignment) * alignment;
	let surplus = (posn + hole.tail) - (aligned + bytes);
	if (zero?(surplus))
	  if (posn == aligned)
	    if (prev)
	      prev.tail := remaining.tail;
	    else
	      layout.layout-holes := remaining.tail;
	    end if;
	  else
	    remaining.head := pair(posn, aligned - posn);
	  end if;
	  return(aligned);
	elseif (positive?(surplus))
	  if (posn == aligned)
	    remaining.head := pair(aligned + bytes, surplus);
	  else
	    hole.tail := aligned - posn;
	    remaining.tail
	      := pair(pair(aligned + bytes, surplus), remaining.tail);
	  end if;
	  return(aligned);
	end if;
      end unless;
      
    finally
      //
      // length according to layout
      let len = layout.layout-length;
      //
      // if `len' is a multiple of `alignment' we have
      // `len' = `aligned' otherwise we have `len' < `aligned'
      let aligned = ceiling/(len, alignment) * alignment;
      if (len < aligned)
	let new = list(pair(len, aligned - len));
	if (prev)
	  prev.tail := new;
	else
	  layout.layout-holes := new;
	end if;
      end if;
      layout.layout-length := aligned + bytes;
      return(aligned);
    end for;
  end block;
end function find-position;

// Position tables.
// ===============

// Instances of class <position-table> are stored in the slot infos
// for instance and each-subclass-allocated slots, i.e., in objects of
// type <instance-slot-info> and <each-subclass-slot-info>.


// <position-table>  -- exported
//
define class <position-table> (<object>)
  slot pt-entries :: false-or(<pt-entry>) = #f;
end class <position-table>;

define sealed domain make (singleton(<position-table>));
define sealed domain initialize (<position-table>);


// print-object {<position-table>}
//    -- method on exported GF.
//
define method print-object
    (table :: <position-table>, stream :: <stream>) => ();
  pprint-logical-block
    (stream,
     prefix: "{",
     body: method (stream)
	     write-class-name(table, stream);
	     write-element(stream, ' ');
	     write-address(table, stream);
	     write(stream, ", ");
	     pprint-indent(#"block", 2, stream);
	     pprint-newline(#"linear", stream);
	     pprint-logical-block
	       (stream,
		prefix: "(",
		body: method (stream)
			for (entry = table.pt-entries then entry.pt-entry-next,
			     first? = #t then #f,
			     while: entry)
			  unless (first?)
			    write-element(stream, ' ');
			    pprint-newline(#"linear", stream);
			  end unless;
			  print(entry, stream);
			end for;
		      end method,
		suffix: ")");
	   end method,
     suffix: "}");
end method print-object;


// <slot-position>  -- exported.
//
define constant <slot-position>
  = type-union(<integer>, singleton(#"data-word"));

// <pt-entry>  -- internal.
//
// Position table entries form a threaded list along the
// `pt-entry-next' slot.
//
define class <pt-entry> (<object>)
  constant slot pt-entry-class :: <cclass>,
    required-init-keyword: class:;
  constant slot pt-entry-position :: <slot-position>,
    required-init-keyword: position:;
  slot pt-entry-next :: false-or(<pt-entry>),
    required-init-keyword: next:;
end class <pt-entry>;

define sealed domain make (singleton(<pt-entry>));
define sealed domain initialize (<pt-entry>);


// print-object {<pt-entry>}
//    -- method on exported GF.
//
define method print-object
    (entry :: <pt-entry>, stream :: <stream>) => ();
  pprint-fields(entry, stream,
		class: entry.pt-entry-class,
		position: entry.pt-entry-position);
end method print-object;


// as{singleton(<list>), <position-table>}
//    -- method on imported GF.
//
// Convert the position table into a list.  Used by the heap dumper so
// that we don't have to try dumping position tables.
//
define method as (class == <list>, table :: <position-table>)
    => res :: <list>;
  for (entry = table.pt-entries then entry.pt-entry-next,
       result = #()
	 then pair(pair(entry.pt-entry-class, entry.pt-entry-position),
		   result),
       while: entry)
  finally
    reverse!(result);
  end for;
end method as;


// clear-positions{<position-table>} -- internal.
//
// Clear a position table.  Only used if someone re-runs part of the compiler
// from the debugger.
//
define inline function clear-positions (table :: <position-table>) => ();
  table.pt-entries := #f;
end function clear-positions;


// add-position -- internal.
//
// Add a position to a position-table.  Raises an error if a subclass
// of the new class has already been assigned a position.  The
// positions are threaded such that a class is never preceded by a
// superclass; retrieving the positions relies on this.
//
// Called from the methods `inherit-layout' and `layout-slot'.
// 
define function add-position
    (table :: <position-table>, class :: <cclass>, position :: <slot-position>)
    => ();
  raise-error-if-subclass-in-table(table, class);
  //
  // If we find a superclass C of `class' in the table and if C's
  // `entry-position' is the same as the new position we do nothing,
  // otherwise we add a new <pt-entry> to the front of the threaded
  // list.
  block (return)
    block (add-it)
      for (entry = table.pt-entries then entry.pt-entry-next,
	   while: entry)
	if (csubtype?(class, entry.pt-entry-class))
	  if (entry.pt-entry-position == position)
	    return();
	  else
	    add-it();
	  end if;
	end if;
      end for;
    end block;			// add-it()

    table.pt-entries
      := make(<pt-entry>, class: class, position: position,
	      next: table.pt-entries);
  end block;			// return()
end function add-position;


// raise-error-if-subclass-in-table -- internal.
//
// Raise an error if a subclass of `class' is already a member of the
// table, do nothing otherwise.
//
define inline function raise-error-if-subclass-in-table
    (table :: <position-table>, class :: <cclass>)
 => ();
  for (entry = table.pt-entries then entry.pt-entry-next,
       while: entry)
    if (csubtype?(entry.pt-entry-class, class))
      error("Attempting to add an entry for %s, but %s (a subclass) "
	      "is already in the position-table.",
	    class, entry.pt-entry-class);
    end if;
  end for;
end function;


// get-direct-position -- exported.
//
// Return the position for direct instances of class.
// Called from `inherit-layout' and `find-slot-offset'.
// 
define function get-direct-position
    (table :: <position-table>, class :: <cclass>)
    => position :: false-or(<slot-position>);
  block (return)
    for (entry = table.pt-entries then entry.pt-entry-next,
	 while: entry)
      //
      // We run along the thread until we find a position whose
      // `pt-entry-class' is a superclass of `class'.  This works
      // because we enter the position table entries sorted from more
      // to less specific classes.
      if (csubtype?(class, entry.pt-entry-class))
	return(entry.pt-entry-position);
      end if;
    end for;
    #f;
  end block;
end function get-direct-position;


// get-general-position -- exported.
//
// Return the position for possibly indirect instances of class, if there is
// a single such position.  If there isn't, then return #f.
//
// Called from `find-slot-offset'.
// 
define function get-general-position
    (table :: <position-table>, class :: <cclass>)
    => offset :: false-or(<slot-position>);
  block (return)
    let result = #f;
    for (entry = table.pt-entries then entry.pt-entry-next,
	 while: entry)
      let entry-class = entry.pt-entry-class;
      if (csubtype?(class, entry-class))
	// This is the entry for direct instances of this class.
	let entry-posn = entry.pt-entry-position;
	if (result & result ~== entry-posn)
	  // It conflicts with the entries we found for subclasses.
	  return(#f);
	else
	  // We found a valid position.
	  return(entry-posn);
	end if;
      elseif (csubtype?(entry-class, class))
	// The entry is for a subclass of the class we are interested in.
	if (result == #f)
	  // It is the first such subclass we have found, so it by itself
	  // can't conflict with anything.
	  result := entry.pt-entry-position;
	elseif (result ~== entry.pt-entry-position)
	  // It conflicts with the position we found for some other class.
	  return(#f);
	end if;
      end if;
    end for;
    error("No entry for %s in %=", class, table);
  end block;
end function get-general-position;

// get-universal-position -- exported.
//
// If there is only one position in the table, return it.  If there is
// no position in the table raise an error.  Otherwise, return #f.
//
define function get-universal-position
    (table :: <position-table>)
    => offset :: false-or(<slot-position>);
  let entry = table.pt-entries;
  unless (entry)
    error("No entries for %=?", table);
  end unless;
  if (entry.pt-entry-next)
    #f;
  else
    entry.pt-entry-position;
  end if;
end function get-universal-position;

// Slot layout stuff.
// =================

// layout-instance-slots  -- exported.
//
define inline function layout-instance-slots () => ();
  do(layout-slots-for, *All-Classes*);
end function layout-instance-slots;


// tc:
// If layout-slots-for is called on a functional class we may
// introduce the following call-chain:
// use-data-word-representation
// => pick-representation
// => direct-representation
// => assign-representation
// => layout-slots-for
// but now we have class.layout-computed? set to #"computing".
// Oops.
//
// A fix that somehow works is the following function but we should
// find the real error some day.


// layout-slots-for-if-possible  -- internal.
//
define function layout-slots-for-if-possible (class :: <cclass>) => ();
  if (class.layout-computed? ~== #"computing")
    layout-slots-for(class);
  end if;
end function layout-slots-for-if-possible;


// layout-slots-for  -- exported.
//
// Compute the layout for `class''s slots.
//
define function layout-slots-for (class :: <cclass>) => ();
  if (class.layout-computed? == #f)
    //
    // Note that we are now working on this class.
    class.layout-computed? := #"computing";
    //
    // Make sure all the superclasses have been assigned a layout.
    ensure-superclass-layout(class);
    //
    // Pick representation for each instance slot.  If the representation
    // doesn't have a bottom value and slot isn't guaranteed to be initialized,
    // then also add a bound? slot for it.
    pick-slot-representations(class);
    //
    // Now that all slots have been added, convert them into a simple
    // object vector.
    class.all-slot-infos := as(<simple-object-vector>, class.all-slot-infos);
    //
    // Do the real work...
    compute-slot-layout-for(class);
  elseif (class.layout-computed? == #"computing")
    error("Someone left %s.layout-computed? as #\"computing\".", class);
  end if;
end function layout-slots-for;


// ensure-superclass-layout -- internal.
//
// Ensure that the layout of all of `class''s superclasses is
// computed.
//
define inline function ensure-superclass-layout(class :: <cclass>)
 => ();
  for (super in class.direct-superclasses)
    layout-slots-for(super);
  end for;
end function;


// pick-slot-representations -- internal.
//
define inline function pick-slot-representations (class :: <cclass>)
 => ();
  for (slot in class.new-slot-infos)
    if (instance?(slot, <instance-slot-info>))
      let rep = pick-representation(slot.slot-type, #"space");
      slot.slot-representation := rep;
      add-initialized?-slot-if-necessary(slot, rep);
    end if;
  end for;
end function;


// add-initialized?-slot-if-necessary -- internal.
//
// If `slot' is not guaranteed to be initialized and if `rep' has no
// bottom value we add an `slot-initialized?' slot to the object.
//
define inline function add-initialized?-slot-if-necessary
    (slot :: <slot-info>, rep :: <representation>)
  => ();
  unless (slot-guaranteed-initialized?(slot, slot.slot-introduced-by)
	    | rep.representation-has-bottom-value?)
    let class = slot.slot-introduced-by;
    let boolean-ctype = specifier-type(#"<boolean>");
    let init?-slot
      = make(<instance-slot-info>,
	     introduced-by: class,
	     type: boolean-ctype,
	     getter: #f,
	     init-value: make(<literal-false>),
	     slot-representation:
	       pick-representation(boolean-ctype, #"space"));
    slot.slot-initialized?-slot := init?-slot;
    //
    // We have to add it to all the subclasses ourselves because
    // inherit-slots has already run.
    for (subclass in class.subclasses)
      add-slot(init?-slot, subclass);
    end for;
  end unless;
end function;


// compute-slot-layout-for -- internal.
//
// After ensuring that all the superclasses of `class' have their
// layout computed in `layout-slots-for' we do the actual computation
// of the slot layout in this function.
//
define inline function compute-slot-layout-for (class :: <cclass>)
 => ();
  // Are there any superclasses?
  let supers = class.direct-superclasses;
  if (empty?(supers))
    compute-virgin-layout(class);
  else
    compute-inherited-layout(class, supers);
  end if;
  //
  // We are done.
  class.layout-computed? := #t;
end function;


// compute-virgin-layout  -- internal.
//
define inline function compute-virgin-layout (class :: <cclass>) => ();
  //
  // No superclasses, assign all the slots a location, starting with a
  // virgin layout.
  class.instance-slots-layout := make(<layout-table>);
  class.vector-slot := #f;
  class.data-word-slot := #f;
  class.each-subclass-slots-count := 0;
  for (slot in class.all-slot-infos)
    layout-slot(slot, class);
  end for;
end function;


// conmpute-inherited-layout  -- internal.
//
define inline function compute-inherited-layout
    (class :: <cclass>, supers :: <list>)
 => ();
  //
  // We have superclasses, so first inherit the layout of the
  // superclass we get the closest- primary-superclass from.
  let processed :: <simple-object-vector>
    = compute-critical-primary-layout(class, supers);
  //
  // If the class is functional, we might have a data-word-slot to
  // deal with.
  if (class.functional?)
    //
    // Have we inherited a data-word-slot?
    if (class.data-word-slot)
      //
      // Yes, check to see if we have added any other instance slots.
      // andreas: somewhere here the functional class bug hides out
      layout-functional-class-with-data-word(class)
    else
      layout-functional-class-without-data-word(class, processed);
    end if;
  end if;
  //
  // Assign a location for all other slots.
  for (slot in class.all-slot-infos)
    unless (member?(slot, processed))
      layout-slot(slot, class);
    end unless;
  end for;
end function compute-inherited-layout;


// compute-critical-primary-layout  -- internal.
//
define inline function compute-critical-primary-layout
    (class :: <cclass>, supers :: <list>)
 => (processed-slot-infos :: <simple-object-vector>);
  let critical-super = supers.head;
  let critical-primary = critical-super.closest-primary-superclass;
  for (super in supers.tail)
    let primary = super.closest-primary-superclass;
    if (~(primary == critical-primary)
	  & csubtype?(primary, critical-primary))
      critical-super := super;
      critical-primary := primary;
    end;
  end;
  class.instance-slots-layout
    := copy-layout-table(critical-super.instance-slots-layout);
  class.vector-slot := critical-super.vector-slot;
  class.data-word-slot := critical-super.data-word-slot;
  class.each-subclass-slots-count
    := critical-super.each-subclass-slots-count;
  for (slot in critical-super.all-slot-infos)
    inherit-layout(slot, class, critical-super);
  end;
  critical-super.all-slot-infos;
end function;


// layout-functional-class-with-data-word  -- internal.
//
define inline function layout-functional-class-with-data-word
    (class :: <cclass>)
 => ();
  block (return)
    for (slot in class.all-slot-infos)
      if (slot ~== class.data-word-slot
	    & instance?(slot, <instance-slot-info>)
	    & slot.slot-introduced-by ~== object-ctype())
	//
	// Yup, tell the representation stuff that this class
	// needs the full general representation.
	use-general-representation(class);
	return();
      end if;
    end for;
    //
    // Nope, tell the representation stuff that this class needs a
    // data-word representation.
    use-data-word-representation
      (class, class.data-word-slot.slot-type);
  end block;
end function;


// layout-functional-class-without-data-word  -- internal.
//
define inline function layout-functional-class-without-data-word
    (class :: <cclass>, processed :: <simple-object-vector>)
 => ();
  //
  // We didn't inherit a data-word, so lets see if we introduce one.
  block (return)
    let instance-slot = #f;
    for (slot in class.all-slot-infos)
      if (instance?(slot, <instance-slot-info>)
	    & slot.slot-introduced-by ~== object-ctype())
	if (instance-slot)
	  //
	  // There are at least two instance slots.  That means no
	  // data-word for us.
	  return();
	end if;
	instance-slot := slot;
      end if;
    end for;
    //
    // Was there a single instance slot.
    if (instance-slot)
      //
      // Assert that we didn't inherit a non-data-word position
      // from the critical superclass.  We shouldn't be able to
      // because if something kept it out of the data-word in that
      // class, that same thing should keep it out of the
      // data-word in this class.
      assert(~member?(instance-slot, processed));
      //
      // Can we stick it in the data-word?
      if (instance?(instance-slot.slot-representation,
		    <data-word-representation>))
	//
	// Yes!  Record it, and tell the representation stuff that
	// we want a data-word representation.
	class.data-word-slot := instance-slot;
	use-data-word-representation(class, instance-slot.slot-type);
	//
	// Assert that the slot is also in the data-word for whoever
	// introduced it.
	assert(instance-slot.slot-introduced-by.data-word-slot
		 == instance-slot);
      end if;
    end if;
  end block;
end function;


// inherit-layout  -- internal GF.
//
define generic inherit-layout
    (slot :: <slot-info>, class :: <cclass>, super :: <cclass>)
 => ();


// inherit-layout {<slot-info>}
//
// By default we do nothing to inherit the layout.
// 
define method inherit-layout
    (slot :: <slot-info>, class :: <cclass>, super :: <cclass>) => ();
  // Default method -- do nothing.
end;

// inherit-layout {type-union(<instance-slot-info>,
//                            <each-subclass-slot-info>)}
//    -- method on internal GF.
//
// Since <instance-slot-info> and <each-subclass-slot-info> have a
// position table we need to store `slot''s slot position.
//
define method inherit-layout
    (slot :: type-union(<instance-slot-info>, <each-subclass-slot-info>),
     class :: <cclass>, super :: <cclass>)
    => ();
  add-position(slot.slot-positions, class,
	       get-direct-position(slot.slot-positions, super));
end method inherit-layout;


// layout-slot {<slot-info>}
//    -- method on exported GF.
//
define method layout-slot
    (slot :: <slot-info>, class :: <cclass>) => ();
  // Default method -- do nothing.
end;

// layout-slot {<instance-slot-info>}
//    -- method on exported GF.
//
// ???
//
define method layout-slot (slot :: <instance-slot-info>, class :: <cclass>)
    => ();
  if (class.vector-slot)
    compiler-fatal-error
      ("variable length slots must be the last slot in the class.");
  end;
  let rep = slot.slot-representation;
  let offset
    = if (slot == class.data-word-slot)
	#"data-word";
      else
	find-position(class.instance-slots-layout,
		      rep.representation-size,
		      rep.representation-alignment);
      end if;
  add-position(slot.slot-positions, class, offset);
end;

// layout-slot {<vector-slot-info>}
//    -- method on exported GF.
//
define method layout-slot (slot :: <vector-slot-info>, class :: <cclass>)
    => ();
  if (class.vector-slot)
    compiler-fatal-error
      ("variable length slots must be the last slot in the class.");
  end;
  class.vector-slot := slot;
  let rep = slot.slot-representation;
  let offset = find-position(class.instance-slots-layout, 0,
			     rep.representation-alignment);
  add-position(slot.slot-positions, class, offset);
end;

// layout-slot {<each-subclass-slot-info>}
//    -- method on exported GF.
//
define method layout-slot
    (slot :: <each-subclass-slot-info>, class :: <cclass>)
    => ();
  let posn = class.each-subclass-slots-count;
  add-position(slot.slot-positions, class, posn);
  class.each-subclass-slots-count := posn + 1;
end;

// Compile time determination of slot offsets and other gunk.
// =========================================================

// find-slot-offset -- exported.
//
// Return the static position that slot occures in general instances of
// instance-type, or #f if no single such position exists.
// 
define generic find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <ctype>)
    => res :: false-or(<slot-position>);

// find-slot-offset {<ctype>}
//    -- method on exported GF.
//
define method find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <ctype>)
    => res :: false-or(<slot-position>);
  #f;
end method find-slot-offset;


// find-slot-offset {<cclass>}
//    -- method on exported GF.
//
define method find-slot-offset
    (slot :: <instance-slot-info>, instance-class :: <cclass>)
    => res :: false-or(<slot-position>);
  if (csubtype?(instance-class.closest-primary-superclass,
		slot.slot-introduced-by))
    get-direct-position(slot.slot-positions, instance-class)
      | error("Can't find position for %= in class %s?", slot, instance-class);
  elseif (instance-class.sealed?)
    get-general-position(slot.slot-positions, instance-class);
  else
    #f;
  end if;
end method find-slot-offset;


// find-slot-offset {<limited-ctype>}
//    -- method on exported GF.
//
define method find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <limited-ctype>)
    => res :: false-or(<slot-position>);
  find-slot-offset(slot, instance-type.base-class);
end method find-slot-offset;


// find-slot-offset {<direct-instance-ctype>}
//    -- method on exported GF.
//
define method find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <direct-instance-ctype>)
    => res :: false-or(<slot-position>);
  let instance-class = instance-type.base-class;
  get-direct-position(slot.slot-positions, instance-class)
    | error("Can't find position for %= in class %s?", slot, instance-class);
end method find-slot-offset;


// find-slot-offset {<union-ctype>}
//    -- method on exported GF.
//
define method find-slot-offset
    (slot :: <instance-slot-info>, instance-type :: <union-ctype>)
    => res :: false-or(<slot-position>);
  let mems = instance-type.members;
  if (empty?(mems))
    #f;
  else
    block (punt)
      let result = find-slot-offset(slot, mems.head) | punt(#f);
      for (mem in mems.tail)
	if (find-slot-offset(slot, mem) ~== result)
	  punt(#f);
	end if;
      end for;
      result;
    end block;
  end if;
end method find-slot-offset;


// slot-guaranteed-initialized? -- exported
//
define function slot-guaranteed-initialized?
    (slot :: <slot-info>, instance-type :: <ctype>) => res :: <boolean>;
  if (slot.slot-init-value | slot.slot-init-function
	| slot.slot-init-keyword-required?)
    #t;
  elseif (empty?(slot.slot-overrides))
    #f;
  else
    csubtype?(instance-type,
	      reduce1(ctype-union,
		      map(override-introduced-by,
			  slot.slot-overrides)));
  end;
end function slot-guaranteed-initialized?
;
    
// best-idea-of-class {<cclass>}
//    -- method on exported GF.
//
define method best-idea-of-class (type :: <cclass>)
    => res :: <cclass>;
  type;
end;


// best-idea-of-class {<limited-ctype>}
//    -- method on exported GF.
//
define method best-idea-of-class (type :: <limited-ctype>)
    => res :: <cclass>;
  base-class(type);
end;


// best-idea-of-class {<union-ctype>}
//    -- method on exported GF.
//
define method best-idea-of-class (type :: <union-ctype>)
    => res :: false-or(<cclass>);
  let mems = type.members;
  if (empty?(mems))
    #f;
  else
    block (punt)
      let result = best-idea-of-class(mems.head) | punt(#f);
      for (mem in mems.tail)
	let other-base-class = best-idea-of-class(mem) | punt(#f);
	block (return)
	  for (super in result.precedence-list)
	    if (csubtype?(mem, super))
	      result := super;
	      return();
	    end;
	  end;
	end;
      end;
      result;
    end;
  end;
end;


// best-idea-of-class {<unknown-type>}
//    -- method on exported GF.
//
define method best-idea-of-class (type :: <unknown-ctype>)
    => res :: <false>;
  #f;
end;

// Defined classes.
// ===============

// <defined-cclass>  -- exported.
//
define class <defined-cclass> (<cclass>)
  //
  // The <class-definition> that installed this class.
  slot class-defn :: <class-definition>, init-keyword: defn:;
end class;

define sealed domain make (singleton(<defined-cclass>));

// Limited mumble classes.
// ======================

// <limited-cclass>  -- exported.
//
define abstract class <limited-cclass> (<cclass>)
end;

define sealed domain make (singleton(<limited-cclass>));

// Direct instance types.
// =====================

// <direct-instance-ctype>  -- exported.
//
define class <direct-instance-ctype> (<limited-ctype>, <ct-value>)
end class;

define sealed domain make (singleton(<direct-instance-ctype>));


// print-object {<direct-instance-ctype>}
//    -- method on imported GF.
//
define method print-object
    (type :: <direct-instance-ctype>, stream :: <stream>) => ();
  pprint-fields(type, stream, base-class: type.base-class);
end;


// print-message {<direct-instance-ctype>}
//    -- method on imported GF.
//
define method print-message
    (type :: <direct-instance-ctype>, stream :: <stream>) => ();
  format(stream, "direct-instance(%s)", type.base-class);
end;


// make {<direct-instance-ctype>}
//    -- method on imported GF.
//
define method make
    (class == <direct-instance-ctype>, #next next-method,
     #key base-class :: <cclass>)
    => res :: <direct-instance-ctype>;
  base-class.%direct-type | (base-class.%direct-type := next-method());
end method make;

// ct-value-cclass {<direct-instance-ctype>}
//    -- method on exported GF.
//
define method ct-value-cclass (object :: <direct-instance-ctype>)
    => res :: <cclass>;
  specifier-type(#"<direct-instance>");
end;


// ctype-extent-dispatch{<direct-instance-ctype>}
//    -- method on exported GF.
//
// If the base-class is abstract, then there can be no instances of it.
// Otherwise, check to see if the class is one of the ones we know the
// extent of.
// 
define method ctype-extent-dispatch (type :: <direct-instance-ctype>)
    => res :: <ctype>;
  let class = type.base-class;
  if (class.abstract?)
    empty-ctype();
  else
    select (class)
      specifier-type(#"<integer>") =>
	// ### Should really be making an integer set.
	make-canonical-limited-integer(class, #f, #f);
      specifier-type(#"<extended-integer>") =>
	// ### Should really be making an integer set.
	make-canonical-limited-integer(class, #f, #f);
      specifier-type(#"<character>") =>
	// ### Should really be making a character set.
	type;
      specifier-type(#"<false>") =>
	make(<singleton-ctype>, value: as(<ct-value>, #f), base-class: class);
      specifier-type(#"<true>") =>
	make(<singleton-ctype>, value: as(<ct-value>, #t), base-class: class);
      specifier-type(#"<empty-list>") =>
	make(<singleton-ctype>, value: as(<ct-value>, #()), base-class: class);
      otherwise =>
	type;
    end select;
  end if;
end method ctype-extent-dispatch;


// csubtype-dispatch{<limited-ctype>,<direct-instance-ctype>}
//    -- method on exported GF.
//
// A limited type is a subtype of a direct-instance-ctype iff the limited-ctype
// has a single direct class, and it is the direct-instance-ctype's base-class.
// 
define method csubtype-dispatch
    (type1 :: <limited-ctype>, type2 :: <direct-instance-ctype>)
    => result :: <boolean>;
  let direct-classes = type1.find-direct-classes;
  direct-classes
    & direct-classes.size == 1
    & direct-classes.first == type2.base-class;
end method csubtype-dispatch;


// Subclass types.
// ==============

define class <subclass-ctype>
    (<limited-ctype>, <ct-value>, <identity-preserving-mixin>)
  //
  // The class this type covers the subclasses of.
  constant slot subclass-of :: <cclass>,
    required-init-keyword: of:;
end class <subclass-ctype>;

define sealed domain make (singleton(<subclass-ctype>));


// make {<subclass-ctype>}
//    -- method on imported GF.
//
define method make (class == <subclass-ctype>, #next next-method,
		    #key of, base-class)
    => res :: <subclass-ctype>;
  of.subclass-ctype
    | (of.subclass-ctype
	 := next-method(class,
			of: of,
			base-class: base-class | specifier-type(#"<class>")));
end method make;


// print-object {<subclass-ctype>}
//    -- method on imported GF.
//
define method print-object
    (type :: <subclass-ctype>, stream :: <stream>) => ();
  pprint-fields(type, stream, of: type.subclass-of);
end method print-object;


// print-message {<subclass-ctype>}
//    -- method on imported GF.
//
define method print-message
    (type :: <subclass-ctype>, stream :: <stream>) => ();
  format(stream, "subclass(%s)", type.subclass-of);
end method print-message;



// ct-value-cclass {<subclass-ctype>}
//    -- method on exported GF.
//
define method ct-value-cclass (ctv :: <subclass-ctype>)
    => res :: <cclass>;
  specifier-type(#"<subclass>");
end method ct-value-cclass;


// ctype-extent-dispatch{<subclass-ctype>}
//    -- method on exported GF.
//
// If the class is sealed, then build a union of singletons for each
// possible subclass.  Otherwise, just stick with the subclass-ctype.
//
define method ctype-extent-dispatch (type :: <subclass-ctype>)
    => res :: <ctype>;
  let class = type.subclass-of;
  if (class.sealed?)
    reduce1(ctype-union,
	    map(method (class :: <cclass>)
		  make(<singleton-ctype>, value: class);
		end method,
		class.subclasses));
  else
    type;
  end if;
end method ctype-extent-dispatch;


// csubtype-dispatch{<subclass-ctype>, <subclass-ctype>}
//    -- method on exported GF.
//
// If the class is sealed, then build a union of singletons for each possible
// subclass.  Otherwise, just stick with the subclass-ctype.
// 
define method csubtype-dispatch
    (type1 :: <subclass-ctype>, type2 :: <subclass-ctype>)
    => res :: <boolean>;
  csubtype?(type1.subclass-of, type2.subclass-of);
end method csubtype-dispatch;


// csubtype-dispatch{<singleton-ctype>, <subclass-ctype>}
//    -- method on exported GF.
//
// If the class is sealed, then build a union of singletons for each
// possible subclass.  Otherwise, just stick with the subclass-ctype.
//
define method csubtype-dispatch
    (type1 :: <singleton-ctype>, type2 :: <subclass-ctype>)
    => res :: <boolean>;
  let ctv = type1.singleton-value;
  instance?(ctv, <cclass>) & csubtype?(ctv, type2.subclass-of);
end method csubtype-dispatch;



// csubtype-intersection-dispatch{<subclass-ctype>, <subclass-ctype>}
//    -- method on exported GF.
//
// If the class is sealed, then build a union of singletons for each
// possible subclass.  Otherwise, just stick with the subclass-ctype.
//
define method ctype-intersection-dispatch
    (type1 :: <subclass-ctype>, type2 :: <subclass-ctype>)
    => (res :: <ctype>, exact? :: <boolean>);
  let (intersection, exact?)
    = ctype-intersection(type1.subclass-of, type2.subclass-of);
  let result = empty-ctype();
  for (class in intersection.members)
    assert(instance?(class, <cclass>));
    result := ctype-union(result, make(<subclass-ctype>, of: class));
  end for;
  values(result, exact?);
end method ctype-intersection-dispatch;



// Proxies.
// =======

// <proxy>  -- exported.
//
define class <proxy> (<ct-value>, <identity-preserving-mixin>)
  slot proxy-for :: <cclass>, required-init-keyword: for:;
end;

define sealed domain make (singleton(<proxy>));
define sealed domain initialize (<proxy>);

// $proxy-memo  -- internal.
//
define constant $proxy-memo = make(<object-table>);


// make {<proxy>}
//    -- method on imported GF.
//
define method make (class == <proxy>, #next next-method, #key for: cclass)
    => res :: <proxy>;
  element($proxy-memo, cclass, default: #f)
    | (element($proxy-memo, cclass) := next-method());
end;


// print-object {<proxy>}
//    -- method on imported GF.
//
define method print-object (proxy :: <proxy>, stream :: <stream>) => ();
  pprint-fields(proxy, stream, for: proxy.proxy-for);
end;


// print-message {<proxy>}
//    -- method on imported GF.
//
define method print-message (proxy :: <proxy>, stream :: <stream>) => ();
  format(stream, "proxy for %s", proxy.proxy-for);
end;
