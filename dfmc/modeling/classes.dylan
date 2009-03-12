Module:   dfmc-modeling
Synopsis: Class type and context-sensitive operations
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Context sensitive queries.

// Direct subclasses queries.

define method ^all-direct-subclasses-known? 
    (class :: <&class>) => (well? :: <boolean>)
  ^class-sealed?(class) 
    // HACK: JB ADDED THIS FOR LOOSE MODE
    & ~form-dynamic?(model-definition(class)) 
    & ~library-contains-wildcard-subclasses?
         (language-definition(model-library(class)))
end method;

define inline function map-collecting-unless-false 
    (f :: <function>, s :: <sequence>) => (s :: <list>)
  collecting ()
    for (item in s)
      let result = f(item);
      if (result) collect(result) end;
    end;
  end;
end function;

define method ^direct-subclasses-known
    (class :: <&class>) => (subclasses :: <sequence>)
  let binding = model-variable-binding(class);
  if (binding)
    let definitions = binding-direct-subclass-definitions(binding);
    map-collecting-unless-false(form-model-object, definitions);
  else
    #() // This class didn't have an explicit definition
  end
end method;

define method ^sealed-with-no-subclasses? (class :: <&class>) => well?;
  ^all-direct-subclasses-known?(class) & empty?(^direct-subclasses(class))
end method;


define method ^direct-subclasses
    (class :: <&class>) => (subclasses :: <sequence>)
  unless (%direct-subclasses-initialized?(class))
    let form = model-definition(class);
    if (form)
      with-dependent-context ($compilation of form)
	%direct-subclasses(class)
	  := mapped-model(^direct-subclasses-known(class))
      end;
    end;
    %direct-subclasses-initialized?(class) := #t;
  end;
  %direct-subclasses(class)
end method;

// Other utilities.


// Returns a list of all subclasses or #f if the class or some subclass is open
// or otherwise unavailable.
define function ^all-subclasses-if-sealed (class :: <&class>)
 => (subclasses :: false-or(<list>));
  let subclasses = #();
  local method add-if (class)
	  if (^all-direct-subclasses-known?(class))
	    member?(class, subclasses) |
              (~member?(class, ^direct-subclasses(class)) &    // gts,98feb09
	      if (every?(add-if, ^direct-subclasses(class)))
		subclasses := pair(class, subclasses)
	      end )
	  end
	end method;
  add-if(class) & subclasses
end function;

// ^sealed-with-no-subclasses? is implemented in the class definition code.

define function ^least-primary-superclass (cl :: <&class>) 
    => (result :: false-or(<&class>))
  // Shared subroutine used by inheritance consistency checking and in the 
  // typist.
  // Cl if it's primary, otherwise least primary superclass of leftmost
  // CPL element that has one.  Least primary superclass of <object> is #f.
  // That amounts to: find the leftmost primary class in the CPL.
  any?(conjoin(^class-primary?, identity), ^all-superclasses(cl))
end;

//// Type operations.

//// Base type.

define method ^base-type (type :: <&class>) => (type :: <&class>)
  type
end method ^base-type;

//// Subtype? relationships.

define method ^subtype? (c1 :: <&class>, c2 :: <&class>) 
    => (value :: <boolean>)
  member?(c2, ^all-superclasses(c1))
end method;

//// Disjointness relationships.

// Disjointness is library-relative for compile-time sealing analysis.
// A method that was allowed to be defined after testing disjoint from
// some sealed domain becomes a potential blocking method (in a 
// particular argument position) of any class that might be defined 
// later to contravene the disjointness test (at that argument 
// position).

// TODO: On the grounds that rule 3 requires more information to be
// computed to do checking, should we define all classes "first" in a 
// compiler pass and mop-up as many problems as possible on the method
// tests? I guess you'd really like error messages in both places to
// indicate the problem.

// Are the classes disjoint as far as a particular library knows?

// "Two classes are disjoint if they have no common subclasses".

// This is only used to possibly give a domain sealing violation warning
// if #f is returned.  So it should return #t unless it's sure such a
// warning would be appropriate.
define method ^known-disjoint?
    (c1 :: <&class>, c2 :: <&class>) => (value :: <boolean>)
/*  local method guaranteed-joint-2? (class)
	  member?(class2, ^all-superclasses(class))
	    | any?(guaranteed-joint-2?,
		   if (^class-sealed?(class) |
			 current-library-description?(model-library(class)))
		     ^direct-subclasses(class)
		   else // else depends on current library, need to compute
		     ^direct-subclasses-known(class)
		   end)
	end;
  // This all-superclasses membership test should be an optimization because
  // the all-superclasses vector is precomputed and readily available in  
  // all contexts, whereas the direct subclasses may have to be computed.
  // This is potentially very expensive, particularly for classes towards
  // the top of the heterarchy (e.g. <object>).
  ~member?(class1, ^all-superclasses(class2))
    & ~member?(class2, ^all-superclasses(class1))
    & ~guaranteed-joint-2?(class1)
end method; */
  // True if classes c1 & c2 are guaranteed disjoint.  In general, 2 classes
  // are disjoint if they have no common subclasses.  All this squirming around
  // is because that's difficult to determine statically.  See example
  // in guaranteed-joint?.
  local method ^classes-disjoint-by-primary?(c1 :: <&class>, c2 :: <&class>) 
            => (disjoint? :: <boolean>)
          // We can prove c1 & c2 are disjoint if their primary superclasses
          // won't allow diplomatic relations.  This happens when both have
          // primary superclasses, and those primaries aren't themselves
          // in a supertype/subtype relationship.
          // 
          // In fact, you just have to check the leftmost primaries on each:
          // The primaries of each class form a chain, a subset of the CPL.
          // If leftmost-prim-1 is a subclass of leftmost-prim-2, then
          // chain above leftmost-prim-2 is already in 1's CPL, and vice versa.
          let c1-left-primary = ^least-primary-superclass(c1);
          let c2-left-primary = ^least-primary-superclass(c2);
          c1-left-primary ~== #f                       &
          c2-left-primary ~== #f                       &
          ~^subtype?(c1-left-primary, c2-left-primary) &
          ~^subtype?(c2-left-primary, c1-left-primary)
        end,
        method ^classes-disjoint-by-slots?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // DRM p. 57: "... two classes which specify a slot with
          // the same getter or setter generic function are disjoint..."
          // Details cribbed from ^compute-slot-descriptors.
          local method slot-match?
                  (s1 :: <&slot-descriptor>, s2 :: <&slot-descriptor>)
                    => (match? :: <boolean>)
                  // Owners different and either getters or setters match.
                  // (I.e., don't be confused by commonly inherited slots!)
                  ^slot-owner(s1) ~== ^slot-owner(s2) & 
                  (^slot-getter(s1) == ^slot-getter(s2) |
                   (^slot-setter(s1) == ^slot-setter(s2) &
		      ^slot-setter(s1) & ^slot-setter(s2) & #t))
                end;
          let c2-slots = ^slot-descriptors(c2);
          any?(rcurry(member?, c2-slots, test: slot-match?), 
              ^slot-descriptors(c1))
        end,
        method ^classes-disjoint-by-domain?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // *** There is another disjointness test, hence this stub: 
          // disjoint-by-sealed-domains.  This is true if the classes
          // are not known to be joint (i.e., there is no explicitly
          // known common subclass) and there is a sealed domain that
          // guarantees that no new common subclasses can be defined.
          ignore(c1); ignore(c2);
          #f
        end,
        // *** There are a bunch of loose/tight compilation issues:
        //     - how much is known about a class in another library?
        //     - how about if it's not exported from that library?
        //     Ultimately, this is a definition of a library signature.
        // *** Can I exploit model-library(ci) here somehow?
        // *** Memoize this & do recursively, not consing like ^all-subclasses.
        // *** ^direct-subclasses-known-to ? ^worldwide-direct-subclasses ?
        method ^classes-disjoint-by-sealing?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // If you get this far, c1 and c2 _could_ be disjoint, but we need
          // to look at subclasses to be sure.  Ensure no common subclass now,
          // and adequate sealing to guarantee there never will be one.
          local method disjoint-using-subclasses? (c1-subclasses :: <sequence>,
                                                   c2 :: <&class>)
                  // we know all of c1's subclasses, and
                  // we know c2's superclasses (but not necessarily its
                  // subclasses)
                  ~any?(rcurry(^subtype?, c2), c1-subclasses)
                end;
          let c1-subclasses = ^all-subclasses-if-sealed(c1);
          if (c1-subclasses)
            disjoint-using-subclasses?(c1-subclasses, c2)
          else
            let c2-subclasses = ^all-subclasses-if-sealed(c2);
            if (c2-subclasses)
              disjoint-using-subclasses?(c2-subclasses, c1)
            else
              #f
            end
          end
        end,
        method ^classes-guaranteed-disjoint-1?(c1 :: <&class>, c2 :: <&class>)
            => (disjoint? :: <boolean>)
          // First check that one is not a subtype of the other
          ~^subtype?(c1, c2) & ~^subtype?(c2, c1)
          // Now they're known not to be subtypes either way.
          & (  ^classes-disjoint-by-primary?(c1, c2)
             | ^classes-disjoint-by-slots?  (c1, c2)
             | ^classes-disjoint-by-domain? (c1, c2)
             | ^classes-disjoint-by-sealing?(c1, c2))
        end;
  // First look in the cache to see if we already know the answer.
  let disjoint-cache
                    = library-type-estimate-disjoint?-cache(current-library-description());
  let cache-key     = pair(c1, c2);
  let cache-element = element(disjoint-cache, cache-key, default: not-found());
  if (found?(cache-element))
    // Found it in the cache.
    values(cache-element, #t)
  else
    // Have to compute it and remember it.  Index under args both ways.
    let val = ^classes-guaranteed-disjoint-1?(c1, c2);
    disjoint-cache[cache-key] := val;
    val
  end
end;

// eof
