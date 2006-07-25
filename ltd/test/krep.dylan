//  -*- Mode: Lisp; Syntax: Common-Lisp;  -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  krep.lisp: Knowledge representation code; final version.
//  Adds support for worlds and attached functions.
requires("krep2");

//  Need some functions from previous version
define variable *primitives* = #(#"and", #"sub", #"ind", #"rel", #"val");

define method add-fact (fact)
  // Add the fact to the data base.
  if (predicate(fact) == #"and")
    begin
      let list92543 = args(fact);
      begin do(add-fact, list92543); list92543; end;
    end;
  elseif (~ every?(method (x) not(instance?(x, <list>)); end method,
                   args(fact))
           | any?(variable-p, args(fact))
           | ~ member?(predicate(fact), *primitives*))
    error("Ill-formed fact: %S", fact);
  elseif (~ fact-present-p(fact))
    index(fact);
    run-attached-fn(fact);
  end if;
  #t;
end method add-fact;

define method fact-present-p (fact)
  // Is this fact present in the data base?
  retrieve(fact);
end method fact-present-p;

//  ==============================
define method run-attached-fn (fact)
  // Run the function associated with the predicate of this fact.
  apply(symbol-get-property(predicate(fact), #"attached-fn"), args(fact));
end method run-attached-fn;

//  ==============================
define method index-new-fact (fact)
  // Index the fact in the data base unless it is already there.
  if (~ fact-present-p(fact)) index(fact); end if;
end method index-new-fact;

//  ==============================
define method test-bears ()
  clear-dtrees();
  begin
    do(add-fact,
       #(#(#"sub", #"animal", #"living-thing"),
         #(#"sub", #"living-thing", #"thing"),
         #(#"sub", #"polar-bear", #"bear"), #(#"sub", #"grizzly", #"bear"),
         #(#"ind", #"yogi", #"bear"), #(#"ind", #"lars", #"polar-bear"),
         #(#"ind", #"helga", #"grizzly")));
    #(#(#"sub", #"animal", #"living-thing"),
      #(#"sub", #"living-thing", #"thing"), #(#"sub", #"polar-bear", #"bear"),
      #(#"sub", #"grizzly", #"bear"), #(#"ind", #"yogi", #"bear"),
      #(#"ind", #"lars", #"polar-bear"), #(#"ind", #"helga", #"grizzly"));
  end;
  // LTD: Function TRACE not yet implemented.
  trace(index);
  add-fact(#(#"sub", #"bear", #"animal"));
  ensure-untrace-1(#(#"index"));
end method test-bears;

// LTD: No macros.
#"a";

// LTD: No macros.
#"each";

// LTD: No macros.
#"??";

//  ==============================
define method translate-exp (exp, #key query-mode-p)
  // Translate exp into a conjunction of the four primitives.
  let conjuncts = #f;
  local method collect-fact (#rest terms)
          push!(terms, conjuncts);
        end method collect-fact,
        method translate (exp)
          //  Figure out what kind of expression this is
          if (not(instance?(exp, <list>)))
            exp;
          elseif (first(exp) == #"a")
            translate-a(tail(exp));
          elseif (first(exp) == #"each")
            translate-each(tail(exp));
          else
            apply(collect-fact, exp);
            exp;
          end if;
        end method translate,
        method translate-a (args)
          let category = pop!(args);
          let self
              = if (args & not(instance?(first(args), <list>)))
                  pop!(args);
                elseif (query-mode-p)
                  generate-symbol(#"string"("?"));
                else
                  generate-symbol(#"string"(as(<string>, category)));
                end if;
          collect-fact(#"ind", self, category);
          for (slot in args) translate-slot(#"val", self, slot); end for;
          self;
        end method translate-a,
        method translate-each (args)
          let category = pop!(args);
          if (predicate(first(args)) == #"isa")
            for (super in tail(pop!(args)))
              collect-fact(#"sub", category, super);
            end for;
          end if;
          for (slot in args) translate-slot(#"rel", category, slot); end for;
          category;
        end method translate-each,
        method translate-slot (primitive, self, slot)
          //  translate (relation value) into a REL or SUB
          assert(size(slot) = 2);
          collect-fact(primitive, first(slot), self, translate(second(slot)));
        end method translate-slot;
  //  Body of translate-exp:
  translate(exp);
  //  Build up the list of conjuncts
  maybe-add(#"and", reverse!(conjuncts));
end method translate-exp;

//  ==============================
define method replace-?-vars (exp)
  // Replace each ? in exp with a temporary var: ?123
  if (exp == #"?")
    generate-symbol(#"string"("?"));
  elseif (not(instance?(exp, <list>)))
    exp;
  else
    reuse-cons(replace-?-vars(first(exp)), replace-?-vars(tail(exp)), exp);
  end if;
end method replace-?-vars;

//  ==============================
// The current world used by index and fetch.
define variable *world* = #"w0";

define method index (key, #key world = *world*)
  // Store key in a dtree node.  Key must be (predicate . args);
  //   it is stored in the dtree, indexed by the world.
  dtree-index(key, key, world, get-dtree(predicate(key)));
end method index;

define method dtree-index (key, value, world, dtree)
  // Index value under all atoms of key in dtree.
  let _that = #f;
  if (instance?(key, <pair>))
    //  index on both first and rest
    dtree-index(first(key), value, world,
                dtree-first(dtree) | (dtree-first(dtree) := make-dtree()));
    dtree-index(tail(key), value, world,
                dtree-rest(dtree) | (dtree-rest(dtree) := make-dtree()));
  elseif (_that := empty?(key))
    _that;
    //  don't index on nil
    elseif (variable-p(key))
    //  index a variable
    nalist-push(world, value, dtree-var(dtree));
  else
    //  Make sure there is an nlist for this atom, and add to it
    nalist-push(world, value, lookup-atom(key, dtree));
  end if;
end method dtree-index;

//  ==============================
define method nalist-push (key, val, nalist)
  // Index val under key in a numbered alist.
  //  An nalist is of the form (count (key val*)*)
  //  Ex: (6 (nums 1 2 3) (letters a b c))
  inc!(head(nalist));
  let pair = cl-assoc(key, tail(nalist));
  if (pair)
    push!(val, tail(pair));
  else
    push!(list(key, val), tail(nalist));
  end if;
end method nalist-push;

//  ==============================
define class <world> (<object>)
  slot world-name, init-keyword: #"world-name";
  slot world-parents, init-keyword: #"world-parents";
  slot world-current, init-keyword: #"world-current";
end class <world>;

//  ==============================
define method get-world (name, #key current, parents = list(*world*))
  // Look up or create the world with this name.
  //   If the world is new, give it the list of parents.
  let _that = #f;
  if (world-p(name))
    name;
  elseif (_that := symbol-get-property(name, #"world"))
    _that;
  else
    symbol-get-property(name, #"world")
     := make-world(name: name, parents: parents, current: current);
  end if;
end method get-world;

*world* := get-world(#"w0", #f, #f);

//  ==============================
define method use-world (world)
  // Make this world current.
  //  If passed a name, look up the world it names
  world := get-world(world);
  if (~ (world == *world*))
    //  Turn the old world(s) off and the new one(s) on,
    //  unless we are already using the new world
    set-world-current(*world*, #f);
    set-world-current(world, #t);
    *world* := world;
  end if;
end method use-world;

define method use-new-world ()
  // Make up a new world and use it.
  //   The world inherits from the current world.
  *world* := get-world(generate-symbol(#"string"("W")));
  *world*.world-current := #t;
  *world*;
end method use-new-world;

define method set-world-current (world, on/off)
  // Set the current field of world and its parents on or off.
  //  nil is off, anything else is on.
  world.world-current := on/off;
  for (parent in world.world-parents)
    set-world-current(parent, on/off);
  end for;
end method set-world-current;

//  ==============================
define method print-world (world, #key stream = #t, depth)
  print(world.world-name, stream);
end method print-world;

//  ==============================
define method mapc-retrieve-in-world (fn, query)
  // For every fact in the current world that matches the query,
  //   apply the function to the binding list.
  for (bucket in fetch(query))
    for (world/entries in bucket)
      if (world-current(first(world/entries)))
        for (answer in tail(world/entries))
          let bindings = unify(query, answer);
          if (~ (bindings == fail)) fn(bindings); end if;
        end for;
      end if;
    end for;
  end for;
end method mapc-retrieve-in-world;

define method retrieve-in-world (query)
  // Find all facts that match query.  Return a list of bindings.
  let answers = #f;
  mapc-retrieve-in-world(method (bindings)
                           push!(bindings, answers);
                         end method,
                         query);
  answers;
end method retrieve-in-world;

define method retrieve-bagof-in-world (query)
  // Find all facts in the current world that match query.
  //   Return a list of queries with bindings filled in.
  map(method (bindings) subst-bindings(bindings, query); end method,
      retrieve-in-world(query));
end method retrieve-bagof-in-world;

//  ==============================
define method nlist-delete (item, nlist)
  // Remove an element from an nlist.
  //   Assumes that item is present exactly once.
  dec!(head(nlist));
  tail(nlist) := remove!(tail(nlist), item, count: 1);
  nlist;
end method nlist-delete;

//  ==============================
//  The attached functions:
def-attached-fn(ind, individual(category),
                //  Cache facts about inherited categories
                query-bind(?super(),
                           apply(list, #"sub", category, #(#"?super")),
                           add-fact(list(#"ind", individual, ?super))));

def-attached-fn(val, relation(ind1, ind2),
                //  Make sure the individuals are the right kinds
                query-bind(?cat1(?cat2),
                           apply(list, #"rel", relation,
                                 #(#"?cat1", #"?cat2")),
                           add-fact(list(#"ind", ind1, ?cat1)),
                           add-fact(list(#"ind", ind2, ?cat2))));

def-attached-fn(rel, relation(cat1, cat2),
                //  Run attached function for any IND's of this relation
                query-bind(?a(?b),
                           apply(list, #"ind", relation, #(#"?a", #"?b")),
                           run-attached-fn(list(#"ind", relation, ?a, ?b))));

def-attached-fn(sub, subcat(supercat), //  Cache SUB facts
                query-bind(?super-super(),
                           apply(list, #"sub", supercat, #(#"?super-super")),
                           index-new-fact(list(#"sub", subcat, ?super-super)),
                           query-bind(?sub-sub(),
                                      list(#"sub", #"?sub-sub", subcat),
                                      index-new-fact(list(#"sub",
                                                          ?sub-sub,
                                                          ?super-super)))),
                query-bind(?sub-sub(), list(#"sub", #"?sub-sub", subcat),
                           index-new-fact(list(#"sub", ?sub-sub, supercat))),
                //  Cache IND facts
                query-bind(?super-super(),
                           apply(list, #"sub", subcat, #(#"?super-super")),
                           query-bind(?sub-sub(),
                                      list(#"sub", #"?sub-sub", supercat),
                                      query-bind(?ind(),
                                                 list(#"ind",
                                                      #"?ind",
                                                      ?sub-sub),
                                                 index-new-fact(list(#"ind",
                                                                     ?ind,
                                                                     ?super-super))))));

