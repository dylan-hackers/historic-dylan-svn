//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  search.lisp: Search routines from section 6.4
define method tree-search (states, goal-p, successors, combiner)
  // Find a state that satisfies goal-p.  Start with states,
  //   and search according to successors and combiner.
  dbg(search: "~&;; Search: ~a", states);
  if (empty?(states))
    fail;
  elseif (goal-p(first(states)))
    first(states);
  else
    tree-search(combiner(successors(first(states)), tail(states)), goal-p,
                successors, combiner);
  end if;
end method tree-search;

define method depth-first-search (start, goal-p, successors)
  // Search new states first until goal is reached.
  tree-search(list(start), goal-p, successors, concatenate);
end method depth-first-search;

define method binary-tree (x) list(2 * x, 1 + 2 * x); end method binary-tree;

define method is (value) method (x) x == value; end method; end method is;

define method prepend (x, y)
  // Prepend y to start of x
  concatenate(y, x);
end method prepend;

define method breadth-first-search (start, goal-p, successors)
  // Search old states first until goal is reached.
  tree-search(list(start), goal-p, successors, prepend);
end method breadth-first-search;

define method finite-binary-tree (n)
  // Return a successor function that generates a binary tree
  //   with n nodes.
  method (x)
    choose(complement(method (child) child > n; end method), binary-tree(x));
  end method;
end method finite-binary-tree;

define method diff (num)
  // Return the function that finds the difference from num.
  method (x) abs(x - num); end method;
end method diff;

define method sorter (cost-fn)
  // Return a combiner function that sorts according to cost-fn.
  method (new, old)
    sort!(concatenate(new, old),
          test: method (x, y) cost-fn(x) < cost-fn(y); end method);
  end method;
end method sorter;

define method best-first-search (start, goal-p, successors, cost-fn)
  // Search lowest cost states first until goal is reached.
  tree-search(list(start), goal-p, successors, sorter(cost-fn));
end method best-first-search;

define method price-is-right (price)
  // Return a function that measures the difference from price,
  //   but gives a big penalty for going over price.
  method (x)
    if (x > price) $most-positive-fixnum; else price - x; end if;
  end method;
end method price-is-right;

define method beam-search (start, goal-p, successors, cost-fn, beam-width)
  // Search highest scoring states first until goal is reached,
  //   but never consider more than beam-width states at a time.
  tree-search(list(start), goal-p, successors,
              method (old, new)
                let sorted = (sorter(cost-fn))(old, new);
                if (beam-width > size(sorted))
                  sorted;
                else
                  copy-sequence(sorted, 0, beam-width);
                end if;
              end method);
end method beam-search;

define class <city> (<object>)
  slot city-name, init-keyword: #"city-name";
  slot city-long, init-keyword: #"city-long";
  slot city-lat, init-keyword: #"city-lat";
end class <city>;

define variable *cities* =
  #(#(#"atlanta", 84.23, 33.45), #(#"los-angeles", 118.15, 34.03),
    #(#"boston", 71.05, 42.21), #(#"memphis", 90.03, 35.09),
    #(#"chicago", 87.37, 41.5), #(#"new-york", 73.58, 40.47),
    #(#"denver", 105.0, 39.45), #(#"oklahoma-city", 97.28, 35.26),
    #(#"eugene", 123.05, 44.03), #(#"pittsburgh", 79.57, 40.27),
    #(#"flagstaff", 111.41, 35.13), #(#"quebec", 71.11, 46.49),
    #(#"grand-jct", 108.37, 39.05), #(#"reno", 119.49, 39.3),
    #(#"houston", 105.0, 34.0), #(#"san-francisco", 122.26, 37.47),
    #(#"indianapolis", 86.1, 39.46), #(#"tampa", 82.27, 27.57),
    #(#"jacksonville", 81.4, 30.22), #(#"victoria", 123.21, 48.25),
    #(#"kansas-city", 94.35, 39.06), #(#"wilmington", 77.57, 34.14));

define method neighbors (city)
  // Find all cities within 1000 kilometers.
  find-all-if(method (c)
                ~ (c == city) & air-distance(c, city) < 1000.0;
              end method,
              *cities*);
end method neighbors;

define method city (name)
  // Find the city with this name.
  cl-assoc(name, *cities*);
end method city;

define method trip (start, dest)
  // Search for a way from the start to dest.
  beam-search(start, is(dest), neighbors,
              method (c) air-distance(c, dest); end method, 1);
end method trip;

define class <path> (<object>)
  slot path-state, init-keyword: #"path-state";
  slot path-previous = #f, init-keyword: #"path-previous";
  slot path-cost-so-far = 0, init-keyword: #"path-cost-so-far";
  slot path-total-cost = 0, init-keyword: #"path-total-cost";
end class <path>;

define method trip (start, dest, #key beam-width = 1)
  // Search for the best path from the start to dest.
  beam-search(make-path(state: start), is(dest, key: path-state),
              path-saver(neighbors, air-distance,
                         method (c) air-distance(c, dest); end method),
              path-total-cost, beam-width);
end method trip;

// Diameter of planet earth in kilometers.
define constant earth-diameter = 12765.0;

define method air-distance (city1, city2)
  // The great circle distance between two cities.
  let d = distance(xyz-coords(city1), xyz-coords(city2));
  //  d is the straight-line chord between the two cities,
  //  The length of the subtending arc is given by:
  earth-diameter * asin((d / 2));
end method air-distance;

define method xyz-coords (city)
  // Returns the x,y,z coordinates of a point on a sphere.
  //   The center is (0 0 0) and the north pole is (0 0 1).
  let psi = deg->radians(city.city-lat);
  let phi = deg->radians(city.city-long);
  list(cos(psi) * cos(phi), cos(psi) * sin(phi), sin(psi));
end method xyz-coords;

define method distance (point1, point2)
  // The Euclidean distance between two points.
  //   The points are coordinates in n-dimensional space.
  sqrt(reduce1(\+,
               map(method (a, b) (a - b) ^ 2; end method, point1, point2)));
end method distance;

define method deg->radians (deg)
  // Convert degrees and minutes to radians.
  (truncate(deg) + remainder(deg, 1) * 5/3) * $pi * 1/180;
end method deg->radians;

define method is (value, #key key = identity, test = \==)
  // Returns a predicate that tests for a given value.
  method (path) test(value, key(path)); end method;
end method is;

define method path-saver (successors, cost-fn, cost-left-fn)
  method (old-path)
    let old-state = old-path.path-state;
    map(method (new-state)
          let old-cost
              = old-path.path-cost-so-far + cost-fn(old-state, new-state);
          make-path(state: new-state, previous: old-path,
                    cost-so-far: old-cost,
                    total-cost: old-cost + cost-left-fn(new-state));
        end method,
        successors(old-state));
  end method;
end method path-saver;

define method print-path (path, #key stream = #t, depth)
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               write-string++("#<Path to ", xp, 0, 10);
               fluid-bind (*print-escape* = #f)
                 write+(pop!(args), xp);
               end fluid-bind;
               write-string++(" cost ", xp, 0, 6);
               using-format(xp, "~,1f", pop!(args));
               write-char++('>', xp);
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(stream, path.path-state, path.path-total-cost);
end method print-path;

define method show-city-path (path, #key stream = #t)
  // Show the length of a path, and the cities along it.
  (method (s, #rest args)
     block (return)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               block (return)
                 block (return)
                   write-string++("#<Path ", xp, 0, 7);
                   using-format(xp, "~,1f", pop!(args));
                   write-string++(" km: ", xp, 0, 5);
                   let args = pop!(args);
                   block (return)
                     block (return)
                       block (return)
                         local method go-l ()
                                 if (empty?(args)) return(#f); end if;
                                 begin
                                   push-char-mode(xp, #"cap1");
                                   fluid-bind (*print-escape* = #f)
                                     write+(pop!(args), xp);
                                   end fluid-bind;
                                   pop-char-mode(xp);
                                 end;
                                 if (empty?(args))
                                   return-from-nil(#f);
                                 end if;
                                 write-string++(" - ", xp, 0, 3);
                                 go-l();
                               end method go-l;
                         go-l();
                       end block;
                     end block;
                   end block;
                   write-char++('>', xp);
                 end block;
                 if (args) copy-sequence(args); end if;
               end block;
             end method,
             s, args);
     end block;
   end method)(stream, path.path-total-cost,
               reverse(map-path(city-name, path)));
  values();
end method show-city-path;

define method map-path (fn, path)
  // Call fn on each state in the path, collecting results.
  if (empty?(path))
    #f;
  else
    pair(fn(path.path-state), map-path(fn, path.path-previous));
  end if;
end method map-path;

define method iter-wide-search (start, goal-p, successors, cost-fn,
                                #key width = 1, max = 100)
  // Search, increasing beam width from width to max.
  //   Return the first solution found at any width.
  dbg(search: "; Width: ~d", width);
  if (~ (width > max))
    beam-search(start, goal-p, successors, cost-fn, width)
     | iter-wide-search(start, goal-p, successors, cost-fn, width: width + 1,
                        max: max);
  end if;
end method iter-wide-search;

define method graph-search (states, goal-p, successors, combiner,
                            #key state= = \==, old-states)
  // Find a state that satisfies goal-p.  Start with states,
  //   and search according to successors and combiner.  
  //   Don't try the same state twice.
  dbg(search: "~&;; Search: ~a", states);
  if (empty?(states))
    fail;
  elseif (goal-p(first(states)))
    first(states);
  else
    graph-search(combiner(new-states(states, successors, state=, old-states),
                          tail(states)),
                 goal-p, successors, combiner, state=,
                 add!(first(states), old-states, test: state=));
  end if;
end method graph-search;

define method new-states (states, successors, state=, old-states)
  // Generate successor states that have not been seen before.
  choose(complement(method (state)
                      member?(state, states, test: state=)
                       | member?(state, old-states, test: state=);
                    end method),
         successors(first(states)));
end method new-states;

define method next2 (x) list(x + 1, x + 2); end method next2;

define method a*-search (paths, goal-p, successors, cost-fn, cost-left-fn,
                         #key state= = \==, old-paths)
  // Find a path whose state satisfies goal-p.  Start with paths,
  //   and expand successors, exploring least cost first.
  //   When there are duplicate states, keep the one with the
  //   lower cost and discard the other.
  dbg(search: ";; Search: ~a", paths);
  if (empty?(paths))
    fail;
  elseif (goal-p(path-state(first(paths))))
    values(first(paths), paths);
  else
    begin
      let path = pop!(paths);
      let state = path.path-state;
      //  Update PATHS and OLD-PATHS to reflect
      //  the new successors of STATE:
      old-paths := insert-path(path, old-paths);
      for (state2 in successors(state))
        let cost = path.path-cost-so-far + cost-fn(state, state2);
        let cost2 = cost-left-fn(state2);
        let path2
            = make-path(state: state2, previous: path, cost-so-far: cost,
                        total-cost: cost + cost2);
        let old = #f;
        //  Place the new path, path2, in the right list:
        if (old := find-path(state2, paths, state=))
          if (better-path(path2, old))
            paths := insert-path(path2, remove!(paths, old));
          end if;
        elseif (old := find-path(state2, old-paths, state=))
          if (better-path(path2, old))
            paths := insert-path(path2, paths);
            old-paths := remove!(old-paths, old);
          end if;
        else
          paths := insert-path(path2, paths);
        end if;
      end for;
      //  Finally, call A* again with the updated path lists:
      a*-search(paths, goal-p, successors, cost-fn, cost-left-fn, state=,
                old-paths);
    end;
  end if;
end method a*-search;

define method find-path (state, paths, state=)
  // Find the path with this state among a list of paths.
  cl-find(state, paths, key: path-state, test: state=);
end method find-path;

define method better-path (path1, path2)
  // Is path1 cheaper than path2?
  path1.path-total-cost < path2.path-total-cost;
end method better-path;

define method insert-path (path, paths)
  // Put path into the right position, sorted by total cost.
  //  MERGE is a built-in function
  cl-merge(<list>, list(path), paths, compose(\<, path-total-cost));
end method insert-path;

define method path-states (path)
  // Collect the states along this path.
  if (empty?(path))
    #f;
  else
    pair(path.path-state, path-states(path.path-previous));
  end if;
end method path-states;

define method search-all (start, goal-p, successors, cost-fn, beam-width)
  // Find all solutions to a search problem, using beam search.
  let solutions = #f;
  beam-search(start,
              method (x)
                if (goal-p(x)) push!(x, solutions); end if;
                #f;
              end method,
              successors, cost-fn, beam-width);
  solutions;
end method search-all;

