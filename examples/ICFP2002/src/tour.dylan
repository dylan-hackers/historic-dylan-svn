module: tour
synopsis: Solves the travelling salesman problem so we can find efficient 
          tours.

// It turns out that there is an efficient, near-optimal solution to the
// travelling salesman problem in our special case. This is because the
// board is "Euclidean" -- that is going from A -> B -> C will never be
// cheaper than going from A -> C.
//
// The basic trick is to to 
// 1. compute the minimum spanning tree for the graph
// 2. Then we can just use it.

// We want to cache the output of find-path, because package and base
// destinations will show up again and again, and we can avoid
// recomputation.

// Now that we have path-length, we will implement Kruskal's algorithm
// for building minimum spanning trees. 

define class <disjoint-set> (<object>)
  slot value :: <object>,
    required-init-keyword: value:;
  slot set-rank :: <integer>,
    required-init-keyword: set-rank:;
  slot parent :: <disjoint-set>,
    init-keyword: parent:;
end class <disjoint-set>;

define method make-set (o :: <object>) => <disjoint-set>;
  debug("make-set(%=)\n", o);
  let s = make(<disjoint-set>, value: o, set-rank: 0);
  s.parent := s;
  s
end method make-set;

define method find-set (s :: <disjoint-set>) => <disjoint-set>;
  // debug("find-set(%=)\n", s);
  if (s ~= s.parent)
    s.parent := find-set(s.parent);
  end if;
  //
  s.parent
end method find-set;

define method link-set (x :: <disjoint-set>, y :: <disjoint-set>) => ()
  debug("link-set(%=, %=)\n", x, y);
  if (x.set-rank > y.set-rank)
    y.parent := x;
  else
    x.parent := y;
    if (x.set-rank = y.set-rank)
      y.set-rank := y.set-rank + 1;
    end if;
  end if;
end method link-set;

define method set-union! (s1 :: <disjoint-set>, s2 :: <disjoint-set>) => ()
  link-set(s1.find-set, s2.find-set)
end method set-union!;

define class <no-path-error> (<error>)
  slot path-start :: <point>,
    required-init-keyword: path-start:;
  slot path-finish :: <point>,
    required-init-keyword: path-finish:;
end class <no-path-error>;

define function no-path-error (start :: <point>, finish :: <point>)
  debug("no-path-error(%=, %=)\n", start, finish);
  signal(make(<no-path-error>,
              path-start: start,
              path-finish: finish));
end function no-path-error;

define function all-edges (tgts :: <vector>, b :: <board>) => <vector>;
  debug("all-edges(%=, {board})\n", tgts);
  let n :: <integer> = tgts.size;

  let vec = make(<vector>, size: truncate/((n - 1) * n, 2), fill: #f);
  let k :: <integer> = 0;
  for (i from 0 below n)
    for (j from i + 1 below n)
      vec[k] := cons(tgts[i], tgts[j]);
      k := k + 1;
    end for;
  end for;
  //
  local method cmp (u, v)
          let (us, uf) = values(u.car.find-set.value, u.cdr.find-set.value);
          let (vs, vf) = values(v.car.find-set.value, v.cdr.find-set.value);
          let d1 = path-length(us, uf, b);
          let d2 = path-length(vs, vf, b);
          //
          unless (d1) no-path-error(us, uf) end;
          unless (d2) no-path-error(vs, vf) end;
          //
          d1 < d2
        end method cmp;
  debug("About to sort\n");
  let v2 = sort!(vec, test: cmp);
  debug("Sorted!\n");
  v2
end function all-edges;

// This is Kruskal's algorithm

define method min-span-tree (tgts :: <sequence>, b :: <board>) => <table>;
  debug("min-span-tree(%=, {board})\n", tgts);
  let a = make(<table>);
  let tgts = map-as(<vector>, make-set, tgts);
  for (arc-set in all-edges(tgts, b))
    let (u, v) = values(arc-set.car, arc-set.cdr);
    if (u.find-set ~== v.find-set)
      a[u.find-set.value] := pair(v.value,
                                  element(a, u.find-set.value, default: #()));
      a[v.find-set.value] := pair(u.value,
                                  element(a, v.find-set.value, default: #()));
      set-union!(u, v);
    end if;
  finally
    a
  end for;
end method min-span-tree;

define method base-tour (start :: <point>, tgts :: <sequence>, b :: <board>)
 => (visit :: false-or(<sequence>))
  debug("find-tour(%=, %=, {board})\n", start, tgts);
  let t = min-span-tree(tgts, b);
  debug("find-tour: t = %=\n", t);
  //
  let visits = make(<table>);
  let visited? = method (p :: <point>) element(visits, p, default: #f) end;
  let mark = method (p :: <point>) => () visits[p] := #t end;
  //
  // Do a depth-first-walk on the spanning tree.
  //
  iterate loop (acc = make(<stretchy-vector>), node = start)
    debug("loop(%=, %=)\n", acc, node);
    node.mark;
    reduce(method (acc :: <stretchy-vector>, next :: <point>)
             if (next.visited?) acc else loop(acc, next) end if;
           end method,
           add!(acc, node),
           element(t, node, default: #()))
  end iterate;
end method base-tour;

define method tour-length (tour :: <sequence>, b :: <board>) => (<integer>)
  let start = tour.first;
  let len :: <integer> = 0;
  for (dest in subsequence(tour, start: 1))
    len := len + path-length(start, dest, b);
    start := dest;
  finally
    len
  end for;
end method tour-length;

define method two-opt! (tour :: <vector>, b :: <board>) => ()
  for (i :: <integer> from 0 below tour.size - 1)
    for (j :: <integer> from 0 below tour.size - 1)
      if (i ~= j)
        let len = tour-length(tour, b);
        let (t, u) = values(tour[i], tour[i + 1]);
        let (v, w) = values(tour[j], tour[j + 1]);
        tour[i + 1] := w;
        tour[j + 1] := u;
        if (len < tour-length(tour, b))
          tour[i + 1] := u;
          tour[j + 1] := w;
        end if;
      end if;
    end for;
  end for;
end method two-opt!;

define method find-tour (start :: <point>, tgts :: <sequence>, b :: <board>)
 => (visit :: false-or(<sequence>))
  let tour = base-tour(start, tgts, b);
  two-opt!(tour, b);
  tour
end method find-tour;

// Print-object methods

define method print-object (set :: <disjoint-set>, s :: <stream>) => ()
  if (set == set.parent)
    format(s, "{disjoint-set value: %=, set-rank: %d, parent: <self>}",
           set.value, set.set-rank);
  else
    format(s, "{disjoint-set value: %=, set-rank: %d, parent: %=}",
           set.value, set.set-rank, set.parent);
  end if;
end method print-object;