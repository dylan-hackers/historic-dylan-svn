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

define class <cons> (<object>)
  constant slot car :: <object>,
    required-init-keyword: car:;
  constant slot cdr :: <object>,
    required-init-keyword: cdr:;
end class <cons>;

define method cons (car :: <object>, cdr :: <object>) => <cons>;
  make(<cons>, car: car, cdr: cdr);
end method cons;

define method \= (a1 :: <cons>, a2 :: <cons>) => (b :: <boolean>)
  a1.car = a2.car & a1.cdr = a2.cdr
end method;

define method equal-hash (a :: <cons>, s :: <hash-state>) =>
    (i :: <integer>, s* :: <hash-state>)
  values-hash(equal-hash, s, a.car, a.cdr)
end method equal-hash;

define constant $cache = make(<equal-table>);

define constant $not-memoized = #"Not memoized";

define class <no-path-error> (<error>)
  slot start :: <point>,
    required-init-keyword: start:;
  slot finish :: <point>,
    required-init-keyword: finish:;
end class <no-path-error>;

define function path-length (p1 :: <point>, p2 :: <point>, b :: <board>)
 => (len :: false-or(<integer>))
  let a = cons(p1, p2);
  let dist = element($cache, a, default: $not-memoized);
  when (dist = $not-memoized)
    let path = find-path(p1, p2, b);
    if (path)
      $cache[a] := path.size;
    else
      $cache[a] := #f;
    end if;
  end when;
  //
  $cache[a];
end function path-length;

// Now that we have path-length, we will implement Prim's algorithm
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
  let s = make(<disjoint-set>, value: o, set-rank: 0);
  s.parent := s;
  s
end method make-set;

define method find-set (s :: <disjoint-set>) => <disjoint-set>;
  when (s ~== parent)
    s.parent := s.parent.find-set
  end when;
  //
  s.parent
end method find-set;

define method link-set (x :: <disjoint-set>, y :: <disjoint-set>) => ()
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

define function all-edges (tgts :: <sequence>, b :: <board>) => <vector>;
  let n :: <integer> = tgts.size;
  let vec = make(<vector>, size: n * n, fill: #f);
  let i :: <integer> = 0;
  for (u :: <point> in tgts)
    for (v :: <point> in tgts)
      vec[i] := cons(u, v);
      i := i + 1;
    end for;
  end for;
  //
  local method cmp (u, v)
          let (us, uf) = values(u.car.find-set.value, u.cdr.find-set.value);
          let (vs, vf) = values(v.car.find-set.value, v.cdr.find-set.value);
          let d1 = path-length(us, uf, b);
          let d2 = path-length(vs, vf, b);
          //
          unless (d1) signal(make(<no-path-error>, start: us, finish: uf)) end;
          unless (d2) signal(make(<no-path-error>, start: vs, finish: vf)) end;
          //
          d1 < d2
        end method cmp;
  sort!(vec, test: cmp);
end function all-edges;

// This is Kruskal's algorithm

define method min-span-tree (tgts :: <sequence>, b :: <board>) => <table>;
  let a = make(<table>);
  let sets = map(make-set, tgts);
  for (arc-set in all-edges(tgts, b))
    let (u, v) = values(arc-set.car, arc-set.cdr);
    if (u.find-set ~== v.find-set)
      a[u.find-set.value] := pair(v.find-set.value,
                                  element(a, u.find-set.value, default: #()));
      set-union!(u, v);
    end if;
  finally
    a
  end for;
end method min-span-tree;

define method find-tour (start :: <point>, tgts :: <sequence>, b :: <board>)
 => (visit :: <sequence>)
  let t = min-span-tree(tgts, b);
  //
  let visits = make(<table>);
  let visited? = method (p) element(visits, p, default: #f) end;
  let mark = method (p) visits[p] := #t end;
  //
  // Do a depth-first-walk on the spanning tree.
  //
  iterate loop (acc = #(), node = start)
    node.mark;
    reduce(method (acc :: <list>, next :: <point>)
             if (next.visited?) acc else loop(acc, next) end if;
           end method,
           pair(node, acc),
           t[node])
  end iterate;
end method find-tour;