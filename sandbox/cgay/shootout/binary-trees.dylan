module: binary-trees

define constant <tree> = type-union(<integer>, <node>);

define class <node> (<object>)
  constant slot left :: <tree>, required-init-keyword: left:;
  constant slot item :: <integer>, required-init-keyword: item:;
  constant slot right :: <tree>, required-init-keyword: right:;
end;

define sealed domain make(singleton(<node>));
define sealed domain initialize(<node>);

define function build
    (n :: <integer>, d :: <integer>) => (tree :: <tree>)
  if (d == 0)
    n
  else
    make(<node>,
         item: n,
         left: build(2 * n - 1, d - 1),
         right: build(2 * n, d - 1))
  end
end function build;

define function check
    (tree :: <tree>) => (i :: <integer>)
  select (tree by instance?)
    <integer> => tree;
    <node> => tree.item + tree.left.check - tree.right.check;
  end
end function check;

begin
  let min-depth = 4;
  let max-depth = max(min-depth + 2,
                      string-to-integer(application-arguments()[0]));
  let stretch-depth = max-depth + 1;

  format-out("stretch tree of depth %d\t check: %d\n",
             stretch-depth, check(build(0, stretch-depth)));

  let long-lived-tree = build(0, max-depth);

  for (d from min-depth to max-depth by 2)
    let iterations = ash(1, max-depth - d + min-depth);
    for (i from 1 to iterations,
         c = 0 then c + check(build(i, d)) + check(build(-i, d)))
    finally
      format-out("%d\t trees of depth %d\t check: %d\n",
                 2 * iterations, d, c);
    end for;
  end for;

  format-out("long lived tree of depth %d\t check: %d\n",
             max-depth, long-lived-tree.check);
end;
