module: test
synopsis: 
author: 
copyright: 

define method force-out(s :: <string>, #rest args)
  apply(format-out, s, args);
  force-output(*standard-output*);
end method force-out;

define method make-tree(n :: <integer>) => (t :: <rb-tree>)
  reduce(method(t, i) => (new-t)
	     insert(t, i, i * i)
	 end method,
	 make(<rb-tree>),
	 range(from: 1, to: n))
end method make-tree;

define method test-display()
  force-out("test-display:\n");
  force-out("  Making tree...\n");
  let t = make-tree(10);
  force-out("  Calling display...\n");
  display(t);
  force-out("\n");
end method test-display;
	  
define method test-forward-iteration-protocol()
  force-out("test-forward-iteration-protocol:\n");
  for (v keyed-by k in make-tree(10))
    force-out("    %=: %=\n", k, v);
  end for;
end method test-forward-iteration-protocol;

define method test-backward-iteration-protocol()
  force-out("test-backward-iteration-protocol:\n");
  for (v keyed-by k in make-tree(10) using backward-iteration-protocol)
    force-out("    %=: %=\n", k, v);
  end for;
end method test-backward-iteration-protocol;

define method test-element()
  force-out("test-element:\n");
  let t = reduce(method(t, i)
		     insert(t, i, make(<string>, size: i, fill: 'x'))
		 end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  for (i in list(3, 7, 4))
    force-out("    %d: %s\n", i, t[i]);
  end for;
end method test-element;

define method test-size()
  force-out("test-size:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  force-out("    size = %d\n", t.size)
end method test-size;

define method test-delete()
  force-out("test-delete:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  t.display;
  let s = reduce(delete, t, list(3, 7, 4, 8));
  s.display;
end method test-delete;
 
define method test-min-key()
  force-out("test-min-key:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  force-out("    %=\n", t.min-key);
end method test-min-key;

define method test-max-key()
  force-out("test-max-key:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  force-out("    %=\n", t.max-key);
end method test-max-key;

define method test-successor()
  force-out("test-successor:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  for (i from t.min-key to t.max-key)
    block()
      force-out("    key = %=, succ = %=\n", i, successor(t, i));
    exception(e :: <condition>)
      force-out("    %s\n", e.condition-format-string)
    end block;
  end for;
end method test-successor;

define method test-predecessor()
  force-out("test-predecessor:\n");
  let t = reduce(method(t, i) insert(t, i, i + i) end,
		 make(<rb-tree>),
		 range(from: 1, to: 10));
  for (i from t.min-key to t.max-key)
    block()
      force-out("    key = %=, pred = %=\n", i, predecessor(t, i));
    exception(e :: <condition>)
      force-out("    %s\n", e.condition-format-string);
    end block;
  end for;
end method test-predecessor;

// Test of a new <rb-tree> class.

define class <string-tree> (<rb-tree>)
end class <string-tree>;

define method order (t :: <string-tree>) => (f :: <function>)
  method (s1 :: <string>, s2 :: <string>) => (b :: <boolean>)
    s1.as-lowercase < s2.as-lowercase
  end method;
end method order;

define method type-for-copy (t :: <string-tree>) => (c :: <class>)
  <case-insensitive-string-table>
end method type-for-copy;

//

define method make-string-tree(#rest strings) => (t :: <string-tree>)
  let i :: <integer> = 0;
  reduce(method (t :: <string-tree>, s :: <string>)
	   insert(t, s, i := i + 1)
	 end method,
	 make(<string-tree>),
	 strings)
end method make-string-tree;

define method test-string-tree-insert()
  force-out("test-string-tree-insert:\n");
  let t = make-string-tree("foo", "bar", "FOO", "foO", "baz");
  for (v keyed-by k in t)
    force-out("    %= -> %=\n", k, v);
  end for;
end method test-string-tree-insert;

define method test-string-tree-type-for-copy();
  force-out("test-string-tree-type-for-copy:\n");
  let t = make-string-tree("foo", "bar", "FOO", "foO", "baz");
  let u = t.shallow-copy;
  for (v keyed-by k in u)
    force-out("    %= -> %=\n", k, v);
  end for;
end method test-string-tree-type-for-copy;

define function main(name, arguments)
  test-display();
  test-forward-iteration-protocol();
  test-backward-iteration-protocol();
  test-size();
  test-element();
  test-delete();
  test-min-key();
  test-max-key();
  test-successor();
  test-predecessor();
  test-string-tree-insert();
  test-string-tree-type-for-copy();
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
