module: red-black
synopsis: An implementation of red-black trees in Dylan. 
author: Neel Krishnaswami <neelk@alum.mit.edu>
copyright: This source code file is in the public domain.
version: 0.7

// This module implements red-black trees. Red-black trees are a
// form of self-balancing binary search trees, which means that they
// permit one to do lookups in O(log N) worst-case time. They are
// very flexible data structures, and can be used as a mapping, as
// priority queue, and also store their members in sorted order.
// Even better, they only require a comparison operator on their keys,
// permitting their use when hash functions are hard to compute.
//
// These trees are augmented in two ways. First, they are augmented
// to store the size of a tree, making calculating a tree's size an
// O(1) operation.
// 
// Second, a not-yet (and perhaps never-will-be) implemented augmentation
// is to store the min and max nodes in the root, to make the priority
// queue operations min-key and max-key O(1) rather than O(lg N). This
// might not be implemented because it would increase the amount of
// data passed around on the stack in insert and delete operations,
// which would have negative consequences for locality. I'll wait to
// see if there's strong demand for it before putting it in. I'm over-
// using multiple values as it is. 
//
// Finally, this class is extremely well suited for use in multithreaded
// apps, since as a purely functional data structure it is inherently
// reentrant and thread-safe. There is no locking at all anywhere in
// the code. 

define open abstract class <rb-tree> (<explicit-key-collection>)
  constant sealed slot size :: <integer>,
    init-keyword: size:,
    init-value: 0;
  constant sealed slot tree :: <tree>,
    init-keyword: tree:,
    init-value: $nil;
end class <rb-tree>;

//////////////////////////////////////////////////
// The <rb-tree> extension protocol
//////////////////////////////////////////////////

define open generic order (t :: <rb-tree>) => (f :: <function>);

//////////////////////////////////////////////////
// The default concrete implementation of <rb-tree>; this is  
// what gets made when make(<rb-tree>) is called. 

define sealed primary class <lt-tree> (<rb-tree>)
end class <lt-tree>;

define method order (t :: <lt-tree>) => (f :: <function>)
  \<
end method order;

define method type-for-copy (t :: <lt-tree>) => (type :: <type>)
  <equal-table>
end method type-for-copy;

define sealed domain make(<lt-tree>.singleton);
define sealed domain initialize(<lt-tree>);
define sealed domain order(<lt-tree>);

//////////////////////////////////////////////////
// Default make on <rb-tree>
//////////////////////////////////////////////////

define method make(class == <rb-tree>, #key) => (o :: <lt-tree>)
  make(<lt-tree>, size: 0, tree: $nil)
end method make;

// Theoretically, this could be one-of(#"red", #"black"), but using
// a limited integer reduces the size of the allocated <node>s. Saving
// 8 bytes per <node> seems worthwhile to me, especially since this is
// wholly internal to the <rb-tree> implementation.

define constant <color> = limited(<integer>, min: 0, max: 1);
define constant $red :: <color> = 0;
define constant $black :: <color> = 1;

define sealed abstract primary class <tree> (<object>)
  constant sealed slot color :: <color>,
    required-init-keyword: color:;
end class <tree>;

define sealed primary class <leaf> (<tree>)
  inherited slot color, init-value: $black;
end class <leaf>;

define constant $nil = make(<leaf>);

define sealed primary class <node> (<tree>)
  constant sealed slot key :: <object>,
    required-init-keyword: key:;
  constant sealed slot value :: <object>,
    required-init-keyword: value:;
  constant sealed slot left :: <tree>,
    required-init-keyword: left:;
  constant sealed slot right :: <tree>,
    required-init-keyword: right:;
end class <node>;

// These seals permit avoiding a GF dispatch when creating a new
// object. It's superfluous for <leaf>, since that's being handled
// as a Flyweight, but I prefer to remain consistent.

define sealed domain make(<rb-tree>.singleton);
define sealed domain make(<leaf>.singleton);
define sealed domain make(<node>.singleton);
define sealed domain initialize(<rb-tree>);
define sealed domain initialize(<leaf>);
define sealed domain initialize(<node>);
define sealed domain size(<rb-tree>);

//////////////////////////////////////////////////
// Private unexported utility methods
//////////////////////////////////////////////////

define sealed method node(c :: <color>, k :: <object>, v :: <object>,
			  lft :: <tree>, rgt :: <tree>)
 => (n :: <node>)
  make(<node>, color: c, key: k, value: v, left: lft, right: rgt)
end method node;

define generic leaf? (t :: <tree>) => (b :: <boolean>);

// This method is a performance hack. It should be two methods
// returning #t and #f respectively, but GD doesn't want to optimize
// that very well. So I refer to the class directly. Bad Neel! :)

define sealed inline method leaf? (t :: <tree>) => (b :: <boolean>)
  instance?(t, <leaf>) 
end method leaf?;

define constant nil? = leaf?;

define sealed method red? (t :: <tree>) => (b :: <boolean>)
  t.color == $red
end method red?;

define sealed method black? (t :: <tree>) => (b :: <boolean>)
  t.color == $black
end method black?;

define sealed method blackify (t :: <tree>) => (new :: <tree>)
  if (t.black?)
    t
  else
    node($black, t.key, t.value, t.left, t.right)
  end if;
end method blackify;

//////////////////////////////////////////////////
// Insertion and deletion
//////////////////////////////////////////////////

// This insertion method is based on the very elegant algorithm described
// in Chris Okasaki's _Purely Functional Data Structures_. The algorithm
// is different from Okasaki in 2 ways. First, I augmented it to carry
// size, max and min information around. Second, I broke up the balance
// function to be smart about which half of the test it was in. This halved
// the number of comparisons each call to balance() needed. It also halved
// the stack space insert required. :)

define sealed method insert (t :: <rb-tree>, k :: <object>, v :: <object>)
 => (new :: <rb-tree>)
  let (new :: <tree>, inserted? :: <boolean>) = %ins(t.tree, k, v, t.order);
  let incr :: <integer> = if (inserted?) 1 else 0 end;
  make(t.object-class,
       size: t.size + incr,
       tree: new.blackify)
end method insert;

define sealed generic %ins (t :: <tree>, k :: <object>, v :: <object>,
			    \< :: <function>)
 => (new :: <node>, inserted? :: <boolean>);

define sealed method %ins (t :: <leaf>, k :: <object>, v :: <object>,
			   \< :: <function>)
 => (new :: <node>, inserted? :: <boolean>)
  values(node($red, k, v, $nil, $nil),
	 #t)
end method %ins;

define sealed method %ins (t :: <node>, k :: <object>, v :: <object>,
			   \< :: <function>)
 => (new :: <node>, inserted? :: <boolean>)
  if (k < t.key)
    let (new-lft :: <tree>, inserted? :: <boolean>) = %ins(t.left, k, v, \<);
    values(l-balance(t.color, t.key, t.value, new-lft, t.right),
	   inserted?)
  elseif (t.key < k)
    let (new-rgt :: <tree>, inserted? :: <boolean>) = %ins(t.right, k, v, \<);
    values(r-balance(t.color, t.key, t.value, t.left, new-rgt),
	   inserted?)
  else
    // In this case, the new key is equivalent to an existing one in
    // the tree. We replace it and the existing value with the new
    // key and value.
    //
    values(node(t.color, k, v, t.left, t.right),
	   #f)
  end if;
end method %ins;

define sealed method l-balance (c :: <color>, k, v,
				lft :: <tree>, rgt :: <tree>)
 => (new :: <tree>)
  if (c == $black)
    case
      lft.red? & lft.left.red?
	=> node($red, lft.key, lft.value,
		node($black, lft.left.key, lft.left.value,
		     lft.left.left, lft.left.right),
		node($black, k, v,
		     lft.right, rgt));
      lft.red? & lft.right.red?
	=> node($red, lft.right.key, lft.right.value,
		node($black, lft.key, lft.value,
		     lft.left, lft.right.left),
		node($black, k, v,
		     lft.right.right, rgt));
      otherwise
	=> node(c, k, v, lft, rgt);
    end case;
  else
    node(c, k, v, lft, rgt)
  end if;
end method l-balance;

define sealed method r-balance (c :: <color>, k, v,
				lft :: <tree>, rgt :: <tree>)
 => (new :: <tree>)
  if (c == $black)
    case
      rgt.red? & rgt.right.red?
	=> node($red, rgt.key, rgt.value,
		node($black, k, v,
		     lft, rgt.left),
		node($black, rgt.right.key, rgt.right.value,
		     rgt.right.left, rgt.right.right));
      rgt.red? & rgt.left.red?
	=> node($red, rgt.left.key, rgt.left.value,
		node($black, k, v,
		     lft, rgt.left.left),
		node($black, rgt.key, rgt.value,
		     rgt.left.right, rgt.right));
      otherwise
	=> node(c, k, v, lft, rgt);
    end case;
  else
    node(c, k, v, lft, rgt)
  end if;
end method r-balance;

// Deletion does not have any algorithm nearly as nice as insertion
// does. I used a modified version of the deletion algorithm in
// Cormen, Leiserson and Rivest, so that instead of inplace update
// we copy nodes. Also the algorithm is in a functional style, which
// is easier to read but still quite hairy. I do the same lbalance
// rbalance trick as above, except here it's for basic readability
// instead of efficiency. :( 

define sealed method delete (t :: <rb-tree>, k :: <object>) => (n :: <rb-tree>)
  make(t.object-class,
       tree: remove-key(t.tree, k, t.order),
       size: t.size - 1)
end method delete;

define sealed method remove-key (t :: <tree>, k :: <object>, \< :: <function>)
 => (new :: <tree>)
  local
  method pop-min (t :: <node>)
   => (min :: <node>, t :: <tree>, double-black? :: <boolean>)
    if (t.left.leaf?)
      if (t.black?)
	values(t, t.right, #t)
      else
	values(t, t.right, #f)
      end if;
    else
      let (min, new-left, double-black?) = pop-min(t.left);
      let (new-left, double-black?) = fix-left(t, new-left, double-black?);
      values(min, new-left, double-black?);
    end if;
  end method pop-min,
  method remove (t :: <tree>, k :: <object>)
   => (new :: <tree>, double-black? :: <boolean>)
    if (t.leaf?)
      error("delete: key %= not in red-black tree", k)
    elseif (k < t.key)
      let (new-left, double-black?) = remove(t.left, k);
      fix-left(t, new-left, double-black?);
    elseif (t.key < k)
      let (new-right, double-black?) = remove(t.right, k);
      fix-right(t, new-right, double-black?);
    else
      // In this case we have found the node with the value to
      // delete. If at most one of the branches is a node, then
      // we can splice it out very easily. We need to warn of
      // a double-black violation if both parent and child are
      // black, but otherwise blackifying the child will preserve
      // the black height invariant.
      //
      if (t.left.nil?)
	values(t.right.blackify, t.black? & t.right.black?)
      elseif (t.right.nil?)
	values(t.left.blackify, t.black? & t.left.black?)
      else
	let (min, new-right, double-black?) = pop-min(t.right);
	fix-right(node(t.color, min.key, min.value, t.left, t.right),
		  new-right,
		  double-black?);
      end if;
    end if;
  end method remove,
  method fix-left (parent :: <node>,
		   new-left :: <tree>,
		   double-black? :: <boolean>)
   => (new :: <tree>, double-black? :: <boolean>)
    if (double-black?)
      let (b, a, d, c, e) = values(parent, new-left, parent.right,
				   parent.right.left, parent.right.right);
      case
	b.black? & d.red?
	  => begin
	       let (q, v?) = fix-left(node($red, b.key, b.value,
					   a, c),
				      a,
				      #t);
	       fix-left(node($black, d.key, d.value,
			     q, e),
			q,
			v?)
	     end;
	d.black? & c.black? & e.black?
	  => begin
	       if (b.red?)
		 values(node($black, b.key, b.value,
			     a, node($red, d.key, d.value, c, e)),
			#f)
	       else
		 values(node($black, b.key, b.value,
			     a, node($red, d.key, d.value, c, e)),
			#t)
	       end if;
	     end;
	d.black? & c.red? & e.black?
	  => fix-left(node(b.color, b.key, b.value,
			   a,
			   node($black, c.key, c.value,
				c.left,
				node($red, d.key, d.value,
				     c.right, e))),
		      a,
		      #t);
	d.black? & e.red?
	  => values(node(b.color, d.key, d.value,
			 node($black, b.key, b.value,
			      a, c),
			 node($black, e.key, e.value,
			      e.left, e.right)),
		    #f);
	otherwise
	  => error("fix-left: This can't happen!");
      end case;
    else
      values(node(parent.color, parent.key, parent.value,
		  new-left,
		  parent.right),
	     #f)
    end if;
  end method fix-left,
  method fix-right (parent :: <node>,
		    new-right :: <tree>,
		    double-black? :: <boolean>)
   => (new :: <tree>, double-black? :: <boolean>)
    if (double-black?)
      let (b, a, d, c, e) = values(parent, new-right, parent.left,
				   parent.left.right, parent.left.left);
      case
	b.black? & d.red?
	  => begin
	       let (q, v?) = fix-right(node($red, b.key, b.value,
					    c, a),
				       a,
				       #t);
	       fix-right(node($black, d.key, d.value,
			      e, q),
			 q,
			 v?)
	     end;
	d.black? & c.black? & e.black?
	  => begin
	       if (b.red?)
		 values(node($black, b.key, b.value,
			     node($red, d.key, d.value, e, c), a),
			#f)
	       else
		 values(node($black, b.key, b.value,
			     node($red, d.key, d.value, e, c), a),
			#t)
	       end if;
	     end;
	d.black? & c.red? & e.black?
	  => fix-right(node(b.color, b.key, b.value,
			    node($black, c.key, c.value,
				 node($red, d.key, d.value,
				      e, c.left),
				 c.right),
			    a),
		       a,
		       #t);
	d.black? & e.red?
	  => values(node(b.color, d.key, d.value,
			 node($black, e.key, e.value,
			      e.left, e.right),
			 node($black, b.key, b.value,
			      c, a)),
		    #f);
	otherwise
	  => error("fix-right: This can't happen!");
      end case;
    else
      values(node(parent.color, parent.key, parent.value,
		  parent.left, new-right),
	     #f)
    end if;
  end method fix-right;
  let (new-tree, double-black?) = remove(t, k);
  //
  new-tree.blackify
end method remove-key;

//////////////////////////////////////////////////
// Priority Queue Operations
//////////////////////////////////////////////////

define sealed method min-key (t :: <rb-tree>) => (k :: <object>)
  if (t.tree.leaf?)
    error("min-key: tree has no elements!");
  else
    iterate loop (t :: <node> = t.tree)
      if (t.left.leaf?)
	t.key
      else
	loop(t.left)
      end if;
    end iterate;
  end if;
end method min-key;

define sealed method max-key (t :: <rb-tree>) => (k :: <object>)
  if (t.tree.leaf?)
    error("max-key: tree has no elements!")
  else
    iterate loop (t :: <node> = t.tree)
      if (t.right.leaf?)
	t.key
      else
	loop(t.right)
      end if;
    end iterate;
  end if;
end method max-key;

define sealed method successor (t :: <rb-tree>, k :: <object>) => (succ :: <object>)
  let \< :: <function> = t.order;
  local
    method minimum-node (t :: <node>) => (min :: <node>)
      if (t.left.leaf?)
	t
      else
	minimum-node(t.right)
      end if;
    end method minimum-node,
    method find-succ (parent :: <tree>) => (next :: <node>, real? :: <boolean>)
      let (t :: <node>,
	   ok? :: <boolean>) = case
				 parent.leaf?
				   => error("successor: no key %= in tree", k);
				 k < parent.key => find-succ(parent.left);
				 parent.key < k => find-succ(parent.right);
				 otherwise => 
				   if (~parent.right.leaf?)
				     values(parent.right.minimum-node, #t)
				   else
				     values(parent, #f)
				   end if;
			       end case;
      if (ok?)
	values(t, ok?)
      else
	values(parent, t == parent.left)
      end if;
    end method find-succ;
  let (succ :: <node>, real-successor? :: <boolean>) = find-succ(t.tree);
  if (real-successor?)
    succ.key
  else
    error("successor: no successor key to %=", k)
  end if;
end method successor;

define sealed method predecessor (t :: <rb-tree>, k :: <object>) => (j :: <object>)
  let \< :: <function> = t.order;
  local
    method maximum-node (tree :: <node>) => (max :: <node>)
      if (tree.right.leaf?)
	tree
      else
	maximum-node(tree.right)
      end if;
    end method maximum-node,
    method find-pred (parent :: <tree>) => (node :: <node>, real? :: <boolean>)
      let (t :: <node>,
	   ok? :: <boolean>) = case
				 parent.leaf?
				   => error("predecessor: no key %= in tree",
					    k);
				 k < parent.key => find-pred(parent.left);
				 parent.key < k => find-pred(parent.right);
				 otherwise =>
				   if (~parent.left.leaf?)
				     values(parent.left.maximum-node, #t)
				   else
				     values(parent, #f)
				   end if;
			       end case;
      if (ok?)
	values(t, ok?)
      else
	values(parent, t == parent.right)
      end if;
    end method find-pred;
  let (pred :: <node>, real-predecessor? :: <boolean>) = find-pred(t.tree);
  if (real-predecessor?)
    pred.key
  else
    error("predecessor: no predecessor key to %=", k)
  end if;
end method predecessor;

//////////////////////////////////////////////////////////////////////
// Collection protocol
//////////////////////////////////////////////////////////////////////

define sealed method element (t :: <rb-tree>, k :: <object>,
			      #key default = $unsupplied)
 => (val :: <object>)
  %lookup(t.tree, k, t.order, default)
end method element;

define sealed method %lookup (t :: <leaf>, k :: <object>,
		       \< :: <function>, default :: <object>)
 => (val :: <object>)
  if (default ~= $unsupplied)
    default
  else
    error("element: no key %= in tree %=", k, tree.object-class)
  end if;
end method %lookup;

define sealed method %lookup (tree :: <node>, k :: <object>,
		       \< :: <function>, default :: <object>)
 => (val :: <object>)
  if (k < tree.key)
    %lookup(tree.left, k, \<, default)
  elseif (tree.key < k)
    %lookup(tree.right, k, \<, default)
  else
    tree.value
  end if;
end method %lookup;

//

define sealed method empty? (t :: <rb-tree>) => (b :: <boolean>)
  %empty?(t.tree)
end method empty?;

define sealed inline method %empty? (t :: <leaf>) => (b :: <boolean>)
  #t
end method %empty?;

define sealed inline method %empty (t :: <node>) => (b :: <boolean>)
  #f
end method %empty;

//

define sealed method key-test (tree :: <rb-tree>) => (f :: <function>)
  let \< = tree.order;
  local method key-eq?(a :: <object>, b :: <object>) => (ans :: <boolean>)
	  ~(a < b | b < a)
	end method key-eq?;
  //
  key-eq?
end method key-test;

//

define sealed method type-for-copy (t :: <rb-tree>) => (class :: <class>)
  <table>
end method type-for-copy;

// The Iteration Protocols

// forward-iteration-protocol and backward-iteration-protocol iterate
// over the keys of the red-black tree in increasing and decreasing
// order, respectively. They do so in O(N) time, with O(lg N) time
// for initialization and O(lg N) space usage. 

// About the only optimization that makes sense for iteration 
// is to do something to eliminate the dynamic typing of the <list>
// used as a stack. But that takes enough code that I decided not
// to until there was a clear need. Note also that the methods are
// polymorphic in the collection type, to make forwarding this
// protocol trivial.

define sealed method forward-iteration-protocol (c :: <rb-tree>)
 => (init :: <list>,
     limit :: <boolean>,
     next :: <function>,
     done? :: <function>,
     current-key :: <function>,
     current-elt :: <function>,
     current-elt-setter :: <function>,
     copy-state :: <function>)
  local method left-stack(t :: <tree>, lst :: <list>)
	 => (new :: <list>)
	  if (t.leaf?)
	    lst
	  else
	    left-stack(t.left, pair(t, lst))
	  end if;
	end method left-stack;
  values(left-stack(c.tree, #()), // init
	 #f,		                   // limit
	 method (c, state :: <list>) => (next-state :: <list>)
	   left-stack(state.head.right, state.tail)
	 end method,
	 method (c, state :: <list>, lim :: <boolean>) => (done? :: <boolean>)
	   state.empty?
	 end method,
	 method (c, state :: <list>) => (current-key :: <object>)
	   state.head.key
	 end method,
	 method (c, state :: <list>) => (current-elt :: <object>)
	   state.head.value
	 end method,
	 method (c, state :: <list>) => (current-elt-setter :: <object>)
	   error("forward-iteration-protocol: <rb-tree> is immutable")
	 end method,
	 method (c, state :: <list>) => (copy-state :: <list>)
	   state.shallow-copy
	 end method)
end method forward-iteration-protocol;

define sealed method backward-iteration-protocol (c :: <rb-tree>)
 => (init :: <list>,
     limit :: <boolean>,
     next :: <function>,
     done? :: <function>,
     current-key :: <function>,
     current-elt :: <function>,
     current-elt-setter :: <function>,
     copy-state :: <function>)
  local method right-stack(t :: <tree>, lst :: <list>)
	 => (new :: <list>)
	  if (t.leaf?)
	    lst
	  else
	    right-stack(t.right, pair(t, lst))
	  end if;
	end method right-stack;
  values(right-stack(c.tree, #()), // init
	 #f,		                    // limit
	 method (c, state :: <list>) => (next-state :: <list>)
	   right-stack(state.head.left, state.tail)
	 end method,
	 method (c, state :: <list>, lim :: <boolean>) => (done? :: <boolean>)
	   state.empty?
	 end method,
	 method (c, state :: <list>) => (current-key :: <object>)
	   state.head.key
	 end method,
	 method (c, state :: <list>) => (current-elt :: <object>)
	   state.head.value
	 end method,
	 method (c, state :: <list>) => (current-elt-setter :: <object>)
	   error("backward-iteration-protocol: <rb-tree> is immutable")
	 end method,
	 method (c, state :: <list>) => (copy-state :: <list>)
	   state.shallow-copy
	 end method)
end method backward-iteration-protocol;

//////////////////////////////////////////////////
// Random Exported Utilities
//////////////////////////////////////////////////

// There's no real compelling reason for the existence of this method
// other than to offer set implementations a fast O(1) way of snagging
// a key from a tree. 

define sealed method a-key (t :: <rb-tree>,
		     #key default = $unsupplied)
 => (key :: <object>)
  if (t.size > 0)
    t.tree.key
  else
    if (default == $unsupplied)
      error("a-key: tree has no elements!")
    else
      default
    end if;
  end if;
end method a-key;

// Debugging methods

// TBD: I should add a print-object method. 

define sealed method display (c :: <rb-tree>) => ()
  local
  method spaces (n :: <integer>) => (s :: <string>)
    make(<string>, size: n, fill: ' ');
  end method spaces,
  method stringize(c :: <tree>) => (s :: <string>)
    if (c.red?)
      "red"
    else
      "black"
    end if;
  end method stringize,
  method display (c :: <tree>, indent :: <integer>) => ()
    if (c.leaf?)
      format-out("\n%s(nil)", indent.spaces)
    else
      format-out("\n%s", indent.spaces);
      format-out("(%s: %= -> %=",
		 c.stringize, c.key, c.value);
      display(c.left, indent + 7);
      display(c.right, indent + 7);
      format-out(")");
    end if;
  end method display;
  //
  display(c.tree, 0);
  format-out("\n");
end method display;


