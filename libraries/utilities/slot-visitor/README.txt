=====================
Library: slot-visitor
=====================

This library exports a macro to make it easier to visit every object and slot in
a "tree" of heterogenous objects. In particular, if you need to drill down and
examine or alter specific objects in the tree, this is a good way to do it.

-- Dustin Voss


===========================
Macro: slot-visitor-definer
===========================

Synopsis: Define a visitor function to recursively visit and alter objects and
slots.


--- Syntax: ---

: define {collection-recursive} slot-visitor NAME
:   CLASS {, {constant} SLOT}... ;
:   {CLASS {, {constant} SLOT}... ;}...
: end {slot-visitor {NAME}}

(Optional items are in braces.)


--- Arguments: ---

NAME    - The name of the visitor generic function.
CLASS   - The name of a class that the visitor can visit.
SLOT    - The name of a slot of the class that the visitor will visit.


--- Discussion: ---

This macro defines a visitor function [1] named 'NAME'. You call the visitor
function with an action function [2] as an argument. This applies the action
function to the object and then recursively visits and applies the action
function to the object's slots, including slots of superclasses also found in
the 'CLASS' list.

Consider the following code:

[EXAMPLE]

define class <person> (<object>)
  slot name :: <string>, init-keyword: #"name";
end class;

define class <employee> (<person>)
  slot manager-name :: <string>, init-keyword: #"manager";
end class;

define slot-visitor visit-every-name                                    [:1]
  <person>,  name;
  <manager>, manager-name;
  <string>   ;
end slot-visitor;

[END EXAMPLE]

Line [:1] defines a 'visit-every-name' visitor function. When 'visit-every-name'
is called on a '<person>', it will apply the supplied action function to the
'<person>' itself and then visit the person's 'name' slot and apply the action
function to that string. When called on an '<employee>', it will apply the
action to the '<employee>' and then to the strings in the employee's
'manager-name' and 'name' slots -- the latter because '<employee>' is a subclass
of '<person>' and the entry for '<person>' lists the 'name' slot.

The visitor function can accept user-specified keyword arguments. These
arguments are passed on to each action function application.

The action function is only applied to an object if it is applicable to that
object based on the function's first argument. If the function is applicable,
the object's slots are then only visited if the function returns true. If the
function is not applicable, the object's slots are visited unconditionally.

Note that if the action function is an implicitly-declared generic function, its
first argument will be specialized on <object> and so the function will be
applicable to every visited object. If using a generic function as the action
function, consider including a do-nothing method specialized on <object> as a
catch-all to prevent missing method errors.

The action function is called with a 'setter' keyword argument. This is a
function by which the action function can replace the object or slot value. It
is false if the action function is applied to a top-level object not contained
by a slot or to an object contained in a slot specified with the 'constant'
[code] adjective shown in the macro syntax. Any 'setter' keyword argument passed
to the visitor function is replaced.

The action function is also called with a 'visited' keyword argument. This is a
table that tracks visited objects to prevent infinite recursion. It defaults to
a <table> if not provided, but you can provide an instance of a subclass of
<table> by calling the visitor function with a 'visited' keyword argument.

The 'constant' [code] adjective shown in the macro syntax indicates that there
is no setter available for the given slot. Every 'SLOT' argument to the macro
that refers to a constant slot should have this adjective or you will get
unknown method name errors during compilation.

The 'collection-recursive' [code] adjective shown in the macro syntax adds a
default behavior for collections that are not included as 'CLASS' arguments to
the macro. This behavior is to visit and apply the action function to every
element of the collection; an appropriate 'setter' argument will be passed to
the action function.


[1]: The visitor function has the following signature:

: NAME (object :: CLASS, action :: <function>, #key, #all-keys) => ()
 

[2]: An action function must be compatible with the following signature, but may
include additional keyword parameters corresponding to arguments of the visitor
function:

: (object :: CLASS, #key setter :: false-or(<function>), visited :: <table>)
: => (do-slots? :: <boolean>)
