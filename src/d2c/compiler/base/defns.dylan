module: definitions
rcs-header: $Header: /home/housel/work/rcs/gd/src/d2c/compiler/base/defns.dylan,v 1.1 1994/12/12 13:01:20 wlott Exp $
copyright: Copyright (c) 1994  Carnegie Mellon University
	   All rights reserved.


// <definition> -- exported.
//
// Abstract class that all definitions inherit from.  A definition is the
// compilers handle on some run-time value.  In addition to the obvious things,
// definitions exist for things like the type of a variable (when it isn't a
// compile-time constant).
// 
define abstract class <definition> (<object>)
  //
  // The name of the module variable this is the definition for.
  slot defn-name :: <name>, required-init-keyword: name:;
end;

define method print-object (defn :: <definition>, stream :: <stream>) => ();
  pprint-fields(defn, stream, name: defn.defn-name);
end;


// {check,make}-syntax-table-additions -- exported.
//
// These functions are called to check and make changes to a modules syntax
// table when some definition becomes accessable.
//
define generic check-syntax-table-additions (table, defn, name) => ();
define generic make-syntax-table-additions (table, defn, name) => ();


define method check-syntax-table-additions
    (table :: <table>, defn :: <false>, name :: <symbol>) => ();
end;

define method check-syntax-table-additions
    (table :: <table>, defn :: <definition>, name :: <symbol>)
    => ();
  // For almost all definitions name shouldn't have any special syntax.
  unless (merge-category(table, name, <name-token>))
    error("Inconsistent syntax for %=", name);
  end;
end;


define method make-syntax-table-additions
    (table :: <table>, defn :: <false>, name :: <symbol>) => ();
end;

define method make-syntax-table-additions
    (table :: <table>, defn :: <definition>, name :: <symbol>)
    => ();
  table[name] := merge-category(table, name, <name-token>);
end;


define abstract class <implicit-definition> (<definition>)
end;

