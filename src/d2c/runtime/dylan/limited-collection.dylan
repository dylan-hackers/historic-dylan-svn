RCS-Header: $Header: /scm/cvs/src/d2c/runtime/dylan/limited-collection.dylan,v 1.1.2.1 2000/06/24 16:19:02 emk Exp $
Module: dylan-viscera
Copyright: See below.
Synopsis: Runtime support for limited collections.

//-------------------------------------------------------------------------
// Copyright (C) 1999 Eric Kidd
// (Please add your name and the year you released your changes here.)
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR
// OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of the author(s) shall not
// be used in advertising or otherwise to promote the sale, use or other
// dealings in this Software without prior written authorization from the
// author(s).
//-------------------------------------------------------------------------

// XXX - We should rename this slot on <limited-type>, but we can't do that
// without modifying cback's heap-dumping code and incrementing the
// bootstrap counter.
define constant limited-type-base-class = limited-integer-base-class;


//=========================================================================
//  make(singleton(<limited-collection>))
//=========================================================================
//  We pick apart the limited collection type and do (another) dispatch.
//
//  To add a new limited collection type, define an appropriate method on
//  make-limited-collection.

define inline method make
    (type :: <limited-collection>, #rest supplied-keys, #key, #all-keys)
 => (instance :: <collection>)
  apply(make-limited-collection,
	type.limited-type-base-class,
	type.limited-element-type,
	type, supplied-keys);
end method make;

define generic make-limited-collection
    (base-class :: <class>,
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #key, #all-keys)
 => (instance :: <collection>);


//=========================================================================
//  <limited-collection-mixin>
//=========================================================================
// Still thinking about this...
//
// XXX - We should be called <limited-collection>, but that name is already
// taken by a subclass of <limited-type>. Investiage doing this renaming
// someday--it will require changes to cback and possibly other parts of
// the compiler.

define class <limited-collection-mixin> (<collection>)
  // What limited type was used to create this collection?
  slot %limited-collection-type :: <limited-collection>,
    required-init-keyword: collection-type:;
end class;

define method element-type (collection :: <limited-collection-mixin>)
 => (type :: <type>, indefinite? :: <boolean>);
  element-type(collection.%limited-collection-type);
end method element-type;


//=========================================================================
//  <limited-object-table>
//=========================================================================
// We wrap the regular  

define class <limited-object-table>
    (<object-table>, <limited-collection-mixin>)
end class <limited-object-table>;

define method make-limited-collection
    (base-class :: type-union(singleton(<table>), singleton(<object-table>)),
     element-type :: <type>,
     collection-type :: <limited-collection>,
     #rest supplied-keys, #key, #all-keys)
 => (instance :: <object-table>)
  apply(make, <limited-object-table>,
	collection-type: collection-type,
	supplied-keys);
end method make-limited-collection;

define method element-setter
    (new-value :: <object>, collection :: <limited-object-table>,
     key :: <object>)
 => (element :: <object>)
  check-type(new-value, collection.%limited-collection-type.element-type);
  next-method();
end method element-setter;
