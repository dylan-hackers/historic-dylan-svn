Module:    xml-converter
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// XML-Converter Client Protocol.
// =============================

define locked variable *converter-ids* = 0;

define method next-converter-id () => (id :: <integer>)
  atomic-increment!(*converter-ids*);
end method next-converter-id;

define open class <xml-converter> (<object>)
  // The ID of this converter, used to create unique ids.
  constant slot converter-id :: <integer> = next-converter-id(),
    init-keyword: converter-id:;

  // All objects we have already converted.
  slot converted-objects :: <table> = make(<table>);
  
  // The objects which we have started to convert but not
  // finished yet.
  slot currently-converting :: <list> = #();
  
  // A table mapping objects to their <unique-id>s.
  constant slot unique-id-table :: <table> = make(<object-table>);

  // A table mapping <unique-id>s to their objects.
  constant slot object-table :: <table> = make(<object-table>);
end class <xml-converter>;


// Converts a Dylan Object to XML.  Takes care of handling cycles. 
// If Preserve-Identity? is true multiple occurrences of the same object are
// dumped as references to its Unique-Id.  Otherwise Unique-Ids are only used
// to break cycles.
//
define open generic convert-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (document :: <xml>);

// Converts an XML representation of an object structure to an equivalent
// Object graph.
//
define open generic convert-from-xml
    (converter, document, #key) => (object :: <object>);


// A unique identifier that can be used to manage object references.
//
define class <unique-id> (<object>)
  constant slot converter-id :: false-or(<integer>),
    init-keyword: converter-id:;
  constant slot serial-number :: <integer>,
    init-keyword: serial-number:;
end class <unique-id>;

define variable *unique-id-table* = make(<table>, weak: value:);

define method make
    (self == <unique-id>, #key converter-id = #f, serial-number)
 => (unique-id :: <unique-id>);
  let old-id = if (serial-number)
                 element(*unique-id-table*, serial-number, default: #f);
               else
                 #f;
               end if;
  if (old-id)
    old-id
  else
    unless (serial-number)
      serial-number := next-serial-number!();
    end unless;
    *unique-id-table*[serial-number] := next-method(self, 
                                                    converter-id: converter-id,
                                                    serial-number: serial-number);
  end if;
end method make;

// The object id is used to break cycles while converting
// objects to XML.
//
define open generic object-id
    (object, #key default) => (id :: false-or(<unique-id>));

// Returns the object-id if it exists, assigns and returns a fresh id otherwise.
//
define open generic object-id!
    (object, #key converter-id :: false-or(<integer>)) => (id :: <unique-id>);

// The name of the class of Object, converted to XML conventions,
// the module and library.
//
define open generic converted-class-name-and-library
    (converter :: <xml-converter>, object)
 => (converted-name :: <string>, module :: <string>, library :: <string>);

// The name of the class of Object, as a single XML entity.
//
define open generic converted-class-name
    (converter :: <xml-converter>, object)
 => (converted-name :: <string>);

define open generic converted-slot-name
    (converter :: <xml-converter>, getter :: <function>) => (converted-name :: <string>);


// Implementor Protocol.
// ====================

// Performs the actual conversion.  Does not have to check for cycles, etc., as this is
// already done by Convert-To-XML.
//
define open generic convert-object-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (document :: <xml>);

define open generic convert-slot-to-xml
    (converter :: <xml-converter>, object :: <object>,  sd :: <slot-descriptor>,
     #key preserve-identity?)
 => (slot :: <xml>);

define open generic convert-slots-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (document :: <sequence>);

define open generic found-cycle?
    (converter :: <xml-converter>, object) => (well? :: <boolean>);

define open generic ensure-unique-id
    (converter :: <xml-converter>, object) => (reference :: <unique-id>);

define open generic convert-unique-reference
    (converter :: <xml-converter>, object) => (reference :: <xml>);


// Implementation.
// ==============

// A table mapping objects to their object IDs.  We don't keep the 
// reverse mapping around globally to allow garbage collection.  Each 
// XML-Converter keeps track of the objects it converted, though.
// FIXME: Make this thread safe.
define variable *object-id-table* = make(<table>, weak: key:);
define locked variable *current-object-id* = 0;

define method next-serial-number!
    () => (next-id :: <integer>)
  atomic-increment!(*current-object-id*);
end method next-serial-number!;

define method object-id
    (object, #key default = #f) => (id :: false-or(<unique-id>));
  element(*object-id-table*, object, default: default);
end method object-id;

define method object-id-setter
    (new-object-id :: <unique-id>, object) => (new-object-id :: <unique-id>);
  element(*object-id-table*, object) := new-object-id;
end method object-id-setter;

define method object-id!
    (object, #key converter-id :: false-or(<integer>)) => (id :: <unique-id>);
  object-id(object)
    | (object-id(object) := make(<unique-id>, converter-id: converter-id));
end method object-id!;

define method ensure-unique-id
    (converter :: <xml-converter>, object) => (xml-id :: <unique-id>);
  let id-table = converter.unique-id-table;
  let id :: false-or(<unique-id>)
    = element(id-table, object, default: #f);
  unless (id)
    id := object-id!(object, converter: converter);
    id-table[object] := id;
  end unless;
  let obj-table = object-table(converter);
  let obj = element(obj-table, id, default: #f);
  unless (obj)
    obj-table[id] := object;
  end unless;
  id;
end method ensure-unique-id;

define macro with-current-xml-element
  { with-current-xml-element
        (?converter:expression, ?object:expression, ?xml:expression)
      ?:body
    end }
 => { let tmp = ?converter.currently-converting;
      block ()
        ?converter.currently-converting := pair(?object, tmp);
        ?body;
      afterwards
        ?converter.converted-objects[?object] := ?xml;
      cleanup
        ?converter.currently-converting := tmp;
      end }
end macro with-current-xml-element;

// WARNING: DEEP MAGIC AHEAD.
// Some internal methods in this implementation use the Functional
// Developer Metaobject Protocol and various compiler internals.
// You are not supposed to understand the following methods
// Unless you are supposed to understand the following methods.

define method class-name-and-library-or-lose (object)
 => (name :: <byte-string>, module :: <byte-string>, library :: <byte-string>)
    let (class-name, class-module, class-library)
      = object.object-class.class->variable;
    unless (class-name)
      error(make(<no-conversion-for-dylan-object-error>,
                 dylan-object: object));
    end unless;
    values(class-name, class-module, class-library);
end method class-name-and-library-or-lose;

define method converted-class-name-and-library
    (converter :: <xml-converter>, object)
 => (converted-class-name :: <string>, module :: <byte-string>, library :: <byte-string>);
  let (name, module, library) = class-name-and-library-or-lose(object);
  let sz = name.size;
  if (sz > 2 & name[0] = '<' & name[sz - 1] = '>')
    values(copy-sequence(name, start: 1, end: sz - 1), module, library);
  else
    error(make(<no-conversion-for-dylan-object-error>,
               dylan-object: object));
  end if;
end method converted-class-name-and-library;

define method converted-class-name
    (converter :: <xml-converter>, object)
 => (xml-name :: <string>);
  let (name, module, library) = converted-class-name-and-library(converter, object);
  // FIXME:  This should be tied to namespaces.
  // We should have a namespace declaration like
  // xmlns:lib="http://opendylan.org/xml-converter/library/module" and output
  // names as elements <lib:name/>
  // Or maybe not.
  concatenate(module, "@", library, ":", name);
end method converted-class-name;

define method found-cycle? (converter :: <xml-converter>, object) => (well? :: <boolean>);
  member?(object, converter.currently-converting);
end method found-cycle?;

define method converted-slot-name
    (converter :: <xml-converter>, getter :: <function>)
 => (converted-slot-name :: <string>);
 let name = debug-name(getter);
 as(<string>, name);
end method converted-slot-name;

define method convert-unique-reference
    (converter :: <xml-converter>, object) => (reference :: <element>);
  make-xml-element (xml-converter-unique-reference)
    attribute unique-id = ensure-unique-id(converter, object);
  end make-xml-element;
end method convert-unique-reference;

// FIXME:  This method uses the internal class <slot-descriptor> although
// it is exported.  Clean up the protocol so that this is not necessary.
//
define method convert-slot-to-xml
    (converter :: <xml-converter>, object, slot :: <slot-descriptor>,
     #key preserve-identity?)
 => (slot :: <xml>);
  let getter = slot-getter(slot);
  let class = object-class(object);
  let offset = slot-offset(slot, class);
  let name = converted-slot-name(converter, getter);
  let initialized? :: <boolean> = slot-initialized?(object, getter);
  let init-keyword :: <symbol> = init-keyword(slot);
  make-xml-element (slot)
    element name = name;
    element initialized = convert-to-xml(converter, initialized?);
    element value = convert-to-xml(converter,
                                   initialized-slot-element(object, offset),
                                   preserve-identity?: preserve-identity?);
    element init-keyword = convert-to-xml(converter, init-keyword);
  end make-xml-element;
end method convert-slot-to-xml;

define method convert-repeated-slot-to-xml
    (converter :: <xml-converter>, object, slot :: <slot-descriptor>,
     #key preserve-identity?)
 => (slot :: <xml>);
  let getter = slot-getter(slot);
  let class = object-class(object);
  let name = converted-slot-name(converter, getter);
  make-xml-element (repeated-slot)
    element name = name;
    element values = for (i from 0 below size(object))
                       convert-to-xml(converter, object[i]);
                     end for;
  end make-xml-element;
end method convert-repeated-slot-to-xml;

// FIXME:  This method uses the internal class <slot-descriptor> although
// it is exported.  Clean up the protocol so that this is not necessary.
//
define method convert-slots-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (slots :: <vector>);
  let class = object-class(object);
  let slot-descriptors
    = as(<vector>, choose(method (sd :: <slot-descriptor>)
                            slot-allocation(sd) ~= #"virtual" 
                          end,
                          slot-descriptors(class)));
  let slots = map(convert-slot-to-xml(converter, object, _, 
                                      preserve-identity?:  preserve-identity?),
                  slot-descriptors);
  let repeated-slot = repeated-slot-descriptor(class);
  if (repeated-slot)
    slots := concatenate!(list(convert-repeated-slot-to-xml
                                 (converter, object, repeated-slot)),
                          slots);
  end;
  slots;
end method convert-slots-to-xml;

define method convert-object-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity? = #t)
 => (document :: <xml>);
    let (class-name, module-name, library-name)
      = converted-class-name-and-library(converter, object);
    let result = make-xml-element (dylan-object)
                   element class-name = class-name;
                   element module-name = module-name;
                   element library-name = library-name;
                 end make-xml-element;
    with-current-xml-element (converter, object, result)
      let slot-elements :: <vector>
        = convert-slots-to-xml(converter, object, 
                               preserve-identity?: preserve-identity?);
      result.node-children
        := concatenate!(result.node-children, slot-elements);
    end with-current-xml-element; 
    result;
end method convert-object-to-xml;

// Deep Magic ends here.

define method convert-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (document :: <xml>);
  if (found-cycle?(converter, object))
    convert-unique-reference(converter, object);
  elseif (preserve-identity? & member?(object, converter.converted-objects))
    convert-unique-reference(converter, object);
  else
    convert-object-to-xml(converter, object, 
                          preserve-identity?: preserve-identity?);
  end if;
end method convert-to-xml;

define method simple-element-value (elt :: <element>)
  let children = node-children(elt);
  unless (children.size = 1)
    error(make(<converted-object-not-simple-error>, xml-element: elt));
  end unless;
  children[0];
end method simple-element-value;

// FIXME:  The following macro should define the converters
// XML -> Dylan as well....

define macro simple-converters-to-xml-definer
  { define simple-converters-to-xml end }
    => { }
  { define simple-converters-to-xml
      convert ?type:expression => ?tag:expression, no-value; ... end }
    => { define method convert-object-to-xml
             (converter :: <xml-converter>, object :: ?type,
              #key preserve-identity?)
          => (document :: <xml>);
           make(<element>,
                name: converted-class-name(converter, object),
                parent: $default-xml-parent,
                children: #[]);
         end;
         define simple-converters-to-xml ... end }
  { define simple-converters-to-xml
      convert ?type:expression => ?tag:expression; ... end }
    => { define method convert-object-to-xml
             (converter :: <xml-converter>, object :: ?type,
              #key preserve-identity?)
          => (document :: <xml>);
           make(<element>,
                name: converted-class-name(converter, object),
                parent: $default-xml-parent,
                children: vector(format-to-string("%S", object)));
         end;
         define simple-converters-to-xml ... end }
  { define simple-converters-to-xml
      convert ?type:expression => ?tag:expression using ?converter:expression; ... end }
    => { define method convert-object-to-xml
             (converter :: <xml-converter>, object :: ?type,
              #key preserve-identity?)
          => (document :: <xml>);
           make(<element>,
                name: converted-class-name(converter, object),
                parent: $default-xml-parent,
                children: vector(?converter(object)));
         end;
         define simple-converters-to-xml ... end }
end macro simple-converters-to-xml-definer;

define simple-converters-to-xml
  convert <number> => "number";
  convert <integer> => "integer";
  convert <single-float> => "single-float";
  convert <double-float> => "double-float";
  convert <character> => "character";
  convert <byte-character> => "byte-character";
  convert <unicode-character> => "unicode-character";
  convert <string> => "string";
  convert <byte-string> => "byte-string";
  convert <unicode-string> => "unicode-string";
  convert <symbol> => "symbol";
  convert singleton(#t) => "true", no-value;
  convert singleton(#f) => "false", no-value;
  convert <empty-list> => "empty-list", no-value;
  convert <unbound> => "unbound", no-value;
  // We shouldn't need to use a dfmc-internal function to
  // convert functions.
  // Alas we do.
  // But can't.  Because we get problems when linking dfmc-common.
  // Investigate this.
  // convert <function> => "function" using name;
end simple-converters-to-xml;

/*

define method convert-from-xml
    (converter, document, #rest args, #key) => (object :: <object>);
  let converters :: <explicit-key-collection>
    = element-converters(converter);
  let name = document.name-with-proper-capitalization;
  let f = element(converters, name, default: #f);
  if (f)
    apply(f, converter, document, args);
  else
    error(make(<no-conversion-for-xml-element-error>,
               xml-element: document));
  end if;
end method convert-from-xml;

define variable *default-xml-to-dylan-converters*
  = make(<string-table>);
*/

// TODO:  Define the default converters.

