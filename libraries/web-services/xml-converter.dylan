Module:    xml-converter
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// XML-Converter Client Protocol.
// =============================

define locked variable *converter-ids* = random(65536);

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

/*
define constant $xml-converters = make(<object-table>);

define method register-xml-converter (converter :: <xml-converter>) => ();
  let id = converter.converter-id;
  $xml-converters[id] := converter;
end method register-xml-converter;

define method find-xml-converter
    (id :: <integer>) => (converter :: false-or(<converter>));
  element($xml-converters, id, default: #f); // Make a new converter if none exists?
end method find-xml-converter;
*/

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


define open generic referenced-object
    (object :: <object>) => (object :: <object>);

define method referenced-object
    (object :: <object>) => (object :: <object>);
  object;
end method referenced-object;

// A unique identifier that can be used to manage object references.
//
define class <unique-id> (<object>)
  constant slot converter-id :: false-or(<integer>),
    init-keyword: converter-id:;
  constant slot serial-number :: <integer>,
    init-keyword: serial-number:;
  // We may use #f to denote a unique-id with no referenced object,
  // because the XML-converter will never generate a unique-id for
  // #f.
  slot referenced-object :: <object>,
    init-keyword: referenced-object:;

  // A list of bakpatches that are to be
  // done if the referenced object for this id becomes known.
  slot backpatches :: <list> = #();
end class <unique-id>;

define variable *unique-id-table* = make(<table>, weak: value:);

define method initialize
    (id :: <unique-id>, #key referenced-object) => ();
  next-method();
  unless (referenced-object)
    referenced-object(id) := id;
  end unless;
end method initialize;
    

define method make
    (self == <unique-id>, 
     #key converter-id = #f, serial-number, referenced-object: new-ref)
 => (unique-id :: <unique-id>);
  let old-id = if (serial-number)
                 element(*unique-id-table*, serial-number, default: #f);
               else
                 #f;
               end if;
  if (old-id)
    let old-ref = referenced-object(old-id);
    if (old-ref & new-ref & old-ref ~== new-ref)
      error(make(<simple-error>,
                  format-string: "Reusing unique reference!",
                  format-arguments: #()));
    elseif (new-ref)
      referenced-object(old-id) := new-ref;
      perform-backpatches(old-id, new-ref);
    end if;
    old-id;
  else
    unless (serial-number)
      serial-number := next-serial-number!();
    end unless;
    *unique-id-table*[serial-number] := next-method(self, 
                                                    converter-id: converter-id,
                                                    serial-number: serial-number,
                                                    referenced-object: new-ref);
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

define open generic library-as-xml-namespace
    (converter :: <xml-converter>, library :: <string>, module :: <string>)
 => (xml-namespace :: <xml-namespace>);

// The name of the class of Object, as a single XML entity.
//
define open generic class-name-as-xml-name
    (converter :: <xml-converter>, object)
 => (converted-name :: <xml-name>);

define open generic converted-slot-name
    (converter :: <xml-converter>, 
    slot-getter-or-descriptor :: type-union(<function>, <slot-descriptor>))
 => (converted-name :: <string>);


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
    (converter :: <xml-converter>, object)
 => (reference :: <unique-id>);

define open generic convert-unique-reference-to-xml
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

define method object-id
    (object :: <unique-id>, #key default = #f) => (id :: <unique-id>);
  object;
end method object-id;

define method object-id-setter
    (new-object-id :: <unique-id>, object) => (new-object-id :: <unique-id>);
  new-object-id.referenced-object := object;
  element(*object-id-table*, object) := new-object-id;
  let back-patches = new-object-id.backpatches;
  perform-backpatches(new-object-id, object);
  new-object-id;
end method object-id-setter;

define method object-id-setter
    (new-object-id :: <unique-id>, object :: <unique-id>)
 => (object :: <unique-id>);
  object;
end method object-id-setter;

define method object-id!
    (object, #key converter-id :: false-or(<integer>)) => (id :: <unique-id>);
  object-id(object)
    | (object-id(object) := make(<unique-id>, converter-id: converter-id));
end method object-id!;

define method request-backpatch
    (unique-id :: <unique-id>, bp :: <function>) => ();
  unique-id.backpatches := pair(bp, unique-id.backpatches);
end method request-backpatch;

define method perform-backpatches (unique-id :: <unique-id>, object)
  let back-patches = unique-id.backpatches;
  // format-out("performing backpatches\n");
  unless (back-patches.empty?)
    for (f in back-patches)
      f(object);
    end for;
  end unless;
end method perform-backpatches;

// This is like ensure-unique-id, but for objects for which do not
// check for already existing ids.  This is done when converting 
// from XML to Dylan so that uninitialized data cannot spuriously
// equal an existing id.

define method assign-unique-id
    (converter :: <xml-converter>, object, unique-id :: <unique-id>)
 => (unique-id :: <unique-id>)
  let id-table = converter.unique-id-table;
  id-table[object] := unique-id;
  let obj-table = converter.object-table;
  obj-table[unique-id] := object;
  perform-backpatches(unique-id, object);
  unique-id;
end method assign-unique-id;

define method assign-unique-id
    (converter :: <xml-converter>, object :: <unique-id>, id :: <unique-id>)
 => (unique-id :: <unique-id>);
  id;
end method assign-unique-id;

define method ensure-unique-id
    (converter :: <xml-converter>, object) => (xml-id :: <unique-id>);
  let id-table = converter.unique-id-table;
  let id :: false-or(<unique-id>)
    = element(id-table, object, default: #f);
  unless (id)
    id := object-id!(object, converter-id: converter.converter-id);
    id-table[object] := id;
  end unless;
  let obj-table = object-table(converter);
  let obj = element(obj-table, id, default: #f);
  unless (obj)
    obj-table[id] := object;
  end unless;
  perform-backpatches(id, object);
  id;
end method ensure-unique-id;

define method ensure-unique-id
    (converter :: <xml-converter>, object :: <unique-id>)
 => (object :: <unique-id>);
  object;
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
  // format-out("converted-class-name-and-library\n");
  let (name, module, library) = class-name-and-library-or-lose(object);
  let sz = name.size;
  if (sz > 2 & name[0] = '<' & name[sz - 1] = '>')
    values(copy-sequence(name, start: 1, end: sz - 1), module, library);
  else
    error(make(<no-conversion-for-dylan-object-error>,
               dylan-object: object));
  end if;
end method converted-class-name-and-library;


define constant $dylan-default-namespace-name
  = "http://www.opendylan.org/xml/0.1/";
define constant $dylan-default-namespace
  = make(<xml-namespace>,
         short-name: "dylan",
         url: concatenate($dylan-default-namespace-name, "dylan/dylan"));

define method library-as-xml-namespace
    (converter :: <xml-converter>, library :: <string>, module :: <string>)
 => (xml-namespace :: <xml-namespace>);
  // format-out("library-as-xml-namespace\n");
  make(<xml-namespace>,
       short-name: concatenate(library, "-", module),
       url: concatenate($dylan-default-namespace-name,
                        library, "/", module));
end method library-as-xml-namespace;

define method class-name-as-xml-name
    (converter :: <xml-converter>, object)
 => (xml-name :: <xml-name>);
  // format-out("class-name-as-xml-name\n");
  let (name, module, library) = converted-class-name-and-library(converter, object);
  let namespace = library-as-xml-namespace(converter, library, module);
  make(<xml-name>,
       namespace: namespace,
       local-name: name);
end method class-name-as-xml-name;

define method found-cycle? (converter :: <xml-converter>, object) => (well? :: <boolean>);
  member?(object, converter.currently-converting);
end method found-cycle?;

define method replace-bad-char
    (char :: <character>) => (char :: <character>);
  select (char)
    '%' => 'P';
    otherwise => char;
  end select;
end method replace-bad-char;

define method replace-bad-chars
    (string :: <string>) => (string :: <string>);
  map-as(<string>, replace-bad-char, string);
end method replace-bad-chars;

define method converted-slot-name
    (converter :: <xml-converter>, getter :: <function>)
 => (converted-slot-name :: <string>);
 let name = debug-name(getter);
 let raw-name = as(<string>, name);
 replace-bad-chars(raw-name);
end method converted-slot-name;

define method converted-slot-name
    (converter :: <xml-converter>, slot-descriptor :: <slot-descriptor>)
 => (converted-slot-name :: <string>);
  let getter = slot-getter(slot-descriptor);
  converted-slot-name(converter, getter);
end method converted-slot-name;

define method convert-unique-id-to-xml
    (converter :: <xml-converter>, id :: <unique-id>)
 => (element :: <element>);
  // format-out("convert-unique-id-to-xml(%=)\n", id);
  make-xml-element ("unique-id")
    element "converter-id" => convert-to-xml(converter, id.converter-id);
    element "serial-number" => convert-to-xml(converter, id.serial-number);
  end make-xml-element;
end method convert-unique-id-to-xml;

define method convert-unique-reference-to-xml
    (converter :: <xml-converter>, object) => (reference :: <element>);
  // format-out("convert-unique-reference-to-xml(%=, %=)\n", converter, object);
  // Convert a unique reference.  Make sure that we avoid
  // cycles.
  
  // We might remove ref if we are sure that referenced-object(id)
  // is always object.
  let id :: <unique-id> = ensure-unique-id(converter, object);
  make-xml-element ("unique-reference")
    element "cid" => convert-to-xml(converter, id.converter-id);
    element "sn" => convert-to-xml(converter, id.serial-number);
  end make-xml-element;
end method convert-unique-reference-to-xml;

// FIXME:  This method uses the internal class <slot-descriptor> although
// it is exported.  Clean up the protocol so that this is not necessary.
//
define method convert-slot-to-xml
    (converter :: <xml-converter>, object, slot :: <slot-descriptor>,
     #key preserve-identity?)
 => (slot :: <xml>);
  // format-out("convert-slot-to-xml\n");
  let getter = slot-getter(slot);
  let class = object-class(object);
  let offset = slot-offset(slot, class);
  let name = converted-slot-name(converter, getter);
  let initialized? :: <boolean> = slot-initialized?(object, getter);
// Probably not needed?
//  let init-keyword :: false-or(<symbol>) = init-keyword(slot);
  make-xml-element ("slot")
//    attribute "xmlns" => $dylan-default-namespace-name;
    element ("name") { content name; }
    element "initialized" => convert-to-xml(converter, initialized?);
    element "value" => convert-to-xml(converter,
                                      initialized-slot-element(object, offset),
                                      preserve-identity?: preserve-identity?);
//    element "init-keyword" => convert-to-xml(converter, init-keyword);
  end make-xml-element;
end method convert-slot-to-xml;

define method convert-repeated-slot-to-xml
    (converter :: <xml-converter>, object, slot :: <slot-descriptor>,
     #key preserve-identity?)
 => (slot :: <xml>);
  let getter = slot-getter(slot);
  let class = object-class(object);
  let name = converted-slot-name(converter, getter);
  let sz = size(object);
  make-xml-element ("repeated-slot")
//    attribute "xmlns" => $dylan-default-namespace-name;
    element "size" => convert-to-xml(converter, sz);
    element ("name") { content name; }
    element "values" ==> map-as(<simple-object-vector>,
                                method (i) => (xml :: <xml>)
                                  make-xml-element ("i")
                                    raw-content convert-to-xml(converter, object[sz - i]);
                                  end make-xml-element;
                                end method,
                                make(<range>, from: 1, to: sz));
  end make-xml-element;
end method convert-repeated-slot-to-xml;

define method non-virtual-slot-descriptors
    (class :: <class>) => (vector :: <vector>);
  as(<vector>, choose(method (sd :: <slot-descriptor>)
                        slot-allocation(sd) ~= #"virtual" 
                          & ~instance?(sd, <repeated-slot-descriptor>);
                      end,
                      slot-descriptors(class)));
end method non-virtual-slot-descriptors;

// FIXME:  This method uses the internal class <slot-descriptor> although
// it is exported.  Clean up the protocol so that this is not necessary.
//
define method convert-slots-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (slots :: <vector>);
  // format-out("convert-slots-to-xml\n");
  let class = object-class(object);
  let slot-descriptors :: <vector>
    = non-virtual-slot-descriptors(class);
  let slots :: <vector> = map-as(<simple-object-vector>,
                                 convert-slot-to-xml(converter, object, _, 
                                      preserve-identity?:  preserve-identity?),
                                 slot-descriptors);
  let repeated-slot = repeated-slot-descriptor(class);
  if (repeated-slot)
    slots := concatenate!(vector(convert-repeated-slot-to-xml
                                  (converter, object, repeated-slot)),
                          slots);
  end;
  slots;
end method convert-slots-to-xml;

define method convert-object-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity? = #t)
 => (document :: <xml>);
  // format-out("convert-object-to-xml(%=, %=)\n", converter, object);
  let (class-name, module-name, library-name)
    = converted-class-name-and-library(converter, object);
  // format-out("  converted-class\n");
  let result = make-xml-element ("dylan-object")
                 attribute "xmlns" => $dylan-default-namespace-name;
                 element ("class-name") { content class-name; }
                 element ("module-name") { content module-name; }
                 element ("library-name") { content library-name; }
                 raw-content convert-unique-id-to-xml
                               (converter, ensure-unique-id(converter, object));
               end make-xml-element;
  // format-out("  created result\n");
  with-current-xml-element (converter, object, result)
    let slot-elements :: <vector>
      = convert-slots-to-xml(converter, object, 
        preserve-identity?: preserve-identity?);
    result.node-children
      := concatenate!(result.node-children, slot-elements);
  end with-current-xml-element; 
  // format-out("  converted slots");
  result;
end method convert-object-to-xml;

define method convert-object-to-xml
    (converter :: <xml-converter>, object :: <unique-id>,
     #key preserve-identity? = #t)
 => (document :: <xml>);
  // format-out("convert-object-to-xml(%=, %=)\n", converter, object);
  let result = convert-unique-id-to-xml(converter, object);
  // format-out("end convert-object-to-xml\n");
  result;
end method convert-object-to-xml;

// Deep Magic ends here.

define method convert-to-xml
    (converter :: <xml-converter>, object, #key preserve-identity?)
 => (document :: <xml>);
  // format-out("convert-to-xml(%=, %=)\n", converter, object);
  if (found-cycle?(converter, object))
    convert-unique-reference-to-xml(converter, object);
  elseif (preserve-identity? & member?(object, converter.converted-objects))
    convert-unique-reference-to-xml(converter, object);
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
           make-xml-element (?tag)
//             attribute "xmlns" => $dylan-default-namespace;
           end make-xml-element;
         end;
         define simple-converters-to-xml ... end }
  { define simple-converters-to-xml
      convert ?type:expression => ?tag:expression; ... end }
    => { define method convert-object-to-xml
             (converter :: <xml-converter>, object :: ?type,
              #key preserve-identity?)
          => (document :: <xml>);
           make-xml-element (?tag)
//             attribute "xmlns" => $dylan-default-namespace;
             content format-to-string("%S", object);
           end make-xml-element;
         end;
         define simple-converters-to-xml ... end }
  { define simple-converters-to-xml
      convert ?type:expression => ?tag:expression using ?converter:expression; ... end }
    => { define method convert-object-to-xml
             (converter :: <xml-converter>, object :: ?type,
              #key preserve-identity?)
          => (document :: <xml>);  
           make-xml-element (?tag)
//             attribute "xmlns" => $dylan-default-namespace;
             content ?converter(object);
           end make-xml-element;
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


define method convert-from-xml
    (converter :: <xml-converter>,
     document :: <document>, #rest args, #key)
 => (object :: <object>);
  apply(convert-from-xml,   converter, root(document), args);
end method convert-from-xml;

define generic denotes-simple-element?
    (converter :: <xml-converter>, name) => (well? :: <boolean>);

define method denotes-simple-element?
    (converter :: <xml-converter>, name) => (no! :: singleton(#f));
  #f;
end method denotes-simple-element?;

define generic convert-simple-element-from-xml
    (converter :: <xml-converter>, element :: <element>)
 => (object :: <object>);

define macro simple-element-denotations-definer
  { define simple-element-denotations end }
 => { }
  { define simple-element-denotations 
      ?:name;
      ...
    end }
 => { define method denotes-simple-element?
          (converter :: <xml-converter>, name == ?#"name")
       => (yes! :: singleton(#t));
        #t;
      end;
      define simple-element-denotations
        ...
      end }
end macro simple-element-denotations-definer;

define simple-element-denotations
  number;
  integer;
  single-float;
  double-float;
  character;
  byte-character;
  unicode-character;
  string;
  byte-string;
  unicode-string;
  symbol;
  true;
  false;
  empty-list;
  unbound;
end;

define method convert-from-xml
    (converter :: <xml-converter>,
     elt :: <element>, #rest args, #key)
 => (object :: <object>);
  // format-out("convert-from-xml(%=)\n", elt);
  // HACK!
  // This should deal in qnames, etc.
  let sname = elt.name;
  // format-out("  convert-from-xml: sname = %=\n", sname);
  if (denotes-simple-element?(converter, sname))
    convert-simple-element-from-xml(converter, elt);
  else
    let result =
      select (sname by \==)
        #"dylan-object" =>
          // format-out("  converting Dylan object %=\n", elt);
          convert-dylan-object-from-xml(converter, elt);
        #"unique-id" =>
          // format-out("  converting unique-id %=\n", elt);
          convert-unique-id-from-xml(converter, elt);
        #"unique-reference" =>
          // format-out("  converting unique-reference %=\n", elt);
          convert-unique-reference-from-xml(converter, elt);
        otherwise =>
          error(make(<no-conversion-for-xml-element-error>,
                     xml-element: elt));
        end select;
    // Make sure that backpatches for result are running.
    ensure-unique-id(converter, result);
    result;
  end if;
end method convert-from-xml;

define method convert-simple-element-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (object :: <object>)
  // format-out("convert-simple-element-from-xml(%=)\n", elt);
  let value = if (elt.node-children.empty?)
                "";
              else
                elt.node-children[0].text ;
              end;
  // format-out("  value: %=\n", value);
  select (elt.name)
    #"number" => string-to-integer(value);
    #"integer" => string-to-integer(value);
    // No support for floats, yet
    #"character" => value[0];
    #"byte-character" => value[0];
    #"unicode-character" => value[0];
    #"string" => value;
    #"byte-string" => value;
    #"unicode-string" => value;
    #"symbol" => as(<symbol>, value);
    #"true" => #t;
    #"false" => #f;
    #"empty-list" => #();
    // What to do with unbound?
    otherwise => /* error(make(<simple-error>,
                            format-string: "Don't know how to convert %= to Dylan.",
                            format-arguments: vector(elt))); */
                 #"no-simple-element-converter-defined";
  end select;
end method convert-simple-element-from-xml;

define method simple-slot-value
    (converter :: <xml-converter>, elt :: <element>, name :: <string>)
 => (value :: <object>);
  // format-out("simple-slot-value(%=, %=)\n", elt, name);
  let found = elt[name];
  // format-out("  found: %= %=\n", object-class(found), found);
  let children = found.node-children;
  // format-out("  found: %= %=\n", object-class(children), children);
  // format-out("  found: %= %=\n", object-class(children[0]), children[0]);
  assert(children.size = 1);
  convert-simple-element-from-xml(converter, children[0]);
end method simple-slot-value;

// Examples for the representation of Dylan values are in the file
// test/object-representations.xml.

define method convert-unique-id-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (unique-id :: <unique-id>);
  // format-out("convert-unique-id-from-xml(%=, %=)\n", converter, elt);
  let converter-id = simple-slot-value(converter, elt, "converter-id");
  let serial-nr = simple-slot-value(converter, elt, "serial-number");
  make(<unique-id>, 
       converter-id: converter-id, serial-number: serial-nr);
end method convert-unique-id-from-xml;

define method convert-unique-reference-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (unique-id-or-referenced-object :: <object>);
  // format-out("convert-unique-reference-from-xml(%=, %=)\n", converter, elt);
  let converter-id = simple-slot-value(converter, elt, "cid");
  let serial-nr = simple-slot-value(converter, elt, "sn");
  let ref = make(<unique-id>, 
                 converter-id: converter-id, serial-number: serial-nr);
  let value = ref.referenced-object;
  value | ref;
end method convert-unique-reference-from-xml;

define method convert-dylan-object-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (object :: <object>);
  // format-out("convert-dylan-object-from-xml(%=)\n", elt);
  
  allocate-object-from-xml(converter, elt);
end method convert-dylan-object-from-xml;

define method as-class-name (name) => (class-name :: <byte-string>);
  concatenate("<", as(<byte-string>, name), ">");
end method as-class-name;

define class <repeated-slot-spec> (<object>)
  constant slot repeated-slot-spec-descriptor :: <repeated-slot-descriptor>,
    required-init-keyword: descriptor:;
  constant slot repeated-slot-spec-size-descriptor :: <slot-descriptor>,
    required-init-keyword: size-descriptor:;
  constant slot repeated-slot-spec-size-init-keyword,
    required-init-keyword: size-init-keyword:;
  constant slot repeated-slot-spec-size :: false-or(<integer>),
    required-init-keyword: size:;
  constant slot repeated-slot-spec-name :: false-or(<byte-string>),
    required-init-keyword: name:;
  constant slot repeated-slot-spec-elements :: <sequence>,
    required-init-keyword: elements:;
end class <repeated-slot-spec>;

define class <slot-spec> (<object>)
  constant slot slot-spec-descriptor :: <slot-descriptor>,
    required-init-keyword: descriptor:;
  constant slot slot-spec-name :: <byte-string>,
    required-init-keyword: name:;
  constant slot slot-spec-bound? :: <boolean>,
    required-init-keyword: bound?:;
  constant slot slot-spec-value-element :: <element>,
    required-init-keyword: value-element:;
  constant slot slot-spec-element :: <element>,
    required-init-keyword: element:;
end class <slot-spec>;


define method extract-repeated-slot-spec-from-xml
    (converter :: <xml-converter>, class :: <class>, elt :: <element>)
 => (repeated-slot-spec :: <repeated-slot-spec>);
  let descriptor :: <repeated-slot-descriptor>
    = repeated-slot-descriptor(class);
  let size-descriptor :: <slot-descriptor>
    = size-slot-descriptor(descriptor);
  let init-keyword = init-keyword(size-descriptor);

  make(<repeated-slot-spec>, 
       descriptor: descriptor,
       size-descriptor: size-descriptor,
       size-init-keyword: init-keyword,
       size: simple-slot-value(converter, elt, "size"),
       name: elt["name"].node-children[0].text,
       elements: map(method (e) e.node-children[0] end,
                     element(elt, "i", default: #(), always-sequence?: #t)));
end method extract-repeated-slot-spec-from-xml;

// Why, oh why, do I have to do this?!?
//
define method extract-object-elements-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (class :: <class>,
     unique-id :: <unique-id>,
     repeated-slot-spec :: false-or(<repeated-slot-spec>),
     slot-specs :: <vector> /* limited(<sequence>, of: <slot-spec>) */ );
  // format-out("extract-object-elements-from-xml(%=)\n", elt);

  let class-elt = elt["class-name"];
  let module-elt = elt["module-name"];
  let library-elt = elt["library-name"];

  // format-out(">>>>> a0 <<<<<\n");

  let id-elt = elt["unique-id"];
  let repeated-slot-elt = element(elt, "repeated-slot", default: #f);
  let slot-elts = element(elt, "slot", default: #(), always-sequence?: #t);

  // format-out(">>>>> a1 <<<<<\n");

  let raw-class-name = class-elt.node-children[0].text;
  let class-name = as-class-name(raw-class-name);
  let module-name = as(<byte-string>, module-elt.node-children[0].text);
  let library-name = as(<byte-string>, library-elt.node-children[0].text);

  // format-out(">>>>> a2 <<<<<\n");

  let unique-id = convert-unique-id-from-xml(converter, id-elt);
  let class = variable->class(class-name, module-name, library-name);

  // format-out(">>>>> a <<<<<\n");

  let repeated-slot-spec :: false-or(<repeated-slot-spec>)
    = if (repeated-slot-elt)
        extract-repeated-slot-spec-from-xml(converter, class, repeated-slot-elt);
      else
        #f;
      end if;

  // format-out(">>>>> b <<<<< %=\n", repeated-slot-spec);

  let slot-descriptors = non-virtual-slot-descriptors(class);
  let slot-descriptors-size = size(slot-descriptors);
  let slot-descriptor-table = make(<string-table>, size: slot-descriptors-size);

  // format-out(">>>>> c <<<<< %=\n", slot-descriptors);

  for (sd in slot-descriptors)
    let name = converted-slot-name(converter, sd);
    // format-out(">>>>> d <<<<< %=, %=\n", sd, name);
    slot-descriptor-table[name] := sd;
  end for;

  // format-out(">>>>> e <<<<<\n");

  let slot-elts-size = slot-elts.size;
  let slot-specs :: <sequence> = make(<stretchy-vector>);

  // format-out(">>>>> f <<<<<\n");

  let i = 0;
  for (elt in slot-elts)
    // format-out(">>>>> g <<<<<\n");

    let name = elt["name"].node-children[0].text;

    // format-out(">>>>> g1 <<<<<\n");
    let descriptor =  element(slot-descriptor-table, name, default: #f);
    if (descriptor)
      let slot-bound? = simple-slot-value(converter, elt, "initialized");
      let value-elt = elt["value"].node-children[0];
      // format-out("bound %=, value-elt: %=\n", slot-bound?, value-elt);
      slot-specs[i] := make(<slot-spec>,
                            name: name,
                            descriptor: descriptor,
                            bound?: slot-bound?,
                            value-element: value-elt,
                            element: elt);
      i := i + 1;
    end if;
  end for;
  // format-out(">>>>> h <<<<<\n");

  values(class, unique-id, repeated-slot-spec, slot-specs);
end method extract-object-elements-from-xml;

define method setter-or-backpatch
    (setter :: <function>, obj, result) => ();
    if (instance?(obj, <unique-id>))
      // format-out("Requesting backpatch for %= %= %=\n", result, obj, setter);
      request-backpatch(obj, 
                        method (new) => ();
                          // format-out("Backpatching %=, %=, %=\n", result, setter, new);
                          setter(new, result);
                        end);
      //                            setter(_, result));
    else
      setter(obj, result);
    end if;
end method setter-or-backpatch;

define method element-setter-or-backpatch
    (i :: <integer>, obj, result) => ();
    if (instance?(obj, <unique-id>))
      // format-out("Requesting backpatch for %= %= %=\n", result, obj, setter);
      request-backpatch(obj, 
                        method (new) => ();
                          // format-out("Backpatching %=, %=, %=\n", result, i, new);
                          result[i] := new;
                        end);
    else
      result[i] := obj;
    end if;
end method element-setter-or-backpatch;

define method allocate-object-from-xml
    (converter :: <xml-converter>, elt :: <element>)
 => (object :: <object>);
  // format-out("allocate-object-from-xml(%=)\n", elt);

  let (class, unique-id, repeated-slot-spec, slot-specs)
    = extract-object-elements-from-xml(converter, elt); 
  let result = allocate-raw-object(class, repeated-slot-spec);
  assign-unique-id(converter, result, unique-id);
  let slot-values = convert-slots-from-xml(converter, slot-specs);
  for (slot-spec in slot-specs, slot-value in slot-values)
    if (slot-spec.slot-spec-bound?)
      let setter = slot-spec.slot-spec-descriptor.slot-setter;
      let obj = referenced-object(slot-value);
      setter-or-backpatch(setter, obj, result);
    end if;
  end for;
  if (repeated-slot-spec)
    allocate-repeated-slot(converter, result, repeated-slot-spec);
  end if;
  result;
end method allocate-object-from-xml;

define method allocate-repeated-slot
    (converter :: <xml-converter>, object :: <object>, repeated-slot-spec) => ();
  let slot-values = map(convert-from-xml(converter, _), 
                        repeated-slot-spec.repeated-slot-spec-elements);
  for (value in slot-values, i from 0)
    element-setter-or-backpatch(i, value, object);
  end for;
end method allocate-repeated-slot;

define method allocate-raw-object
    (class :: <class>,
     repeated-slot-spec :: false-or(<repeated-slot-spec>))
 => (object :: <object>);
  // format-out("allocate-raw-object(%=, %=)\n", class, repeated-slot-spec);
  if (repeated-slot-spec)
    let repeated-slot :: false-or(<slot-descriptor>)
      = repeated-slot-spec-descriptor(repeated-slot-spec);
    let size :: false-or(<integer>)
      = repeated-slot-spec-size(repeated-slot-spec);
    if (repeated-slot)
      let size-slot :: <slot-descriptor> = size-slot-descriptor(repeated-slot);
      let size-keyword = init-keyword(size-slot);
      allocate-instance(class, vector(size-keyword, size));
    else
      system-allocate-simple-instance(class);
    end if;
  else
    system-allocate-simple-instance(class);
  end if;
end method allocate-raw-object;

define method convert-slots-from-xml
    (converter :: <xml-converter>, slot-specs :: <sequence>)
 => (slot-values :: <sequence>);
  // format-out("convert-slots-from-xml(%=)\n", slot-specs);
  map(convert-slot-from-xml(converter,  _), slot-specs);
end method convert-slots-from-xml;

define method convert-slot-from-xml
    (converter :: <xml-converter>, slot-spec :: <slot-spec>)
 => (slot-value :: <object>);
  if (slot-spec.slot-spec-bound?)
    convert-from-xml(converter, slot-spec.slot-spec-value-element);
  else
    #f;
  end if;
end method convert-slot-from-xml;

