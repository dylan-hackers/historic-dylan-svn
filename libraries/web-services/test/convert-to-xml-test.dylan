Module:    web-services-test
Synopsis:  Tests for the XML converter.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

*debug?* := #f;

define class <a> (<object>)
end class <a>;

define class <b> (<object>)
  slot unbound-slot,
    init-keyword: slot:;
end class <b>;

define class <c> (<object>)
  slot c-a = 1;
  slot c-b = #"foobar";
  slot c-c = "slot c-c";
  slot c-d = 'x';
end class <c>;

define class <d> (<object>)
  slot cycle;
end class <d>;

define method initialize (self :: <d>, #key) => ();
  next-method();
  self.cycle := self;
end method initialize;


define test next-serial-number!-test
    (description: "Check next-serial-number!")
  let id = next-serial-number!();
  let next-id = next-serial-number!();
  check-equal("next-id is 1 higher", id + 1, next-id);
end test next-serial-number!-test;

define test object-id-test
    (description: "Check object-id and object-id!")
  let v = make(<stretchy-vector>);
  check-false("object-id is false", object-id(v));
  let w = object-id!(v);
  check("object-id is <unique-id>", instance?, w, <unique-id>);
  check("object-id = object-id!", \==, w, object-id(v));

  let v = make(<a>);
  check-false("object-id is false", object-id(v));
  let w = object-id!(v);
  check("object-id is <unique-id>", instance?, w, <unique-id>);
  check("object-id = object-id!", \==, w, object-id(v));
end test object-id-test;

define test class-name-and-library-or-lose-test
    (description: "Check class-name-and-library-or-lose")
  let (name, module, library) = class-name-and-library-or-lose(7);
  check-equal("class name of integer", "<integer>", name);
  check-equal("class module of integer", "dylan", library);
  check-equal("class library of integer", "dylan", library);
end test class-name-and-library-or-lose-test;

define test converted-class-name-and-library-test
    (description: "Check converted-class-name-and-library")
  let xc = make(<xml-converter>);
  let (name, module, library) = converted-class-name-and-library(xc, 7);

  check-equal("class name of integer", "integer", name);
  check-equal("class module of integer", "dylan", library);
  check-equal("class library of integer", "dylan", library);
end test converted-class-name-and-library-test;

define test library-as-xml-namespace-test ()
  let xc = make(<xml-converter>);
  let ns = library-as-xml-namespace(xc, "foo", "bar");
  check-equal("namespace url",
              concatenate($dylan-default-namespace-name, "foo/bar"),
              namespace-url(ns));
  check-equal("namespace short-name",
              "foo-bar",
              namespace-short-name(ns));
end test library-as-xml-namespace-test;

define test class-name-as-xml-name-test ()
  let xc = make(<xml-converter>);
  let qname = class-name-as-xml-name(xc, 12);
  check("qname is xml-name", instance?, qname, <xml-name>);
  check-equal("local-name is integer",
              "integer", name-local-name(qname));
  check-equal("namespace is dylan-dylan",
              $dylan-default-namespace,
              name-namespace(qname));
end test class-name-as-xml-name-test;

define test converted-slot-name-test
    (description: "Check converted-slot-name")
  check-equal("converted-slot-name of converted-objects",
              "converted-objects",
              converted-slot-name(make(<xml-converter>),
                                  converted-objects));
end test converted-slot-name-test;

define test convert-unique-id-attribute-test
    (description: "convert-unique-id")
  let xc = make(<xml-converter>);
  let my-a = make(<a>);
  let id = ensure-unique-id(xc, my-a);
  check("result is <unique-id>",
        instance?, id, <unique-id>);
  check-true("my-a has id", my-a.object-id);
  check-equal("attribute value is id",
              my-a.object-id, 
              id);
  check-equal("attribute saved in table",
              xc.unique-id-table[my-a], id);
  let result = convert-to-xml(xc, id);
  check("result is element", instance?, result, <element>);
  format-out("%=\n", result);
  check-equal("result matches",
              "",
              format-to-string("%=", result));
end test convert-unique-id-attribute-test;

define test convert-unique-reference-test
    (description: "convert-unique-reference")
  let xc = make(<xml-converter>);
  let my-a = make(<a>);
  // let xml = convert-object-to-xml(xc, my-a);
  let elt = convert-unique-reference(xc, my-a);
  check("elt is element",
        instance?, elt, <element>);
  check-equal("elt name",
              "xml-converter-unique-reference",
              elt.name-with-proper-capitalization);
  check-equal("elt children", #[], elt.node-children);
end test convert-unique-reference-test;

define test found-cycle?-test
    (description: "found-cycle?")
  let xc = make(<xml-converter>);
  let my-a = make(<a>);
  let xml = make(<element>, name: "foo");
  check-false("Found no cycle", found-cycle?(xc, my-a));
  check-true("Found cycle",
             with-current-xml-element (xc, my-a, xml)
               found-cycle?(xc, my-a);
             end);
  check-false("Found no cycle", found-cycle?(xc, my-a));
end test found-cycle?-test;

define function check-attribute (attributes, name, value)
  let index = find-key(attributes,
                       method (attr)
                         attr.name-with-proper-capitalization = name
                       end);
  check(format-to-string("Attribute %s exists", name),
        instance?, index, <integer>);
  let attr = attributes[index];
  // Probably superfluous, because we wouldn't have found attr
  // if this test fails.
  check-equal(format-to-string("Attribute %s has proper name", name),
              name,
              attr.name-with-proper-capitalization);
  check-equal(format-to-string("Attribute %s has value %s", name, value),
              value,
              attr.attribute-value);
end function check-attribute;

define function check-simple-element (elt, name, value, #key module = "dylan")
  // Put name in the Dylan module.
  let name = concatenate(module, "@dylan:", name);
  check("elt is element",
        instance?, elt, <element>);
  check-equal(format-to-string("name is \"%S\"", name),
              name,
              elt.name-with-proper-capitalization);
  let elt-value = elt.simple-element-value;
  check-equal(format-to-string("value is \"%S\"", value),
              value, elt-value); 
end function check-simple-element;

define function check-simple-child (children, name, type, value, #key module = "dylan")
  // This assumes that all member of Children are slots...
  let index = find-key(children,
                       method (child)
                         check-equal("child is slot",
                                     "slot",
                                     child.name-with-proper-capitalization);
                         let i = find-key(child.attributes,
                                          method (attr)
                                            attr.name-with-proper-capitalization = "name"
                                          end);
                         child.attributes[i].attribute-value = name;
                       end);
  check(format-to-string("Child %s exists", name),
        instance?, index, <integer>);
  let slot = children[index];
  let elts = slot.node-children;
  check-equal("slot has single element",
              1, elts.size);
  let elt = elts[0];
  check-simple-element(elt, type, value, module: module);
end function check-simple-child;

define test convert-simple-elements-test
    (description: "Test simple converters.")
  let xc = make(<xml-converter>);
  check-simple-element(convert-to-xml(xc, 123), "integer", "123");
  check-simple-element(convert-to-xml(xc, -123), "integer", "-123");
  check-simple-element(convert-to-xml(xc, 1.0s0), "single-float", "1.0000000");
  check-simple-element(convert-to-xml(xc, 1.0d0), "double-float", "1.0000000d0");
  check-simple-element(convert-to-xml(xc, 'x'), "byte-character", "x", 
                                      module: "dylan-extensions");
  check-simple-element(convert-to-xml(xc, "foo"), "byte-string", "foo");
  check-simple-element(convert-to-xml(xc, #"bar"), "symbol", "bar");
end test convert-simple-elements-test;

define test convert-slot-to-xml-test-1
    (description: "convert-slot-to-xml")
  let my-b = make(<b>);
  let xc = make(<xml-converter>);
  let descriptors = slot-descriptors(object-class(my-b));
  check-equal("my-b has only one slot",
              1, descriptors.size);
  let sd = descriptors[0];
  let elt = convert-slot-to-xml(xc, my-b, sd);
  check("elt is element",
        instance?, elt, <element>);
  check-equal("elt name is \"slot\"",
              "slot",
              elt.name-with-proper-capitalization);
  let attrs = attributes(elt);
  check-equal("elt has 2 attributes",
              2, attrs.size);
  check-attribute(attrs, "name", "unbound-slot");
  check-attribute(attrs, "initialized", "false");
  check-equal("elt has no children",
              0, elt.node-children.size);
end test convert-slot-to-xml-test-1;

define test convert-slot-to-xml-test-2
    (description: "convert-slot-to-xml")
  let my-b = make(<b>, slot: 123);
  let xc = make(<xml-converter>);
  let descriptors = slot-descriptors(object-class(my-b));
  check-equal("my-b has only one slot",
              1, descriptors.size);
  let sd = descriptors[0];
  let elt = convert-slot-to-xml(xc, my-b, sd);

  check("elt is element",
        instance?, elt, <element>);
  check-equal("elt name is \"slot\"",
              "slot",
              elt.name-with-proper-capitalization);

  let attrs = attributes(elt);
  check-equal("elt has 2 attributes",
              2, attrs.size);
  check-attribute(attrs, "name", "unbound-slot");
  check-attribute(attrs, "initialized", "true");

  let children = elt.node-children;
  check-equal("elt has 1 child",
              1, children.size);
  let child = children[0];
  check-simple-element(child, "integer", "123");
end test convert-slot-to-xml-test-2;

define test convert-object-to-xml-test-1
    (description: "Test convert-object-to-xml")
  let xc = make(<xml-converter>);
  let my-c = make(<c>);
  let elt = convert-object-to-xml(xc, my-c);
  let children = elt.node-children;
  check-equal("elt has 4 children",
              4, children.size);
  check-simple-child(children, "c-a", "integer", "1");
  check-simple-child(children, "c-b", "symbol", "foobar");
  check-simple-child(children, "c-c", "byte-string", "slot c-c");
  check-simple-child(children, "c-d", "byte-character", "x",
                     module: "dylan-extensions");
end test convert-object-to-xml-test-1;

define test convert-object-to-xml-test-2
    (description: "Test convert-object-to-xml")
  let xc = make(<xml-converter>);
  let my-d = make(<d>);
  let elt = convert-object-to-xml(xc, my-d);
  let attrs = elt.attributes;
  check-equal("no attributes", 0, attrs.size);
  let children = elt.node-children;
  check-equal("elt has 1 child", 1, children.size);
  let slot = children[0];
  check-equal("slot is slot",
              "slot",
              slot.name-with-proper-capitalization);  
  check-equal("slot has 1 child",
              1, slot.node-children.size);
  let child = slot.node-children[0];
  check-equal("child is unique-id",
              "xml-converter-unique-reference",
              child.name-with-proper-capitalization);
  let cas = child.attributes;
  check-equal("child has 1 attribute", 1, cas.size);
  check-attribute(cas, "unique-id", format-to-string("%S", object-id(my-d)));
end test convert-object-to-xml-test-2;

define test convert-to-xml-test-1
    (description: "Test convert-to-xml")
  let xc = make(<xml-converter>);
  let my-c = make(<c>);
  let elt = convert-to-xml(xc, my-c);
  let children = elt.node-children;
  check-equal("elt has 4 children",
              4, children.size);
  check-simple-child(children, "c-a", "integer", "1");
  check-simple-child(children, "c-b", "symbol", "foobar");
  check-simple-child(children, "c-c", "byte-string", "slot c-c");
  check-simple-child(children, "c-d", "byte-character", "x", 
                     module: "dylan-extensions");
end test convert-to-xml-test-1;

define test convert-to-xml-test-2
    (description: "Test convert-to-xml")
  let xc = make(<xml-converter>);
  let my-d = make(<d>);
  let elt = convert-to-xml(xc, my-d);
  let attrs = elt.attributes;
  check-equal("no attributes", 0, attrs.size);
  let children = elt.node-children;
  check-equal("elt has 1 child", 1, children.size);
  let slot = children[0];
  check-equal("slot is slot",
              "slot",
              slot.name-with-proper-capitalization);  
  check-equal("slot has 1 child",
              1, slot.node-children.size);
  let child = slot.node-children[0];
  check-equal("child is unique-id",
              "xml-converter-unique-reference",
              child.name-with-proper-capitalization);
  let cas = child.attributes;
  check-equal("child has 1 attribute", 1, cas.size);
  check-attribute(cas, "unique-id", format-to-string("%S", object-id(my-d)));
end test convert-to-xml-test-2;

define suite convert-to-xml-suite ()
  test next-serial-number!-test;
  test object-id-test;
  test class-name-and-library-or-lose-test;
  test converted-class-name-and-library-test;
  test library-as-xml-namespace-test;
  test class-name-as-xml-name-test;
  test converted-slot-name-test;
  test convert-unique-id-attribute-test;
  test convert-unique-reference-test;
  test found-cycle?-test;
/*
  test convert-simple-elements-test;
  test convert-slot-to-xml-test-1;
  test convert-slot-to-xml-test-2;
  test convert-object-to-xml-test-1;
  test convert-object-to-xml-test-2;
  test convert-to-xml-test-1;
  test convert-to-xml-test-2;
*/
end suite convert-to-xml-suite;

