module:    web-services-test
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

define test convert-unique-id-test
    (description: "convert-unique-id")
  // format-out("convert-unique-id-test\n");
  let xc = make(<xml-converter>);
  let my-a = make(<a>);
  // format-out("!!!!! a !!!!!\n");
  let id = ensure-unique-id(xc, my-a);
  // format-out("!!!!! b !!!!!\n");
  check("result is <unique-id>",
        instance?, id, <unique-id>);
  check-true("my-a has id", my-a.object-id);
  check-equal("attribute value is id",
              my-a.object-id, 
              id);
  check-equal("attribute saved in table",
              xc.unique-id-table[my-a], id);
  // format-out("!!!!! c !!!!!\n  %=\n", id);
  let result = convert-to-xml(xc, id);
  // format-out(" !!!!! d !!!!!\n");
  check("result is element", instance?, result, <element>);
  // format-out(" !!!!! e !!!!! %=\n", result);
  /*
  check-equal("result matches",
              "",
              format-to-string("%=", result));
  */
end test convert-unique-id-test;

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

define test convert-object-test-1
    (description: "converting <a>")
  let xc = make(<xml-converter>);
  let obj = make(<a>);
  // format-out(" !!!!! 1a !!!!! %=\n", obj);  
  let xml = convert-to-xml(xc, obj);
  // format-out(" !!!!! 1b !!!!! %=\n", xml);  
  // format-out("%=\n\n", xml);
end test convert-object-test-1;

define test convert-object-test-2
    (description: "converting <b>")
  let xc = make(<xml-converter>);
  let obj = make(<b>);
  let xml = convert-to-xml(xc, obj);
  // format-out("%=\n\n", xml);
end test convert-object-test-2;

define test convert-object-test-3
    (description: "converting <c>")
  let xc = make(<xml-converter>);
  let obj = make(<c>);
  let xml = convert-to-xml(xc, obj);
  // format-out("%=\n\n", xml);
end test convert-object-test-3;

define test convert-object-test-4
    (description: "converting <d>")
  let xc = make(<xml-converter>);
  let obj = make(<d>);
  let xml = convert-to-xml(xc, obj);
  // format-out("%=\n\n", xml);
end test convert-object-test-4;

define test convert-object-test-5
    (description: "converting <array>")
  let xc = make(<xml-converter>);
  let obj = make(<array>, size: 5);
  obj[1] := #t;
  obj[2] := make(<a>);
  let xml = convert-to-xml(xc, obj);
  // format-out("%=\n\n", xml);
end test convert-object-test-5;

define test convert-object-test-6
    (description: "converting complex <array>")
  let xc = make(<xml-converter>);
  let obj = make(<array>, size: 5);
  obj[1] := #t;
  obj[2] := make(<a>);
  obj[3] := obj;
  let xml = convert-to-xml(xc, obj);
  // format-out("%=\n\n", xml);
end test convert-object-test-6;

define test convert-unique-id-two-way-test
    (description: "converting unique-id to XML and back")
  let my-a = make(<a>);
  let xc = make(<xml-converter>);
  let id = make(<unique-id>, 
                converter-id: 1234, serial-number: 666, referenced-object: 7);
  let id-xml = convert-to-xml(xc, id);
  let id2 = convert-unique-id-from-xml(xc, id-xml);
  check("id = id2", \==, id, id2);
  check-equal("id refers to 7", 7, referenced-object(id2));
end test convert-unique-id-two-way-test;

define test convert-object-two-way-test-1
    (description: "converting <a>")
  let xc = make(<xml-converter>);
  let obj = make(<a>);
  let xml = convert-to-xml(xc, obj);
  let obj2 = convert-from-xml(xc, xml);
  check("object is an <a>", instance?, obj2, <a>);
  // format-out("%=\n\n", xml);
end test convert-object-two-way-test-1;

define test convert-object-two-way-test-2
    (description: "converting <b>")
  let xc = make(<xml-converter>);
  let obj = make(<b>);
  let xml = convert-to-xml(xc, obj);
  let obj2 = convert-from-xml(xc, xml);
  check("object is an <b>", instance?, obj2, <b>);
  // format-out("%=\n\n", xml);
  check("slot is unbound", 
        complement(slot-initialized?), obj2, unbound-slot);
end test convert-object-two-way-test-2;


define test convert-object-two-way-test-3
    (description: "converting <c>")
  let xc = make(<xml-converter>);
  let obj = make(<c>);
  let xml = convert-to-xml(xc, obj);
  let obj2 = convert-from-xml(xc, xml);
  check("object is an <c>", instance?, obj2, <c>);
  check-equal("slot 1", 1, c-a(obj2));
  check-equal("slot 2", #"foobar", c-b(obj2));
  check-equal("slot 3", "slot c-c", c-c(obj2));
  check-equal("slot 4", 'x', c-d(obj2));
  // format-out("%=\n\n", xml);
end test convert-object-two-way-test-3;

// This doesn't work since we are in effect
// reusing unique ids.
define test convert-object-two-way-test-4
    (description: "converting <d>")
  let xc = make(<xml-converter>);
  let obj = make(<d>);
  check("obj cyclic", \==, obj, obj.cycle);
  let xml = convert-to-xml(xc, obj);
  let obj2 = convert-from-xml(xc, xml);
  check("object is an <d>", instance?, obj2, <d>);
  check("obj2 cyclic", \==, obj2, obj2.cycle);
end test convert-object-two-way-test-4;

define test convert-object-two-way-test-5
    (description: "converting <d>")
  let xc = make(<xml-converter>);
  let str = "<dylan-object xmlns=\"http://www.opendylan.org/xml/0.1/\"><class-name>d"
            "</class-name><module-name>web-services-test</module-name><library-name>"
            "web-services-test</library-name><unique-id><converter-id><false/>"
            "</converter-id><serial-number><integer>12345</integer></serial-number>"
            "</unique-id><slot><name>cycle</name><initialized><true/></initialized>"
            "<value><unique-reference><cid><false/></cid><sn><integer>12345</integer></sn>"
            "</unique-reference></value></slot></dylan-object>";
  let xml = parse-document(str);
  let obj2 = convert-from-xml(xc, xml);
  check("object is an <d>", instance?, obj2, <d>);
  check("obj2 cyclic", \==, obj2, obj2.cycle);
end test convert-object-two-way-test-5;

define test convert-object-two-way-test-6
    (description: "converting complex <array>")
  let xc = make(<xml-converter>);
  let str = "<dylan-object xmlns=\"http://www.opendylan.org/xml/0.1/\"><class-name>simple-objec"
            "t-vector</class-name><module-name>dylan</module-name><library-name>dylan</librar"
            "y-name><unique-id><converter-id><false/></converter-id><serial-number><integer>1"
            "2</integer></serial-number></unique-id><repeated-slot><size><integer>5</integer>"
            "</size><name>vector-element</name><i><false/></i><i><true/></i><i><dylan-object "
            "xmlns=\"http://www.opendylan.org/xml/0.1/\"><class-name>a</class-name><module-name"
            ">web-services-test</module-name><library-name>web-services-test</library-name><u"
            "nique-id><converter-id><false/></converter-id><serial-number><integer>13</intege"
            "r></serial-number></unique-id></dylan-object></i><i><unique-reference><cid><fals"
            "e/></cid><sn><integer>12</integer></sn></unique-reference></i><i><false/></i></r"
            "epeated-slot><slot><name>vector-element</name><initialized><true/></initialized>"
            "<value><false/></value></slot></dylan-object>";
  let xml = parse-document(str);
  let a = convert-from-xml(xc, xml);
  check("object is an <array>", instance?, a, <array>);
  check-equal("0 is false", #f, a[0]);
  check-equal("1 is true", #t, a[1]);
  check("2 is an <a>", instance?, a[2], <a>);
  check("3 is a", \==, a, a[3]);
  check-equal("4 is false", #f, a[4]);
  // format-out("%=\n\n", xml);
end test convert-object-two-way-test-6;

define suite convert-to-xml-suite ()
/*
  test next-serial-number!-test;
  test object-id-test;
  test class-name-and-library-or-lose-test;
  test converted-class-name-and-library-test;
  test library-as-xml-namespace-test;
  test class-name-as-xml-name-test;
  test converted-slot-name-test;
  test convert-unique-id-test;
  test found-cycle?-test;
  test convert-object-test-1;
  test convert-object-test-2;
  test convert-object-test-3;
  test convert-object-test-4;
  test convert-object-test-5;
  test convert-object-test-6;
*/
  test convert-unique-id-two-way-test;
  test convert-object-two-way-test-1;
  test convert-object-two-way-test-2;
  test convert-object-two-way-test-3;
//  test convert-object-two-way-test-4;
  test convert-object-two-way-test-5;
  test convert-object-two-way-test-6;
end suite convert-to-xml-suite;

