Module:    xml-parser-test
Synopsis:  Tests for the XML converter.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define test namespace-creation-test ()
  let ns1 = make(<xml-namespace>, url: "http://example.com/foo/bar");
  let ns2 = make(<xml-namespace>, url: "http://example.com/foo/bar", short-name: "foo-bar");
  check("ns1 = ns2", \==, ns1, ns2);
  check-equal("short name", "foo-bar", namespace-short-name(ns1));

  let ns3 = make(<xml-namespace>);
  let ns4 = make(<xml-namespace>, short-name: "boo");
  check("ns3 ~== ns4", \~==, ns3, ns4);
  check-equal("ns3: no short name", namespace-short-name(ns3), #f);
  check-equal("ns4: short name", namespace-short-name(ns4), "boo");
end test namespace-creation-test;

define test default-default-namespace-test ()
  check-equal("default namespace: no url",
              namespace-url(default-namespace()), #f);
  check-equal("default namespace: no short-name",
              namespace-short-name(default-namespace()), #f);
end test default-default-namespace-test;

define test with-default-namespace-test ()
  let old-default = default-namespace();
  let new-default = make(<xml-namespace>, 
                         url: "http://xantira.com/test",
                         short-name: "xantira");
  check-false("old default not new default",
              old-default = new-default);
  check-true("old default is default",
             old-default = default-namespace());
  with-default-namespace (new-default)
    check-true("new default is default",
               new-default = default-namespace());
    check-true("new-default url", 
                namespace-url(new-default) = "http://xantira.com/test"); 
    check-equal("new-default short-name",
                namespace-short-name(new-default), "xantira");
  end with-default-namespace;
  check-false("old default not new default",
              old-default = new-default);
  check-true("old default is default",
             old-default = default-namespace());
end test with-default-namespace-test;

define test xml-namespace-test ()
    check-equal("xml url", 
                namespace-url($xml-namespace),
                "http://www.w3.org/XML/1998/namespace");
    check-equal("new-default short-name",
                namespace-short-name($xml-namespace), "xml");
end test xml-namespace-test;

define test with-local-namespace-test ()
  let my-x = #f;
  with-local-namespace ("x" => "www.my-x.com")
    my-x := find-local-namespace("x");
    check("x is namespace", instance?, my-x, <xml-namespace>);
    check-equal("x short name",
                "x", namespace-short-name(my-x));
    check-equal("x url",
                "www.my-x.com", namespace-url(my-x));
  end with-local-namespace;
  let my-y = find-local-namespace("x");
  check-false("x no longer local namespace", my-y = my-x);
end test with-local-namespace-test;

define test with-local-namespaces-test ()
  let my-x = #f;
  let my-y = #f;
  let my-z = #f;
  
  with-local-namespaces ("x" => "www.my-x.com",
                         "y" => "www.xantira.com",
                         "z" => "www.my-x.com")
    my-x := find-local-namespace("x");
    my-y := find-local-namespace("y");
    my-z := find-local-namespace("z");
    let def = find-local-namespace("def");

    check("x is namespace", instance?, my-x, <xml-namespace>);
    check("y is namespace", instance?, my-y, <xml-namespace>);
    check("z is namespace", instance?, my-z, <xml-namespace>);
    check("def is namespace", instance?, def, <xml-namespace>);

    check("x = z", \==, my-x, my-z);
    check("def is default", \==, def, default-namespace());

    check-equal("x short name", "x", namespace-short-name(my-x));
    check-equal("y short name", "y", namespace-short-name(my-y));
    check-equal("z short name", "x", namespace-short-name(my-z));

    check-equal("x url",  "www.my-x.com", namespace-url(my-x));
    check-equal("y url",  "www.xantira.com", namespace-url(my-y));
    check-equal("z url",  "www.my-x.com", namespace-url(my-z));
  end with-local-namespaces;
  let my-y = find-local-namespace("x");
  check-false("x no longer local namespace", my-y = my-x);
end test with-local-namespaces-test;

define test string-as-xml-name-test ()
  let (ab, bb, cb) = values(#f, #f, #f);
  with-local-namespaces ("a" => "www.aaa.com",
                         "b" => "www.bbb.com",
                         "c" => "www.aaa.com")
    ab := string-as-xml-name("a:b");
    bb := string-as-xml-name("b:b");
    cb := string-as-xml-name("c:b");
    check-equal("a:b", "a:b", xml-name-as-string(<string>, ab));
    check-equal("b:b", "b:b", xml-name-as-string(<string>, bb));
    check-equal("c:b", "a:b", xml-name-as-string(<string>, cb));
  end with-local-namespaces;
end test string-as-xml-name-test;

define test xml-namespace-attribute?-test ()
  let a1 = make(<attribute>, name: "foo", value: "");
  check-false("foo is no namespace attribute", 
              xml-namespace-attribute?(a1));
  let a2 = make(<attribute>, name: "xml:foo", value: "");
  check-false("xml:foo is no namespace attribute",
              xml-namespace-attribute?(a2));
  let a3 = make(<attribute>, name: "xmlnsfoo", value: "");
  check-false("xmlnsfoo is no namespace attribute",
              xml-namespace-attribute?(a3));
  let a4 = make(<attribute>, name: "xmlns", value: "");
  check-true("xmlns is a namespace attribute",
              xml-namespace-attribute?(a4));
  let a5 = make(<attribute>, name: "xmlns:a", value: "");
  check-true("xmlns:a is a namespace attribute",
              xml-namespace-attribute?(a5));
  let a6 = make(<attribute>, name: "xmlns:", value: "");
  check-true("xmlns: is a namespace attribute",
             xml-namespace-attribute?(a6));
end test xml-namespace-attribute?-test;

define test parse-document-test-1 ()
  let doc = "<?xml version=\"1.1\"?>"
            "<a xmlns=\"foobar\"/>";
  let tree = parse-document(doc);
  check("tree is document", instance?, tree, <document>);
  let root = tree.root;
  check("root is element", instance?, root, <element>);
  check-equal("root name is \"a\"", 
              "a", root.name-with-proper-capitalization);
  let qname = root.xml-name;
  check-true("root has qname", qname);
  check-equal("root local-name is \"a\"", "a",
              qname.name-local-name);
  let namespace = root.xml-name.name-namespace;
  check-true("root has namespace", namespace);
  check-equal("root namespace short name is \"#f\"",
              #f, namespace.namespace-short-name);
  check-equal("root url is \"foobar\"",
              "foobar", namespace.namespace-url);
end test parse-document-test-1;

define function check-qname (elt :: <element>, bare-name, local-name,
                             namespace-name, url)
  let elt-name = elt.name-with-proper-capitalization;
  check(format-to-string("%= is element", elt-name),
        instance?, elt, <element>);
  check-equal(format-to-string("%S name is %=", elt-name, bare-name), 
              bare-name, elt.name-with-proper-capitalization);
  let qname = elt.xml-name;
  check-true(format-to-string("%S has qname", elt-name), qname);
  check-equal(format-to-string("%S local-name is %=", elt-name, local-name),
              local-name,
              qname.name-local-name);
  let namespace = qname.name-namespace;
  check-true(format-to-string("%S has namespace", elt-name),
             namespace);
  check-equal(format-to-string("%S namespace short name is %=",
                               elt-name, namespace-name),
              namespace-name, namespace.namespace-short-name);
  check-equal(format-to-string("%S url is %=", elt-name, url),
              url, namespace.namespace-url);
end function check-qname;


define test parse-document-test-2 ()
  let doc = "<?xml version=\"1.1\"?>"
            "<a xmlns=\"http://foobar.de\" xmlns:foo=\"www.foo.com/xxx\">"
            "  <foo:b/><c/>"
            "</a>";
  let tree = parse-document(doc);
  check("tree is document", instance?, tree, <document>);
  let root = tree.root;
  check-qname(root, "a", "a", #f, "http://foobar.de"); 
  let children = root.node-children;
  let b = children[0];
  let c = children[1];
  check-qname(b, "foo:b", "b", "foo", "www.foo.com/xxx");
  check-qname(c, "c", "c", #f, "http://foobar.de");
end test parse-document-test-2;


define suite namespace-suite ()
  test namespace-creation-test;
  test default-default-namespace-test;
  test with-default-namespace-test;
  test xml-namespace-test;
  test with-local-namespace-test;
  test with-local-namespaces-test;
  test string-as-xml-name-test;
  test xml-namespace-attribute?-test;
  test parse-document-test-1;
  test parse-document-test-2;
end suite namespace-suite;

