module: uri-test

define macro uri-reference-test-definer 
  { define uri-reference-test ?name:token
      ?uri:token
      ( ?scheme:token , ?authority:token ,
        ?path:token , ?query:token ,
        ?fragment:token )
      => ?result:token
   end
  } 
   =>
  { 
    define test "uri-reference-" ## ?name ()
      let uri = parse-uri(?uri);
      check-equal("scheme", uri.uri-scheme, ?scheme);
      check-equal("authority", uri.uri-authority, ?authority);
      check-equal("path", build-path(uri), ?path);
      check-equal("query", build-query(uri), ?query);
      check-equal("fragment", uri.uri-fragment, ?fragment);
      let target-uri = transform-uris($base-uri, uri);
      check-equal("target-uri", build-uri(target-uri), ?result);
    end
  }
end macro;

define suite uri-suite ()
  suite uri-transform-suite;
  suite uri-normalization-suite;
end;

define suite uri-transform-suite ()
  test uri-base-test;
  suite uri-transform-normal-suite;
  suite uri-transform-abnormal-suite
end;

define constant $base-uri = parse-uri("http://a/b/c/d;p?q");

define test uri-base-test ()
  check-equal("base-uri scheme", $base-uri.uri-scheme, "http");
  check-equal("base-uri authority", $base-uri.uri-authority, "a");
  check-equal("base-uri path", build-path($base-uri), "/b/c/d;p");
  check-equal("base-uri query", build-query($base-uri), "q");
  check-equal("base-uri fragment", $base-uri.uri-fragment, "");
end;

define suite uri-transform-normal-suite ()
  test uri-reference-normal-test-1;
  test uri-reference-normal-test-2;
  test uri-reference-normal-test-3;
  test uri-reference-normal-test-4;
  test uri-reference-normal-test-5;
  test uri-reference-normal-test-6;
  test uri-reference-normal-test-7;
  test uri-reference-normal-test-8;
  test uri-reference-normal-test-9;
  test uri-reference-normal-test-10;
  test uri-reference-normal-test-11;
  test uri-reference-normal-test-12;
  test uri-reference-normal-test-13;
  test uri-reference-normal-test-14;
  test uri-reference-normal-test-15;
  test uri-reference-normal-test-16;
  test uri-reference-normal-test-17;
  test uri-reference-normal-test-18;
  test uri-reference-normal-test-19;
  test uri-reference-normal-test-20;
  test uri-reference-normal-test-21;
  test uri-reference-normal-test-22;
  test uri-reference-normal-test-23;
end;

define uri-reference-test normal-test-1
  "g:h" ("g", "", "h", "", "") => "g:h"
end;

define uri-reference-test normal-test-2
  "g" ("", "", "g", "", "") => "http://a/b/c/g"
end;

define uri-reference-test normal-test-3
  "./g" ("", "", "./g", "", "") => "http://a/b/c/g"
end;

define uri-reference-test normal-test-4
  "g/" ("", "", "g/", "", "") => "http://a/b/c/g/"
end;

define uri-reference-test normal-test-5
  "/g" ("", "", "/g", "", "") => "http://a/g"
end;

define uri-reference-test normal-test-6
  "//g" ("", "g", "", "", "") => "http://g"
end;

define uri-reference-test normal-test-7
  "?y" ("", "", "", "y", "") => "http://a/b/c/d;p?y"
end;

define uri-reference-test normal-test-8
  "g?y" ("", "", "g", "y", "") => "http://a/b/c/g?y"
end;

define uri-reference-test normal-test-9
  "#s" ("", "", "", "", "s") => "http://a/b/c/d;p?q#s"
end;

define uri-reference-test normal-test-10
  "g#s" ("", "", "g", "", "s") => "http://a/b/c/g#s"
end;

define uri-reference-test normal-test-11
  "g?y#s" ("", "", "g", "y", "s") => "http://a/b/c/g?y#s"
end;

define uri-reference-test normal-test-12
  ";x" ("", "", ";x", "", "") => "http://a/b/c/;x"
end;

define uri-reference-test normal-test-13
  "g;x" ("", "", "g;x", "", "") => "http://a/b/c/g;x"
end;

define uri-reference-test normal-test-14
  "g;x?y#s" ("", "", "g;x", "y", "s") => "http://a/b/c/g;x?y#s"
end;

define uri-reference-test normal-test-15
  "" ("", "", "", "", "") => "http://a/b/c/d;p?q"
end;

define uri-reference-test normal-test-16
  "." ("", "", ".", "", "") => "http://a/b/c/"
end;

define uri-reference-test normal-test-17
  "./" ("", "", "./", "", "") => "http://a/b/c/"
end;

define uri-reference-test normal-test-18
  ".." ("", "", "..", "", "") => "http://a/b/"
end;

define uri-reference-test normal-test-19
  "../" ("", "", "../", "", "") => "http://a/b/"
end;

define uri-reference-test normal-test-20
  "../g" ("", "", "../g", "", "") => "http://a/b/g"
end;

define uri-reference-test normal-test-21
  "../.." ("", "", "../..", "", "") => "http://a/"
end;

define uri-reference-test normal-test-22
  "../../" ("", "", "../../", "", "") => "http://a/"
end;

define uri-reference-test normal-test-23
  "../../g" ("", "", "../../g", "", "") => "http://a/g"
end;

define suite uri-transform-abnormal-suite ()
  test uri-reference-abnormal-test-1;
  test uri-reference-abnormal-test-2;
  test uri-reference-abnormal-test-3;
  test uri-reference-abnormal-test-4;
  test uri-reference-abnormal-test-5;
  test uri-reference-abnormal-test-6;
  test uri-reference-abnormal-test-7;
  test uri-reference-abnormal-test-8;
  test uri-reference-abnormal-test-9;
  test uri-reference-abnormal-test-10;
  test uri-reference-abnormal-test-11;
  test uri-reference-abnormal-test-12;
  test uri-reference-abnormal-test-13;
  test uri-reference-abnormal-test-14;
end;

define uri-reference-test abnormal-test-1
  "../../../g" ("", "", "../../../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-2
  "../../../../g" ("", "", "../../../../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-3
  "/./g" ("", "", "/./g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-4
  "/../g" ("", "", "/../g", "", "") => "http://a/g"
end;

define uri-reference-test abnormal-test-5
  "g." ("", "", "g.", "", "") => "http://a/b/c/g."
end;

define uri-reference-test abnormal-test-6
  ".g" ("", "", ".g", "", "") => "http://a/b/c/.g"
end;

define uri-reference-test abnormal-test-7
  "g.." ("", "", "g..", "", "") => "http://a/b/c/g.."
end;

define uri-reference-test abnormal-test-8
  "..g" ("", "", "..g", "", "") => "http://a/b/c/..g"
end;

define uri-reference-test abnormal-test-9
  "./../g" ("", "", "./../g", "", "") => "http://a/b/g"
end;

define uri-reference-test abnormal-test-10
  "./g/." ("", "", "./g/.", "", "") => "http://a/b/c/g/"
end;

define uri-reference-test abnormal-test-11
  "g/./h" ("", "", "g/./h", "", "") => "http://a/b/c/g/h"
end;

define uri-reference-test abnormal-test-12
  "g/../h" ("", "", "g/../h", "", "") => "http://a/b/c/h"
end;

define uri-reference-test abnormal-test-13
  "g;x=1/./y" ("", "", "g;x=1/./y", "", "") => "http://a/b/c/g;x=1/y"
end;

define uri-reference-test abnormal-test-14
  "g;x=1/../y" ("", "", "g;x=1/../y", "", "") => "http://a/b/c/y"
end;

define suite uri-normalization-suite ()
  test uri-path-segment-normalization-test;
end;

define test uri-path-segment-normalization-test ()
  check-equal("path", "/a/c", remove-dot-segments("/a/b/../c"));
  check-equal("path", "/a/b/c", remove-dot-segments("/a/b/./c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/./b/../c"));
  check-equal("path", "/b/c", remove-dot-segments("/a/../b/./c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/b/./../c"));
  check-equal("path", "/a/c", remove-dot-segments("/a/b/.././c"));
  check-equal("path", "/b/c", remove-dot-segments("/a/../../../b/c"));

  check-equal("path", "a/c", remove-dot-segments("a/b/../c"));
  check-equal("path", "a/b/c", remove-dot-segments("a/b/./c"));
  check-equal("path", "a/c", remove-dot-segments("a/./b/../c"));
  check-equal("path", "b/c", remove-dot-segments("a/../b/./c"));
  check-equal("path", "a/c", remove-dot-segments("a/b/./../c"));
  check-equal("path", "a/c", remove-dot-segments("a/b/.././c"));
  check-equal("path", "b/c", remove-dot-segments("a/../../../b/c"));
end;

begin
  run-test-application(uri-suite); //, arguments: #("-debug"));
end;
