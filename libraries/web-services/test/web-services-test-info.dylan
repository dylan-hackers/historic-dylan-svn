Module:    web-services-test
Synopsis:  Tests for the web service framework.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define constant $web-services-test-name :: <byte-string> = "web-services-test";
define constant $web-services-test-major-version :: <byte-string> = "1";
define constant $web-services-test-minor-version :: <byte-string> = "0";

define method web-services-test-full-name () => (full-name :: <byte-string>)
  concatenate($web-services-test-name, " Version ",
              $web-services-test-major-version, ".",
              $web-services-test-minor-version)
end method web-services-test-full-name;
