Module:    web-services-info
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define constant $web-services-name :: <byte-string> = "web-services";
define constant $web-services-major-version :: <byte-string> = "1";
define constant $web-services-minor-version :: <byte-string> = "0";

define method web-services-full-name () => (full-name :: <byte-string>)
  concatenate($web-services-name, " Version ",
              $web-services-major-version, ".",
              $web-services-minor-version)
end method web-services-full-name;

