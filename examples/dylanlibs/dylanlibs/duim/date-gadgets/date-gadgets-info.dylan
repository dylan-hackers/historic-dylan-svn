Module:    date-gadgets
Synopsis:  DUIM gadgets for selecting and entering dates and times
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define constant $application-name :: <byte-string> = "date-gadgets";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "0";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;
