Module:    date-time-play
Synopsis:  A brief description of the project.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define constant $application-name :: <byte-string> = "date-time-play";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "0";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;
