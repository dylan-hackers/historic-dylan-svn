Module:    genetics-editor
Synopsis:  A Creatures 2 genetics editor
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.
License:   See License.txt

define constant $application-name :: <byte-string> = "genetics-editor";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "1.1";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;
