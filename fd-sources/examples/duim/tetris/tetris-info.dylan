Module:    tetris
Synopsis:  DUIM implementation of the game Tetris
Author:    Richard Tucker
Copyright: Copyright (c) 1998-2000 Functional Objects, Inc. All rights reserved.

define constant $application-name :: <byte-string> = "Tetris";
define constant $application-major-version :: <byte-string> = "1";
define constant $application-minor-version :: <byte-string> = "0";

define method application-full-name () => (full-name :: <byte-string>)
  concatenate($application-name, " Version ",
              $application-major-version, ".",
              $application-minor-version)
end method application-full-name;

