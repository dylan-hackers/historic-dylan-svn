module: meta-types
synopsis: types useful when collecting words, numbers, etc
author:  Douglas M. Auclair
copyright: (c) 2001, LGPL

// and constants as classes
define constant $letter        :: <byte-string> =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
define constant $digit         :: <byte-string> = "0123456789";
define constant $num-char      :: <byte-string> = concatenate($digit, ".-+eE");
define constant $space         :: <byte-string> = " \t\n\r";
define constant $punctuation   :: <byte-string> = "!?.,";
define constant $graphic-char  :: <byte-string> = "_@#$%^&*+=~/";

// except spaces and <punctuation>
define constant $any-char      :: <byte-string> =
  concatenate($graphic-char, $letter, $num-char);
