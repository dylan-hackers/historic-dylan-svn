module: meta-types
synopsis: types useful when collecting words, numbers, etc
author:  Douglas M. Auclair
copyright: (c) 2001, LGPL

// and constants as classes
define constant $letter = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			  "abcdefghijklmnopqrstuvwxyz";
define constant $digit = "0123456789";
define constant $num-char = concatenate($digit, ".-+eE");
define constant $space = " \t\n\r";
define constant $punctuation = "!?.,";
define constant $graphic-char = "_@#$%^&*+=~/";

// except spaces and <punctuation>
define constant $any-char = concatenate($graphic-char, $letter, $num-char);
