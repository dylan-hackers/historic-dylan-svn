module: meta-types
synopsis: types useful when collecting words, numbers, etc
author:  Douglas M. Auclair
copyright: (c) 2001, LGPL

// and constants as classes
define constant <letter> = apply(one-of, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                 "abcdefghijklmnopqrstuvwxyz");

define constant <digit> = apply(one-of, "0123456789");

define constant <num-char>
  = type-union(<digit>, apply(one-of, ".-eE+"));

define constant <space> = apply(one-of, " \t\n\r");
// map(curry(as, <character>), #(#x20, #x9, #xD, #xA)));

define constant <graphic-char> = apply(one-of, "_!@#$%^&*()+=~/");

// except space
define constant <any-char> = type-union(<graphic-char>, <letter>,
                                        <num-char>);
