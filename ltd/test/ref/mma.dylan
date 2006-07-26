//  -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
//  Mock MMA (A Lisp language mathematica-like system)
// (c) copyright 1990, 1991 by Richard J. Fateman and Univ. of California
//  see enclosed notice (file copyright)
//  this file should be loaded in at COMPILE time for every file in
//  the mma package.  It should also be loaded in (once) when the
//  mma package is set up.
//  Mathematica, on which this is based,
//  is described in S. Wolfram: Mathematica, a
//  System for Doing Mathematics By Computer, (Addison-Wesley).
//  this line is not quite enough. Need to do, prior to compiling this
#f;

//  obsolete (provide 'mma)
define module #"mma" use #"common-lisp", #"excl"; end module mma;

"(in-package mma)";

//  this next line is not enough.. need to have these macros
//  available at compile time.
// (declaim (ftype macro ulist uconsm))
// LTD: Function LOAD not yet implemented.
load("ucons1");

define variable built-in-syms =
  //  these are the atoms used by the parser, evaluator, display,
  //  etc.  They must be the same in each of the separate packages,
  //  and so each package should be in this package ( :mma).
  #(#"addto", #"alias", #"alternatives", //  added 11/17/94
    #"and", #"apply", #"blank", #"blanknullsequence", #"blanksequence",
    #"compoundexpression", #"condition", #"delayed", #"derivative",
    #"divideby", #"dot", #"equal", #"exit", #"factorial", #"factorial2",
    #"function", #"greater", #"greaterequal", #"if", #"in", #"increment",
    #"inequality", #"integer", #"less", #"lessequal", #"list", #"map",
    #"mapall", #"messagename", #"noncommutativemultiply", #"not", #"null",
    #"optional", #"or", #"out", #"part", #"pattern", #"patterntest", #"plus",
    #"power", #"predecrement", #"preincrement", #"mput", #"putappend",
    #"real", #"repeated", #"repeatednull", #"replace", #"replaceall",
    #"replacerepeated", #"rule", #"ruledelayed", #"sameq", #"sequence",
    #"set", #"setdelayed", #"slot", #"slotsequence", #"subtractfrom",
    #"tagset", #"tagsetdelayed", #"times", #"timesby", #"unalias", #"unequal",
    #"unsameq", #"unset", #"upset", #"upsetdelayed", #"$line", #"quote");

//  from eval
define module mma export tl, mread1; end module mma;

