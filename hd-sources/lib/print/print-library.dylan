module: Dylan-User
author: Gwydion Project
synopsis: This file defines the Print library and modules.
copyright: See below.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//

/// This code was modified at Harlequin, Inc. to work with the new Streams
/// Library designed by Harlequin and CMU.
///


define library print
  use harlequin-dylan;
  use transcendentals;
  use threads;
  use streams;
  export pprint, print, print-internals;
end library print;

define module pprint
  create <pretty-stream>,
         \printing-logical-block,
         pprint-logical-block,
         pprint-newline,
         pprint-indent,
         pprint-tab,
         *default-line-length*,
         *print-miser-width*;
end module pprint;

define module print
  create print,
         print-object,
         print-to-string,
         *print-length*,
         *print-level*,
	 *print-circle?*,
	 *print-pretty?*,
	 *print-escape?*;
  create \printing-object,
         do-printing-object;
end module print;

define module print-internals
  use harlequin-dylan;
  use basic-locator-classes;
  use transcendentals;
  use threads;
  use dylan-extensions;
  use dylan-primitives;
  use byte-vector;
  use streams-internals;
  use pprint, export: all;
  use print, export: all;

  export *default-length*,
	 *default-level*,
	 *default-circle?*,
	 *default-pretty?*,
	 *default-escape?*,
	 *print-depth*;
end module print-internals;
