module: Dylan-User
author: Gwydion Project
synopsis: This file defines the Format library and modules.
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


define library format
  use dylan;
  use transcendentals;
  use harlequin-extensions;
  use threads;
  use streams;
  use print;
  export format, format-internals;
end library format;

define module format
  create format,
         format-to-string,
         print-message;
end module format;

define module format-internals
  use dylan;
  use dylan-extensions;
  use transcendentals;
  use harlequin-extensions;
  use threads;
  use byte-vector;
  use streams-internals;
  use print;
  use format, export: all;
end module format-internals;
