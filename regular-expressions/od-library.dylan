module:     dylan-user
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the Regular
            Expressions library.
copyright: see below

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// Revamped.  --cgay Dec 2007

define library regular-expressions
  use common-dylan;
  use string-extensions;
  use io,
    import: { format-out };  // for debugging only
  export
    regular-expressions;
end;

define module regular-expressions
  use common-dylan,
    exclude: {
      split    // todo -- just add a method to this one
    };
  use format-out;  // for debugging only
  use string-conversions;
  use character-type;
  use string-hacking;
  use %do-replacement;
  use %parse-string;
  use substring-search;
  export
    compile-regex,
    <regex>,
      regex-search,
      regex-search-strings,
      regex-group-count,
      regex-position,
      make-regex-positioner,
      regex-replace,
      make-regex-replacer,
    <regex-error>,
    <invalid-regex>,
      regex-pattern,
    <regex-match>,                   // results of a successful search
      <match-group>,
      match-group,
      match-groups,
      group-start,
      group-end,
      group-text,
      <invalid-match-group>,

    split,
    make-splitter,
    join;
  export
    split-string;   // ???
end module regular-expressions;

