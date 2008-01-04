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


// Added regex module with new API.  --cgay, June 2007

define library regular-expressions
  use common-dylan;
  use string-extensions;
  use table-extensions;
  export
    regex,                                             // new API
    regular-expressions;                               // old API
end library regular-expressions;

define module regex                  // new API module
  create
    compile-regex,
    regex-search,
    regex-search-strings,
    <regex>,
    <invalid-regex>,
      invalid-regex-pattern,
    <regex-match>,                   // results of a successful search
      regex-match-group,
      regex-match-group-count,
      group-start,
      group-end,
      group-text,
      <invalid-match-group>;
end module regex;

define module regular-expressions    // old API module
  create
    regexp-position, make-regexp-positioner,
    regexp-match,
    regexp-replace, make-regexp-replacer,
    translate, make-translator,
    split, make-splitter,
    join,
    <illegal-regexp>,
      regexp-pattern;

  create
    split-string;
end module regular-expressions;

define module regular-expressions-impl
  use common-dylan,
    exclude: { split };
  use string-conversions;
  use character-type;
  use string-hacking;
  use %do-replacement;
  use %parse-string;
  use substring-search;
  use regular-expressions;                             // API module
  use regex;
end module regular-expressions-impl;
