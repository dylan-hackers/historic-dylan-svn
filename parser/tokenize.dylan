module: tokenize
copyright: see below


//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998 - 2005  Gwydion Dylan Maintainers
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

// Tokenizer interface.
// <tokenizer> -- exported.

define primary abstract class <tokenizer> (<object>)
end class <tokenizer>;

define generic get-token (tokenizer :: <tokenizer>)
    => (token :: <token>, srcloc :: <source-location>);

define generic unget-token
    (tokenizer :: <tokenizer>, token :: <token>, srcloc :: <source-location>)
    => ();

define generic note-potential-end-point (tokenizer :: <tokenizer>) => ();

define method note-potential-end-point (tokenizer :: <tokenizer>) => ();
end method note-potential-end-point;



// <line-convention>
//
define constant <line-convention>
  = one-of(#"unknown", #"unixy", #"mac-like", #"dosoid", #"dosoid-LF", #"leading-CR");

// <line-tokenizer> -- exported.
//
// An object knowing line-ending conventions.
// assumes line and line-setter functions defined on subclasses.
//
define primary abstract class <line-tokenizer> (<tokenizer>)
  slot line-convention :: <line-convention>,
    init-value: #"unknown", init-keyword: convention:;
end class <line-tokenizer>;

define generic line
  (tokenizer :: <line-tokenizer>) => (line :: <integer>);

define generic line-setter
  (to-line :: <integer>, tokenizer :: <line-tokenizer>) => (new-line ::  <integer>);

define generic examine-ending
  (ending :: one-of('\n', '\r'),
   tokenizer :: <line-tokenizer>) => ();


define generic examine-ending-aux
  (ending :: one-of('\n', '\r'),
   convention :: <line-convention>,
   tokenizer :: <line-tokenizer>) => ();

define inline method examine-ending-aux
  (ending :: one-of('\n', '\r'),
   convention :: <line-convention>,
   tokenizer :: <line-tokenizer>) => ();

  tokenizer.line := tokenizer.line + 1;
end;

define inline method examine-ending-aux
  (ending == '\n',
   convention == #"dosoid-LF",
   tokenizer :: <line-tokenizer>) => ();

  next-method();
  tokenizer.line-convention := #"dosoid";
end;

/*
This does not work, because lexer.posn is not correctly adjusted?

// TODO: handle \rsssss\n properly by looking at last line start
define inline method examine-ending-aux
  (ending == '\n',
   convention == #"dosoid-LF",
   lexer :: <lexer>) => ();

  let check :: singleton(13) = lexer.lexer-source.contents[lexer.posn - 1];
  next-method();
end;
*/

define inline method examine-ending-aux
  (ending == '\n',
   convention == #"dosoid",
   tokenizer :: <line-tokenizer>) => ();

  error("dosoid file containing two line-feeds in succession?");
end;

define inline method examine-ending-aux
  (ending == '\n',
   convention == #"mac-like",
   tokenizer :: <line-tokenizer>) => ();

  error("mac-like file containing line-feed?");
end;

define inline method examine-ending-aux
  (ending == '\n',
   convention == #"unknown",
   tokenizer :: <line-tokenizer>) => ();

  tokenizer.line-convention := #"unixy";
  examine-ending(ending, tokenizer);
end;

define inline method examine-ending-aux
  (ending == '\n',
   convention == #"leading-CR",
   tokenizer :: <line-tokenizer>) => ();

  tokenizer.line-convention := #"dosoid"; // already incremented
end;

define method examine-ending
  (ending == '\n', tokenizer :: <line-tokenizer>)
 => ();

  examine-ending-aux(ending, tokenizer.line-convention, tokenizer)
end;


define inline method examine-ending-aux
  (ending == '\r',
   convention == #"dosoid",
   tokenizer :: <line-tokenizer>) => ();

  tokenizer.line-convention := #"dosoid-LF";
end;

define inline method examine-ending-aux
  (ending == '\r',
   convention == #"dosoid-LF",
   tokenizer :: <line-tokenizer>) => ();

  error("dosoid file with two consecutive returns?");
end;

define inline method examine-ending-aux
  (ending == '\r',
   convention == #"unixy",
   tokenizer :: <line-tokenizer>) => ();

  error("unixy file containing carriage-return?");
end;

define inline method examine-ending-aux
  (ending == '\r',
   convention == #"unknown",
   tokenizer :: <line-tokenizer>) => ();

  next-method();
  tokenizer.line-convention := #"leading-CR";
end;

define inline method examine-ending-aux
  (ending == '\r',
   convention == #"leading-CR",
   tokenizer :: <line-tokenizer>) => ();

  tokenizer.line-convention := #"mac-like";
  examine-ending(ending, tokenizer);
end;


define method examine-ending
  (ending == '\r', tokenizer :: <line-tokenizer>)
 => ();

  examine-ending-aux(ending, tokenizer.line-convention, tokenizer)
end;

/*
define lout method-documentation examine-ending
  (ending == '\r', tokenizer :: <line-tokenizer>)
 => ();
   // graph for the line-ending state machine
   // TODO
end;
*/

