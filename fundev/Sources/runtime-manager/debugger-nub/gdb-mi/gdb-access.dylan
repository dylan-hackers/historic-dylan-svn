module: gdb-access
synopsis: 
author: gabor@mac.com
copyright: see below
qoute-of-the-day: Intelligence is the ability to avoid work, yet getting work done. -- Linus Torvalds


//======================================================================
//
// Copyright (c) 2004  Gwydion Dylan Maintainers
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
//    the contribution of the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// the Gwydion Dylan Maintainers make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

//--------------------------------------------------
// TESTCASES:
// these should end up in a TestWorks suite
//

define constant test-transcript
  = #[
  
  
#[
       "34-eee"
       "34^error,msg=\"Undefined MI command: eee\""
],
#[
"-break-after 1 7"
"&\"No breakpoint number 1.\n\""
"^error,msg=\"No breakpoint number 1.\""
],

#[
"-break-list"
"^done,BreakpointTable={}"
],

#[
"-break-info 1"
"^done,BreakpointTable={}"
],

#[
],

#[
],

#[
],

#[
],

#[
],

#[
],

#[
],

#[
],

#[
]


     ];

//--------------------------------------------------
// MI-sessions
// you can have any number of sessions open with gdb
//

define class <gdb-mi-session>(<object>)
  // 
  // pending: associating sent (but not yet answered) tokens with commands
  //
  slot session-pending :: <deque> = make(<deque>);// ###bug### when writing <dequeue>
  //
  // history: a sequence of tokens, including the pending ones
  //
  slot session-history :: <stretchy-vector> = make(<stretchy-vector>);
  //
  // results: associating results with tokens
  // 
  slot session-results :: <table> = make(<table>);
  //
  // commands: associating commands with tokens
  // 
  slot session-commands :: <table> = make(<table>);
end;

define abstract class <command>(<object>)
  slot command-token :: false-or(<integer>), init-keyword: token:, init-value: #f;
  class slot current-token :: <integer>;//, init-value: 0;// ###bug### = 0;
end;

define class <cli-command>(<command>)
  constant slot cli-command :: <byte-string>, required-init-keyword: command:;
end;


define constant <operation> = <symbol>;
define constant <positive> = <integer>; // for now... ####


define abstract class <mi-command>(<command>)
end;


define abstract class <mi-result>(<object>)
end;

define generic parse-result(result-class :: <class>, mi-reply :: <byte-string>) => result :: <mi-result>;


define macro mi-operation-definer
  // low-level definers

  { define class-for-sequential mi-operation ?:name(?sequence-slot) end }
  =>
  {
    define class ?name ## "-<command>"(<mi-command>)
      ?sequence-slot;
    end;
  }
  
  
  { define class-for-regular mi-operation ?:name(?regular-class-options) end }
  =>
  {
    define class ?name ## "-<command>"(<mi-command>)
      ?regular-class-options
    end;
  }


  { define creator-for-sequential mi-operation ?:name(?rest-sequence, ?sequence-arg) end }
  =>
  {
    define function ?name(?rest-sequence)
      make(?name ## "-<command>", ?#"name", ?sequence-arg)
    end;
  }

  { define emitter-for-sequential mi-operation ?:name(?rest-sequence, ?sequence-arg) end }
  =>
  {
  }

  { define result-for-sequential mi-operation ?:name(?p-sequence:*) end }
  =>
  {
    define class ?name ## "-<result>"(<mi-result>)
    end;
  }

  { define parser-for-sequential mi-operation ?:name(?p-sequence:*) end }
  =>
  {
    define method parse-result(result-class == ?name ## "-<result>", mi-reply :: <byte-string>) => result :: <mi-result>;
      block (return)
        local method finish(inp, matched, more)
                let got :: singleton(1) = matched.size;
                return(matched.head(?name ## "-<result>"));
              end;
         
         let parsers = list(?p-sequence, finish);
         parsers.head(mi-reply, #(), parsers.tail);
      end block;
    end method;
  }



  // high-level definers
  //
  { define mi-operation ?:name; ?parse-sequence end }
  =>
  {
    // TBD
  }

  // sequences of something
  { define mi-operation ?:name((?argument)); ?parse-sequence end }
  =>
  {
    define class-for-sequential mi-operation ?name(?argument) end;
    define creator-for-sequential mi-operation ?name(?argument, ?argument) end;
    define emitter-for-sequential mi-operation ?name(?argument, ?argument) end;
    define result-for-sequential mi-operation ?name(?parse-sequence) end;
    define parser-for-sequential mi-operation ?name(?parse-sequence) end
  }

  // options in front of something
  { define mi-operation ?:name [ ?option ] ?options; ?parse-sequence end }
  =>
  {
    define class-for-regular mi-operation ?name([ ?option ] ?options) end;
//    define creator-for-sequential mi-operation ?name(?sequence, ?sequence) end;
//    define emitter-for-sequential mi-operation ?name(?sequence, ?sequence) end;
//    define result-for-sequential mi-operation ?name(?parse-sequence) end;
//    define parser-for-sequential mi-operation ?name(?parse-sequence) end
  }

  { define mi-operation ?:name { ?alternate } ?options; ?parse-sequence end }
  =>
  {
    define class-for-regular mi-operation ?name(?alternate ?options) end;
//    define creator-for-sequential mi-operation ?name(?sequence, ?sequence) end;
//    define emitter-for-sequential mi-operation ?name(?sequence, ?sequence) end;
//    define result-for-sequential mi-operation ?name(?parse-sequence) end;
//    define parser-for-sequential mi-operation ?name(?parse-sequence) end
  }

  // arguments
  { define mi-operation ?:name(?argument, ?arguments) ?options; ?parse-sequence end }
  =>
  {
///////    define class-for-regular mi-operation ?name(?argument ?arguments) end;
    // ---- more
  }

  // auxiliary patterns
  //
  sequence-slot:
    { [ ?ignore:name ?:name :: ?:expression;(?blurb:*) ] }
    => { slot ?name :: limited(<vector>, of: ?expression), required-init-keyword: ?#"name" }

  rest-sequence:
    { [ ?ignore:name ?:name :: ?:expression;(?blurb:*) ] }
    => { #rest ?name :: ?expression }

  sequence-arg:
    { [ ?ignore:name ?:name :: ?:expression;(?blurb:*) ] }
    => { ?name }

  parse-sequence:
    {} => {}
    { ?parse-directive; ... } => { ?parse-directive, ... }
  
  parse-directive:
    { resulting ?:name } => { method(inp, matched, more) => (); let m = match(inp, "^" ?"name"); more.head(inp, pair(if (m & m.empty?) make.curry else #f.always end if, matched), more.tail) end method }
    { resulting ?:name => ?parser:expression } => { method(inp, matched, more) => (); let m = match(inp, "^" ?"name"); more.head(inp, pair(if (m) rcurry(?parser, m) else #f.always end if, matched), more.tail) end method }

  options:
    {} => {}
    { [ ?option ] ... } => { [ ?option ] ... }
    { { ?alternate } ... } => { ?alternate ... }

  option:
    { ?:name } => { ?name ?name :: <boolean>;(init-keyword: ?#"name", init-value: #f) }
    { ?flag:name ?:name } => { ?flag ?name :: <boolean>;(init-keyword: ?#"flag", init-value: #f) }
    { ?flag:name ?:name :: ?:expression } => { ?flag ?name :: ?expression;(required-init-keyword: ?#"flag") }
    { ?:name :: ?:expression } => { ?name ?name :: ?expression;(required-init-keyword: ?#"name") }
    
  alternate:
    { } => { }
    { ?option; ... } => { [ ?option ] ... }

  arguments:
    {} => {}
    { ?argument, ... } => { ?argument ... }

  argument:
    { ?:name :: ?:expression } => { [ ?name ?name :: ?expression;(required-init-keyword: ?#"name") ] }

  regular-class-options:
    {} => {}
    { [ ?regular-class-option ] ... } => { ?regular-class-option; ... }

  regular-class-option:
    { ?flag:name ?:name :: ?:expression;(?stuff:*) } => { constant slot ?name :: ?expression, ?stuff }
end;

/*
define macro mi-parser-definer
  { define mi-parser ?:name(?:*) ?:* end }
  =>
  {}
end;
*/

// ####################################
// GDB/MI Breakpoint table commands

define method parse-breakpoint-table()
end;

define method placeholder() /// ###
end;

define mi-operation break-after(number :: <positive>, count :: <positive>);
  resulting error => report;
  resulting done => parse-breakpoint-table;
end;

define mi-operation break-condition(number :: <positive>, expr :: <mi-expression>);
  resulting error => report;
  resulting done => placeholder; /// ###
end;

define mi-operation break-delete((breakpoint :: <positive>));
  resulting error => report;
  resulting done;
end;

define mi-operation break-disable((breakpoint :: <positive>));
  resulting error => report;
  resulting done;
end;

define mi-operation break-enable((breakpoint :: <positive>));
  resulting error => report;
  resulting done;
end;

define mi-operation break-info(breakpoint :: <positive>)
end;

define constant <mi-expression> = <integer>; // #####
define constant <mi-address> = <symbol>; // #####
define constant <mi-location> = <integer>; // #####

define mi-operation break-insert
    [ t ]
    [ h ]
    [ r ]
    [ c condition :: <mi-expression> ]
    [ i ignore-count :: <positive> ]
    [ p thread :: <positive> ]
    { line :: <positive>; addr :: <mi-address> };

  resulting done => parse-breakpoint;// bkpt={number="1",addr="0x0001072c",file="recursive2.c",line="4"}
end;




define generic parse-as(inp :: <byte-string>, type :: <type>)
 => (remaining :: false-or(<byte-string>), value);

define method parse-as(inp :: <byte-string>, type :: <type>)
 => (remaining :: false-or(<byte-string>), value);
 ugh:
end;


define method parse-as(inp :: <byte-string>, type == <integer>)
 => (remaining :: false-or(<byte-string>), value);
  let m = match(inp, "\"");
  let pos = m & subsequence-position(m, "\"");
  pos & values(copy-sequence(m, start: pos + 1), copy-sequence(m, end: pos).string-to-integer)
end;

define method parse-as(inp :: <byte-string>, type == <byte-string>)
 => (remaining :: false-or(<byte-string>), value);
  let m = match(inp, "\"");
  let pos = m & subsequence-position(m, "\""); // ## escaped?
  pos & values(copy-sequence(m, start: pos + 1), copy-sequence(m, end: pos))
end;

define method parse-as(inp :: <byte-string>, type == <mi-address>)
 => (remaining :: false-or(<byte-string>), value);
  let (m, val) = match-c-string(inp);
  m & values(m, as(<symbol>, val))
end;

define macro mi-parser-definer
  // Low-level definers
  //
  { define tuple-value mi-parser ?:name(?tag:name) {?tuple-field; ?tuple-fields} {?tuple-keyword/values} end }
   =>
  {
    define function "parse-" ## ?name(mi-tuple :: <byte-string>, matched :: <list>, more) => ();
      let m = match(mi-tuple, ?"tag" "={");
      ?tuple-field;
      ?tuple-fields;
      more.head(mi-tuple,
                if (m)
                  pair(method() values(m, apply(make, ?name ## "-<tuple>", vector(?tuple-keyword/values))) end method, matched)
                else
                  matched
                end if,
                more.tail)
    end
  }
  
  { define class-for-tuple mi-parser ?:name(?tag:name) {?tuple-slots} end }
   =>
  {
    define class ?name ## "-<tuple>"(<object>)
      ?tuple-slots
    end
  }

  // High-level definers
  //
  { define mi-parser ?:name(?tag:name) (?fields) end }
   =>
  {
    define tuple-value mi-parser ?name(?tag) {?fields} {?fields} end;
    define class-for-tuple mi-parser ?name(?tag) {?fields} end
  }
  
  fields:
    { } => { }
    { ?field; ... } => { ?field; ... }
  
  field:
// optional field   { [ ?:name :: ?:expression ] } => { ?name false-or(?expression) }
    { ?:name :: ?:expression } => { ?name ?expression }

  tuple-fields:
    { } => { let m = m & match(m, "}") }
    { ?tuple-field; ... } => { let m = m & match(m, ","); ?tuple-field; ... }

  tuple-field:
    { ?tag:name ?type:expression }
     =>
    {
      let m = m & match(m, ?"tag" "=");
      let (m, ?tag :: false-or(?type)) = m & parse-as(m, ?type)
    }

  tuple-keyword/values:
    { } => { }
    { ?tag:name ?:expression; ... } => { ?#"tag", ?tag, ... }

  tuple-slots:
    { } => { }

    { ?tag:name ?type:expression; ... } 
     =>
    { slot /* ?=?"name" ## */ ?tag :: ?type, required-init-keyword: ?#"tag" }
end;


define mi-parser breakpoint(bkpt)
  (
    number :: <positive>;
    addr :: <mi-address>;
    file :: <byte-string>;
    line :: <positive>
  )
end;


//###### can be removed
// ###
begin

  let inp = "bkpt={number=\"1\",addr=\"0x0001072c\",file=\"recursive2.c\",line=\"4\"}1234";
  
  block (outta-here)
    local method final(rest, matches, more)
            let igno :: singleton(1) = matches.size;
            let (rem, tup) = matches.head();
            format-out("parsed: %=\nremainder: %=\n\n", tup, rem);
            outta-here();
          end;


// format-out(match(inp, "bkpt={"));

    parse-breakpoint(inp, #(), list(final))
  end block;
end begin;


define mi-operation break-list;
  resulting done => parse-breakpoint-table;
end;

/*
define mi-parser mi-column
(
  width :: <positive>;
  alignment :: <integer>;
  col_name :: <byte-string>;
  colhdr :: <byte-string>;
)
end;
*/

/*
define mi-parser breakpoint-table(BreakpointTable)
  (
    nr_rows :: <positive>;
    nr_cols :: <positive>;
    hdr :: [ mi-column-<tuple> ];
    body :: [ breakpoint-<tuple> ];
  )

  validate (breakpoint-table.nr_rows = body.size
            & breakpoint-table.nr_cols = hdr.size)
end;
*/

/*
BreakpointTable={nr_rows="1",nr_cols="6",
hdr=[
{width="3",alignment="-1",col_name="number",colhdr="Num"},
{width="14",alignment="-1",col_name="type",colhdr="Type"},
{width="4",alignment="-1",col_name="disp",colhdr="Disp"},
{width="3",alignment="-1",col_name="enabled",colhdr="Enb"},
{width="10",alignment="-1",col_name="addr",colhdr="Address"},
{width="40",alignment="2",col_name="what",colhdr="What"}
],
body=[bkpt={number="1",type="breakpoint",disp="keep",enabled="y",
addr="0x000100d0",func="main",file="hello.c",line="5",times="0",
ignore="3"}]
}
*/

define mi-operation break-watch{a; r};
  resulting done => parse-reason;
end;

// ####################################
// GDB/MI Data Manipulation



/*
define mi-operation data-disassemble(
    [ s start-addr -e end-addr ]
  | [ -f filename -l linenum [ -n lines ] ]
  -- mode
  )

  resulting done => parse-instructions; // asm_insns
end;
*/

define mi-operation data-disassemble(mode :: one-of(source:, raw:))
    [ s start-addr /* -e end-addr */ ]
    [ f filename /* -l linenum [ -n lines ] */];

  resulting done => parse-instructions; // asm_insns
end;


define mi-operation data-evaluate-expression(expr :: <mi-expr>)
  resulting done => parse-value; // ="0xefffeb7c"
end;


define mi-operation data-list-changed-registers;
  resulting done => parse- // changed-registers=["0","1","2","4","5","6","7","8","9",
// "10","11","13","14","15","16","17","18","19","20","21","22","23",
// "24","25","26","27","28","30","31","64","65","66","67","69"]
end;


define method parse-registers();
end;


define mi-operation data-list-register-names((regno :: <positive>));
  resulting done => parse-registers; // -names=["r1","r2","r3"]
end;

define mi-operation data-list-register-values(fmt :: <character>) ((regno :: <positive>));
end;


// ####################
// GDB/MI Program control

define mi-operation exec-abort;

end;

define mi-operation exec-arguments(args)

end;

define mi-operation exec-continue;

end;

define mi-operation exec-finish;

end;

define mi-operation exec-interrupt;

end;

define mi-operation exec-next;

end;

define mi-operation exec-next-instruction;

end;

define mi-operation exec-return;

end;

define mi-operation exec-run;

end;

define mi-operation exec-show-arguments;

end;

define mi-operation exec-step;

end;

define mi-operation exec-step-instruction;

end;

define mi-operation exec-until
    [ location :: <mi-location> ];

end;

define mi-operation file-exec-and-symbols(file :: <byte-string>)

end;

define mi-operation file-exec-file
    [file :: <byte-string>];

end;

define mi-operation file-list-exec-sections;

end;

define mi-operation file-list-exec-source-file;

end;

define mi-operation file-list-exec-source-files;

end;

define mi-operation file-list-shared-libraries;

end;

define mi-operation file-list-symbol-files;

end;

define mi-operation file-symbol-file
    [file :: <byte-string>];

end;

// ####################################
// Miscellaneous GDB commands in GDB/MI

define mi-operation gdb-exit;

end;

define mi-operation gdb-set;

end;

define mi-operation gdb-show(expr)

end;

define mi-operation gdb-version;

end;

define mi-operation interpreter-exec(interpreter, command)

end;

/*
define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;

define mi-operation 

end;
*/


// ####################################
// GDB/MI Stack Manipulation Commands

define mi-operation stack-info-frame;

end;

define mi-operation stack-info-depth
//    [ max-depth :: false-or(<integer>) ];
    [ max-depth :: <integer> ];

end;

define mi-operation stack-list-arguments(show-values)
  [ low-frame /*; high-frame */ ];

end;

define mi-operation stack-list-frames
  [ low-frame /*; high-frame */ ];

end;

/*
define mi-operation stack-list-locals(print-values -- { all-values, simple-values} )

end;
*/


// ####################################
// GDB/MI Symbol Query Commands

define mi-operation symbol-info-address(symbol)

end;

define mi-operation symbol-info-file;

end;

define mi-operation symbol-info-function;

end;

define mi-operation symbol-info-line;

end;

define mi-operation symbol-info-symbol(addr)

end;

define mi-operation symbol-list-functions;

end;

define mi-operation symbol-list-lines(filename)

end;

define mi-operation symbol-list-types;

end;

define mi-operation symbol-list-variables;

end;

define mi-operation symbol-locate;

end;

define mi-operation symbol-type(variable)

end;


// ####################################
// GDB/MI Target Manipulation Commands


define mi-operation target-attach
    {pid; file};

end;

define mi-operation target-compare-sections
    [ section ];

end;

define mi-operation target-detach;

end;

define mi-operation target-disconnect;

end;

define mi-operation target-download;

end;

define mi-operation target-exec-status;

end;

define mi-operation target-list-available-targets;

end;

define mi-operation target-list-current-targets;

end;

define mi-operation target-list-parameters;

end;

/*
define mi-operation target-select(type parameters ...)

end;
*/

// ####################################
// GDB/MI Thread Commands

define mi-operation thread-info;

end;

define mi-operation thread-list-all-threads;

end;

define mi-operation thread-list-ids;

end;

define mi-operation thread-select(threadnum)

end;


// ####################################
// GDB/MI Tracepoint Commands


// ####################################
// GDB/MI Variable Objects





define method print-object(s :: <stream>, cli :: <cli-command>)

end;

// ####################################
// Support functions

define function match(in :: <byte-string>, what :: <byte-string>) => remaining :: false-or(<byte-string>);
  let s = what.size;
  if (in.size >= s
      & copy-sequence(in, end: s) = what)
    copy-sequence(in, start: s)
  end if;
end;

define class <error-result>(<mi-result>)
  slot result-message :: <byte-string>, required-init-keyword: message:;
end;

define function report(response-class :: <class>, remaining :: <byte-string>) => resp :: <error-result>;
  make(<error-result>, message: remaining)
end;


define function match-c-string(in :: <byte-string>)
 => (remaining :: false-or(<byte-string>), result :: false-or(<byte-string>));

  local method consume-escape(in :: <byte-string>, sofar :: <list>, pos :: <integer>)
         => (result :: <byte-string>, final-pos :: <integer>);
          select (in[pos])
            '"', '\\' => consume(in, pair(in[pos], sofar), pos + 1);
            otherwise => error("unrecognized escape character: %=", in[pos]);
          end select
        end method,
        method consume(in :: <byte-string>, sofar :: <list>, pos :: <integer>)
         => (result :: <byte-string>, final-pos :: <integer>);
          select (in[pos])
            '\\' => consume-escape(in, sofar, pos + 1);
            '"' => values(as(<byte-string>, sofar.reverse!), pos + 1);
            otherwise => consume(in, pair(in[pos], sofar), pos + 1);
          end select
        end method;

  if (~in.empty? & in.first = '"')
    let (result :: <byte-string>, final-pos :: <integer>)
      = consume(in, #(), 1);
    values(copy-sequence(in, start: final-pos), result);
  end;
end;


//###### can be removed
begin
 let (a,b) = match-c-string("\"sd\\\\s\\\"dg\"123");
 format-out("remainer: %=\nc-string: %=\n", a, b);
end;


// ####################################
// Parser combinators

define function juxtapose(p1 :: <function>, p2 :: <function>)
 => juxtaposition :: <function>;
  method(inp :: <byte-string>, matched :: <list>, more) // => no-result :: <never-returns>;
    let (ign1, matched-by-p1)
      = block (done-first)
          p1(inp, #(), list(done-first));
        end block;
    if (matched-by-p1.empty?)
      more.head(inp, matched, more.tail)
    else
      let (rest, product1) = matched-by-p1.head();
      let (ign2, matched-by-p2)
        = block (done-second)
            p1(rest, #(), list(done-second));
          end block;
      if (matched-by-p2.empty?)
        more.head(inp, matched, more.tail)
      else
        let match = matched-by-p2.head;
        more.head(inp,
                  pair(method()
                         let (rest, product2) = match();
                         values(rest, list(product1, product2))
                       end,
                       matched),
                  more.tail)
      end if;
    end if;
  end method
end function;

//###### can be removed
// ###
begin

  let inp = "bkpt={number=\"1\",addr=\"0x0001072c\",file=\"recursive2.c\",line=\"4\"}bkpt={number=\"2\",addr=\"0x0001072c\",file=\"recursive2.c\",line=\"4\"}zzzzz";
  
  block (outta-here)
    local method final(rest, matches, more)
            let igno :: singleton(1) = matches.size;
            let (rem, both) = matches.head();
            format-out("parsed: %=\nremainder: %=\n\n", both, rem);
            format-out("breakpoints: 1st: %= 2nd: %=\n\n", both[0].number, both[1].number);
            outta-here();
          end;

    juxtapose(parse-breakpoint, parse-breakpoint)(inp, #(), list(final))
  end block;
end begin;

