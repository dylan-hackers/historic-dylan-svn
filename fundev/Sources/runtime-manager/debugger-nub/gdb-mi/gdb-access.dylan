module: gdb-access
synopsis: 
author: gabor@mac.com
copyright: see below


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

define method foo(libname)
  format-out("Hello, world from gdb-access!\n");
end method foo;



//--------------------------------------------------
// TESTCASES:
// these should end up in a testworks suite
//

define constant test-transcript
  = #[
       "34-eee"
       "34^error,msg=\"Undefined MI command: eee\""

#[
-break-after 1 7
&"No breakpoint number 1.\n"
^error,msg="No breakpoint number 1."
]

#[
-break-list
^done,BreakpointTable={}
]

#[
-break-info 1
^done,BreakpointTable={}
]

#[
]

#[
]

#[
]

#[
]

#[
]

#[
]

#[
]

#[
]

#[
]


     ];

//--------------------------------------------------
// MI-sessions
// you can have any number of sessions open with gdb
//

define class <gdb-mi-session>
  // 
  // pending: associating sent (but not yet answered) tokens with commands
  //
  slot session-pending :: <dequeue> = make(<dequeue>);
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
  slot command-token :: <integer>
  class slot current-token :: <integer> = 0;
end;

define class <cli-command>(<command>)
  constant slot cli-command :: <byte-string>, required-init-keyword: command:;
end;


define constant <operation> = <symbol>;

define class <mi-command>(<command>)
end;


define mi-operation break-after(number :: <positive>, count :: <positive>)
  resulting error => report;
  resulting done => parse-breakpoint-table;
end;

define mi-operation break-condition(number :: <positive>, expr :: <mi-expression>)
end;

define mi-operation break-delete((breakpoint :: <positive>)) // sequence !! map to #rest
end;

define mi-operation break-disable((breakpoint :: <positive>)) // sequence !! map to #rest
end;

define mi-operation break-enable((breakpoint :: <positive>)) // sequence !! map to #rest
end;

define mi-operation break-info breakpoint(breakpoint :: <positive>)
end;


define mi-operation break-insert(
    [ "-t" ]
    [ "-h" ]
    [ "-r" ]
    [ "-c" condition :: <mi-expression> ]
    [ "-i" ignore-count :: <positive> ]
    [ "-p" thread :: <positive> ]
    { line :: <positive>; addr :: <mi-address> })
  resulting done => parse-breakpoint;// bkpt={number="1",addr="0x0001072c",file="recursive2.c",line="4"}
end;

define mi-parser breakpoint(bkpt)
  number :: <positive>;
  addr :: <mi-address>;
  file :: <byte-string>;
  line :: <positive>
end;


define mi-operation break-list
  resulting done => parse-breakpoint-table;
end;

define mi-parser breakpoint-table(BreakpointTable)

end;

define mi-operation break-watch({"-a"; "-r"})
  resulting done => parse-reason;
end;

define mi-operation data-disassemble(
    [ -s start-addr -e end-addr ]
  | [ -f filename -l linenum [ -n lines ] ]
  -- mode
  )

  resulting done => parse-instructions; // asm_insns
end;


define mi-operation data-evaluate-expression(expr :: <mi-expr>)
  resulting done => parse-value; // ="0xefffeb7c"
end;


define mi-operation data-list-changed-registers;
  resulting done => parse- // changed-registers=["0","1","2","4","5","6","7","8","9",
"10","11","13","14","15","16","17","18","19","20","21","22","23",
"24","25","26","27","28","30","31","64","65","66","67","69"]
end;





define mi-operation data-list-register-names([ ( regno :: <positive> ) ])
done,register-names=["r1","r2","r3"]
end;

define mi-operation data-list-register-values(fmt :: <character>, [ ( regno :: <positive> ) ]
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

define mi-operation 

end;

define mi-operation exec-show-arguments;

end;

define mi-operation exec-step

end;

define mi-operation exec-step-instruction

end;

define mi-operation exec-until([ location :: <mi-location> ])

end;

define mi-operation file-exec-and-symbols(file :: <byte-string>)

end;

define mi-operation file-exec-file([file :: <byte-string>])

end;

define mi-operation file-list-exec-sections

end;

define mi-operation file-list-exec-source-file

end;

define mi-operation file-list-exec-source-files

end;

define mi-operation file-list-shared-libraries

end;

define mi-operation file-list-symbol-files

end;

define mi-operation file-symbol-file([file :: <byte-string>]}

end;

// ####################################
// Miscellaneous GDB commands in GDB/MI

define mi-operation gdb-exit

end;

define mi-operation gdb-set

end;

define mi-operation gdb-show(expr)

end;

define mi-operation gdb-version

end;

define mi-operation interpreter-exec interpreter command

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

define mi-operation 

end;



// ####################################
// GDB/MI Stack Manipulation Commands


define method print-object(s :: <stream>, cli :: <cli-command>)

