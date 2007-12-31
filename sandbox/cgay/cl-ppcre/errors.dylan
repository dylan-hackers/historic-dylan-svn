//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/errors.lisp,v 1.16 2007/01/01 23:43:10 edi Exp $
//  Copyright (c) 2002-2007, Dr. Edmund Weitz. All rights reserved.
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions
//  are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above
//      copyright notice, this list of conditions and the following
//      disclaimer in the documentation and/or other materials
//      provided with the distribution.
//  THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
//  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
//  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
//  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
//  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
//  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
//  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"(in-package cl-ppcre)";

// The string which caused the syntax error.
define variable *syntax-error-string* = #f;

define class <ppcre-error> (<simple-error>); end class <ppcre-error>;

define class <ppcre-syntax-error> (<ppcre-error>)
  slot ppcre-syntax-error-string, init-keyword: #"string";
  slot ppcre-syntax-error-pos, init-keyword: #"pos";
end class <ppcre-syntax-error>;

// LTD: Function DOCUMENTATION not yet implemented.
documentation(#"ppcre-syntax-error-string", #"function")
 := "Returns the string the parser was parsing when the error was\nencountered (or NIL if the error happened while trying to convert a\nparse tree).";

// LTD: Function DOCUMENTATION not yet implemented.
documentation(#"ppcre-syntax-error-pos", #"function")
 := "Returns the position within the string where the error occured\n(or NIL if the error happened while trying to convert a parse tree";

define class <ppcre-invocation-error> (<ppcre-error>);
end class <ppcre-invocation-error>;

// LTD: No macros.
#"signal-ppcre-syntax-error*";

// LTD: No macros.
#"signal-ppcre-syntax-error";

// LTD: No macros.
#"signal-ppcre-invocation-error";

