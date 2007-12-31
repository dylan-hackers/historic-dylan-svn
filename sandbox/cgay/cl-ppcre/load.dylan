//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/load.lisp,v 1.15 2007/01/01 23:43:10 edi Exp $
//  Copyright (c) 2002-2007, Dr. Edmund Weitz.  All rights reserved.
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
"(in-package cl-user)";

begin
  let cl-ppcre-base-directory
      = // LTD: Function MAKE-PATHNAME not yet implemented.
        make-pathname(name: #f, type: #f, version: #f,
                      defaults: // LTD: Function PARSE-NAMESTRING not yet implemented.
                                parse-namestring(*load-truename*));
  let must-compile = #f;
  %with-compilation-unit(method ()
                           for (file in #("packages", "specials", "util",
                                          "errors", "lexer", "parser",
                                          "regex-class", "convert",
                                          "optimize", "closures",
                                          "repetition-closures", "scanner",
                                          "api", "ppcre-tests"))
                             let pathname
                                 = // LTD: Function MAKE-PATHNAME not yet implemented.
                                   make-pathname(name: file, type: "lisp",
                                                 version: #f,
                                                 defaults: cl-ppcre-base-directory);
                             #"let"((#"compiled-pathname"(#"compile-file-pathname"(#"pathname")))(),
                                    #"unless"(#"and"(#"not"(#"must-compile"),
                                                     #"probe-file"(#"compiled-pathname"),
                                                     #"<"(#"file-write-date"(#"pathname"),
                                                          #"file-write-date"(#"compiled-pathname"))),
                                              #"setq"(must-compile: #"t"),
                                              #"compile-file"(#"pathname")),
                                    #"setq"(pathname: #"compiled-pathname"));
                             // LTD: Function LOAD not yet implemented.
                             load(pathname);
                           end for;
                         end method);
end;

