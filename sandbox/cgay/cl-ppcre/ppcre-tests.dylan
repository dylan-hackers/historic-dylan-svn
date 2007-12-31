//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE-TEST; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/ppcre-tests.lisp,v 1.34 2007/01/01 23:43:10 edi Exp $
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
"(in-package cl-ppcre-test)";

define variable *cl-ppcre-test-base-directory* =
  // LTD: Function MAKE-PATHNAME not yet implemented.
  make-pathname(name: #f, type: #f, version: #f,
                defaults: // LTD: Function PARSE-NAMESTRING not yet implemented.
                          parse-namestring(*load-truename*));

define method full-gc () "Start a full garbage collection."; end method full-gc;

//  warning: ugly code ahead!!
//  this is just a quick hack for testing purposes
define method time-regex (factor, regex, string :: <string>,
                          #key case-insensitive-mode, multi-line-mode,
                          single-line-mode, extended-mode)
  // Auxiliary function used by TEST to benchmark a regex scanner
  // against Perl timings.
  let scanner
      = create-scanner(regex, case-insensitive-mode: case-insensitive-mode,
                       multi-line-mode: multi-line-mode,
                       single-line-mode: single-line-mode,
                       extended-mode: extended-mode);
  let dummy = full-gc();
  let start = get-internal-real-time();
  for (i from 0 below factor) scanner(string, 0, size(string)); end for;
  as((get-internal-real-time() - start) / $internal-time-units-per-second,
     <float>);
end method time-regex;

define method create-string-from-input (input)
  if (empty?(input) | instance?(input, <string>))
    input;
  else
    string-list-to-simple-string(begin
                                   let _acc = make(<deque>);
                                   for (element in input)
                                     if (instance?(element, <string>))
                                       push-last(_acc, element);
                                     else
                                       push-last(_acc,
                                                 as(<string>,
                                                    as(<character>,
                                                       element)));
                                     end if;
                                   finally
                                     _acc;
                                   end for;
                                 end);
  end if;
end method create-string-from-input;

define method test (#key file-name
                          = // LTD: Function MAKE-PATHNAME not yet implemented.
                            make-pathname(name: "testdata", type: #f,
                                          version: #f,
                                          defaults: *cl-ppcre-test-base-directory*),
                    threaded)
  // Loop through all test cases in FILE-NAME and print report. Only in
  // LispWorks and SCL: If THREADED is true, also test whether the scanners
  // work multi-threaded.
  with-open-file (stream
                   = (file-name,
                      external-format: if (file-name-provided-p)
                                         #"default";
                                       else
                                         #"iso-8859-1";
                                       end if))
    let testcount :: <integer> = 0;
    fluid-bind (*regex-char-code-limit*
                 = if (file-name-provided-p)
                     *regex-char-code-limit*;
                   else
                     256;
                   end if)
      fluid-bind (*allow-quoting*
                   = if (file-name-provided-p)
                       *allow-quoting*;
                     else
                       #t;
                     end if)
        let input-line = #f;
        let counter = #f;
        let info-string = #f;
        let regex = #f;
        let case-insensitive-mode = #f;
        let multi-line-mode = #f;
        let single-line-mode = #f;
        let extended-mode = #f;
        let string = #f;
        let perl-error = #f;
        let factor = #f;
        let perl-time = #f;
        let ex-result = #f;
        let ex-subs = #f;
        let loop-ignore-2163 = #f;
        local method go-end-loop () #f; end method go-end-loop,
              method go-next-loop ()
                input-line
                 := // LTD: Function READ not yet implemented.
                    read(stream, #f, #f);
                let loop-desetq-temp = input-line;
                counter := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                info-string := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                regex := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                case-insensitive-mode := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                multi-line-mode := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                single-line-mode := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                extended-mode := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                string := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                perl-error := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                factor := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                perl-time := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                ex-result := head(loop-desetq-temp);
                loop-desetq-temp := tail(loop-desetq-temp);
                ex-subs := head(loop-desetq-temp);
                if (~ input-line) go-end-loop(); end if;
                let info-string = create-string-from-input(info-string);
                let regex = create-string-from-input(regex);
                let string = create-string-from-input(string);
                let ex-result = create-string-from-input(ex-result);
                let ex-subs = map(create-string-from-input, ex-subs);
                let errors = #();
                inc!(testcount);
                if (threaded)
                  format-out("Test #%S (ID %S)\n", testcount, counter);
                  force-output(*standard-output*);
                end if;
                if (~ threaded)
                  if (zero?(modulo(testcount, 10)))
                    format-out(".");
                    force-output(*standard-output*);
                  end if;
                  if (zero?(modulo(testcount, 100)))
                    write-element(*standard-output*, '\n');
                  end if;
                end if;
                // LTD: Function HANDLER-CASE not yet implemented.
                handler-case(begin
                               fluid-bind (*use-bmh-matchers*
                                            = if ((factor > 1
                                                    & positive?(perl-time)))
                                                *use-bmh-matchers*;
                                              else
                                                #f;
                                              end if)
                                 let scanner
                                     = create-scanner(regex,
                                                      case-insensitive-mode: case-insensitive-mode,
                                                      multi-line-mode: multi-line-mode,
                                                      single-line-mode: single-line-mode,
                                                      extended-mode: extended-mode);
                                 let (result1, result2, sub-starts, sub-ends)
                                     = scan(scanner, string);
                                 if (perl-error)
                                   push!(format(#f,
                                                "\nexpected an error but got a result"),
                                         errors);
                                 else
                                   if (~ (result1 == ex-result))
                                     if (result1)
                                       let result
                                           = copy-sequence(string, result1,
                                                           result2);
                                       if (~ (result = ex-result))
                                         push!(format(#f,
                                                      "\nexpected %= but got %=",
                                                      ex-result, result),
                                               errors);
                                       end if;
                                       begin
                                         sub-starts := as(<list>, sub-starts);
                                         sub-ends := as(<list>, sub-ends);
                                       end;
                                       for (i from 0, ex-sub in ex-subs,
                                            sub-start = sub-starts[i] then sub-starts[i],
                                            sub-end = sub-ends[i] then sub-ends[i],
                                            sub = if (sub-start & sub-end)
                                                    copy-sequence(string,
                                                                  sub-start,
                                                                  sub-end);
                                                  else
                                                    #f;
                                                  end if then if (sub-start
                                                                   & sub-end)
                                                                copy-sequence(string,
                                                                              sub-start,
                                                                              sub-end);
                                                              else
                                                                #f;
                                                              end if)
                                         if (~ (ex-sub = sub))
                                           push!(format(#f,
                                                        "\n\\%S: expected %= but got %=",
                                                        i + 1, ex-sub, sub),
                                                 errors);
                                         end if;
                                       end for;
                                     else
                                       push!(format(#f,
                                                    "\nexpected %= but got %=",
                                                    ex-result, result1),
                                             errors);
                                     end if;
                                   end if;
                                 end if;
                                 if (threaded)
                                   let thread-result
                                       = threaded-scan(scanner, string);
                                   if (thread-result)
                                     push!(thread-result, errors);
                                   end if;
                                 end if;
                               end fluid-bind;
                             end,
                             condition(msg(),
                                       if (~ perl-error)
                                         push!(format(#f,
                                                      "\ngot an unexpected error: '%S'",
                                                      msg),
                                               errors);
                                       end if));
                errors := reverse!(errors);
                if (errors)
                  if (factor <= 1 | zero?(perl-time))
                    (method (stream,
                             #key format-arg-2164
                                   = error(// LTD: Can't convert type specification.
                                           #"format-error",
                                           complaint: "required argument missing",
                                           control-string: "~&~4@A (~A):~{~&   ~A~}~%",
                                           offset: 5),
                             format-arg-2165
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A (~A):~{~&   ~A~}~%",
                                      offset: 9),
                             format-arg-2166
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A (~A):~{~&   ~A~}~%",
                                      offset: 13),
                             #rest args)
                       begin
                         write-element(stream, '\n');
                         let g441 = 4;
                         let g442 = 1;
                         let g443 = 0;
                         let g444 = ' ';
                         format-princ(stream, format-arg-2164, #(), #"t",
                                      g441, g442, g443, g444);
                         write(stream, " (");
                         print(format-arg-2165, stream);
                         write(stream, "):");
                         let orig-args = format-arg-2166;
                         let args = orig-args;
                         block (return)
                           while (#t)
                             if (empty?(args)) return(#f); end if;
                             write-element(stream, '\n');
                             write(stream, "   ");
                             print(if (args)
                                     pop!(args);
                                   else
                                     error(// LTD: Can't convert type specification.
                                           #"format-error",
                                           complaint: "no more arguments",
                                           control-string: "~&~4@A (~A):~{~&   ~A~}~%",
                                           offset: 20);
                                   end if,
                                   stream);
                           end while;
                         end block;
                         write-element(stream, '\n');
                       end;
                       args;
                     end method)(#t, counter, info-string, errors);
                  end if;
                elseif (factor > 1 & positive?(perl-time))
                  begin
                    let result
                        = time-regex(factor, regex, string,
                                     case-insensitive-mode: case-insensitive-mode,
                                     multi-line-mode: multi-line-mode,
                                     single-line-mode: single-line-mode,
                                     extended-mode: extended-mode);
                    (method (stream,
                             #key format-arg-2167
                                   = error(// LTD: Can't convert type specification.
                                           #"format-error",
                                           complaint: "required argument missing",
                                           control-string: "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)",
                                           offset: 5),
                             format-arg-2168
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)",
                                      offset: 11),
                             format-arg-2169
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)",
                                      offset: 15),
                             format-arg-2170
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)",
                                      offset: 39),
                             format-arg-2171
                              = error(// LTD: Can't convert type specification.
                                      #"format-error",
                                      complaint: "required argument missing",
                                      control-string: "~&~4@A: ~,4F (~A repetitions, Perl: ~,4F seconds, CL-PPCRE: ~,4F seconds)",
                                      offset: 63),
                             #rest args)
                       begin
                         write-element(stream, '\n');
                         let g441 = 4;
                         let g442 = 1;
                         let g443 = 0;
                         let g444 = ' ';
                         format-princ(stream, format-arg-2167, #(), #"t",
                                      g441, g442, g443, g444);
                         write(stream, ": ");
                         let g604 = #f;
                         let g605 = 4;
                         let g606 = #f;
                         let g607 = #f;
                         let g608 = ' ';
                         format-fixed(stream, format-arg-2168, g604, g605,
                                      g606, g607, g608, #f);
                         write(stream, " (");
                         print(format-arg-2169, stream);
                         write(stream, " repetitions, Perl: ");
                         let g604 = #f;
                         let g605 = 4;
                         let g606 = #f;
                         let g607 = #f;
                         let g608 = ' ';
                         format-fixed(stream, format-arg-2170, g604, g605,
                                      g606, g607, g608, #f);
                         write(stream, " seconds, CL-PPCRE: ");
                         let g604 = #f;
                         let g605 = 4;
                         let g606 = #f;
                         let g607 = #f;
                         let g608 = ' ';
                         format-fixed(stream, format-arg-2171, g604, g605,
                                      g606, g607, g608, #f);
                         write(stream, " seconds)");
                       end;
                       args;
                     end method)(#t, counter, as(result / perl-time, <float>),
                                 factor, perl-time, result);
                  end;
                else
                  #f;
                end if;
                go-next-loop();
                go-end-loop();
              end method go-next-loop;
        go-next-loop();
      end fluid-bind;
    end fluid-bind;
    values();
  end with-open-file;
end method test;

