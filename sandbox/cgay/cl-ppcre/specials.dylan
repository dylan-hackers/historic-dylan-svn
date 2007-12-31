//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/specials.lisp,v 1.25 2007/03/24 23:52:45 edi Exp $
//  globally declared special variables
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

//  special variables used to effect declarations
// The standard optimize settings used by most declaration expressions.
define variable *standard-optimize-settings* =
  #(#"optimize", #"speed", #(#"safety", 0), #(#"space", 0), #(#"debug", 1),
    #(#"compilation-speed", 0));

// Special optimize settings used only by a few declaration expressions.
define variable *special-optimize-settings* =
  #(#"optimize", #"speed", #"space");

//  special variables used by the lexer/parser combo
// Whether the parser will start in extended mode.
define variable *extended-mode-p* = #f;

#f;

//  special variables used by the SCAN function and the matchers
// The string which is currently scanned by SCAN.
// Will always be coerced to a SIMPLE-STRING.
define variable *string* = "";

#f;

// Where to start scanning within *STRING*.
define variable *start-pos* = 0;

#f;

// The real start of *STRING*. This is for repeated scans and is only used internally.
define variable *real-start-pos* = #f;

#f;

// Where to stop scanning within *STRING*.
define variable *end-pos* = 0;

#f;

// An array which holds the start positions
// of the current register candidates.
define variable *reg-starts* = make(<vector>, size: 0);

#f;

// An array which holds the next start positions
// of the current register candidates.
define variable *regs-maybe-start* = make(<vector>, size: 0);

#f;

// An array which holds the end positions
// of the current register candidates.
define variable *reg-ends* = make(<vector>, size: 0);

#f;

// Start of the next possible end-string candidate.
define variable *end-string-pos* = #f;

// Counts the number of "complicated" repetitions while the matchers
// are built.
define variable *rep-num* = 0;

#f;

// Counts the number of repetitions the inner regexes of which may
// have zero-length while the matchers are built.
define variable *zero-length-num* = 0;

#f;

// An array to keep track of how often
// repetitive patterns have been tested already.
define variable *repeat-counters* = make(<vector>, size: 0);

#f;

// An array to keep track of the last positions
// where we saw repetitive patterns.
// Only used for patterns which might have zero length.
define variable *last-pos-stores* = make(<vector>, size: 0);

#f;

// Whether the scanners created by CREATE-SCANNER should use the (fast
// but large) Boyer-Moore-Horspool matchers.
define variable *use-bmh-matchers* = #t;

// Whether the parser should support Perl's \Q and \E.
define variable *allow-quoting* = #f;

// Whether the parser should support AllegroCL's named registers
// (?<name>"<regex>") and back-reference \k<name> syntax.
define variable *allow-named-registers* = #f;

begin
  let g2207 = #"cl-ppcre";
  let g2206 = add!(g2207, *features*);
  *features* := g2206;
end;

//  stuff for Nikodemus Siivola's HYPERDOC
//  see <http://common-lisp.net/project/hyperdoc/>
//  and <http://www.cliki.net/hyperdoc>
//  also used by LW-ADD-ONS
define variable *hyperdoc-base-uri* = "http://weitz.de/cl-ppcre/";

begin
  let exported-symbols-alist
      = block (return)
          let loop-it-2210 = #f;
          let symbol = #f;
          let loop-pkgsym-2208 = #"cl-ppcre";
          block (return)
            let g2214 = loop-pkgsym-2208;
            let g2213
                = map(method (package)
                        if (// LTD: Function PACKAGEP not yet implemented.
                            packagep(package))
                          package;
                        else
                          // LTD: Function FIND-PACKAGE not yet implemented.
                          find-package(package)
                           | error(// LTD: Can't convert type specification.
                                   #"simple-package-error",
                                   package: as(<string>, package),
                                   format-control: "~@<~S does not name a package ~:>",
                                   format-arguments: list(package));
                        end if;
                      end method,
                      if (instance?(g2214, <pair>))
                        g2214;
                      else
                        list(g2214);
                      end if);
            let g2215 = #f;
            let g2216 = head(g2213);
            let g2217 = #f;
            let g2218 = #f;
            let g2219 = #f;
            block (return)
              // LTD: Function MACROLET not yet implemented.
              macrolet((g2220(next-kind(), #f,
                              begin
                                let symbols = generate-symbol();
                                backq-list(#"progn",
                                           backq-list(#"setf", #"g2216",
                                                      next-kind),
                                           #(#"setf", #"g2215", #()),
                                           select (next-kind)
                                             #"internal"
                                                => backq-list(#"let",
                                                              backq-list(backq-cons(symbols,
                                                                                    #(#(#"package-internal-symbols",
                                                                                        #(#"car",
                                                                                          #"g2213"))))),
                                                              backq-list(#"when",
                                                                         symbols,
                                                                         backq-list(#"setf",
                                                                                    #"g2218",
                                                                                    backq-list(#"package-hashtable-table",
                                                                                               symbols)),
                                                                         backq-list(#"setf",
                                                                                    #"g2217",
                                                                                    backq-list(#"package-hashtable-hash",
                                                                                               symbols))));
                                             #"external"
                                                => backq-list(#"let",
                                                              backq-list(backq-cons(symbols,
                                                                                    #(#(#"package-external-symbols",
                                                                                        #(#"car",
                                                                                          #"g2213"))))),
                                                              backq-list(#"when",
                                                                         symbols,
                                                                         backq-list(#"setf",
                                                                                    #"g2218",
                                                                                    backq-list(#"package-hashtable-table",
                                                                                               symbols)),
                                                                         backq-list(#"setf",
                                                                                    #"g2217",
                                                                                    backq-list(#"package-hashtable-hash",
                                                                                               symbols))));
                                             #"inherited"
                                                => backq-list(#"let",
                                                              backq-list(backq-cons(symbols,
                                                                                    #(#(#"and",
                                                                                        #"g2219",
                                                                                        #(#"package-external-symbols",
                                                                                          #(#"car",
                                                                                            #"g2219")))))),
                                                              backq-list(#"when",
                                                                         symbols,
                                                                         backq-list(#"setf",
                                                                                    #"g2218",
                                                                                    backq-list(#"package-hashtable-table",
                                                                                               symbols)),
                                                                         backq-list(#"setf",
                                                                                    #"g2217",
                                                                                    backq-list(#"package-hashtable-hash",
                                                                                               symbols))));
                                             otherwise
                                                => #f;
                                           end select);
                              end))(g2221(this-kind(),
                                          begin
                                            let next-kind
                                                = second(member?(this-kind,
                                                                 #(#"external")));
                                            if (next-kind)
                                              backq-list(#"g2220", next-kind);
                                            else
                                              backq-list(#"if",
                                                         #(#"endp",
                                                           #(#"setf",
                                                             #"g2213",
                                                             #(#"cdr",
                                                               #"g2213"))),
                                                         #(#"return-from",
                                                           #"g2224"),
                                                         backq-list(#"g2220",
                                                                    head(#(#"external"))));
                                            end if;
                                          end)),
                       if (g2213)
                         block (return)
                           #f;
                           #f;
                           g2220(#"external");
                           let g2222 = method (number) number > 1; end method;
                           block (return)
                             // LTD: Function MACROLET not yet implemented.
                             macrolet((loop-pkgsym-next-2209(#f, #f,
                                                             backq-list(#"block",
                                                                        #"g2224",
                                                                        backq-list(#"loop",
                                                                                   backq-list*(#"case",
                                                                                               #"g2216",
                                                                                               backq-append(if (member?(internal: #(#"external")))
                                                                                                              #(#(#"internal",
                                                                                                                  #(#"setf",
                                                                                                                    #"g2215",
                                                                                                                    #(#"position-if",
                                                                                                                      #(#"function",
                                                                                                                        #"g2222"),
                                                                                                                      #(#"the", 
                                                                                                                        #"hash-vector",
                                                                                                                        #"g2217"),
                                                                                                                      #"start", 
                                                                                                                      #(#"if",
                                                                                                                        #"g2215",
                                                                                                                        #(#"1+",
                                                                                                                          #"g2215"),
                                                                                                                        0))),
                                                                                                                  #(#"if",
                                                                                                                    #"g2215",
                                                                                                                    #(#"return-from",
                                                                                                                      #"g2224", 
                                                                                                                      #(#"values",
                                                                                                                        #"t",
                                                                                                                        #(#"svref",
                                                                                                                          #"g2218",
                                                                                                                          #"g2215"),
                                                                                                                        #"g2216",
                                                                                                                        #(#"car",
                                                                                                                          #"g2213"))),
                                                                                                                    #(#"g2221", 
                                                                                                                      #"internal"))));
                                                                                                            end if,
                                                                                                            if (member?(external: #(#"external")))
                                                                                                              #(#(#"external",
                                                                                                                  #(#"setf",
                                                                                                                    #"g2215",
                                                                                                                    #(#"position-if",
                                                                                                                      #(#"function",
                                                                                                                        #"g2222"),
                                                                                                                      #(#"the", 
                                                                                                                        #"hash-vector",
                                                                                                                        #"g2217"),
                                                                                                                      #"start", 
                                                                                                                      #(#"if",
                                                                                                                        #"g2215",
                                                                                                                        #(#"1+",
                                                                                                                          #"g2215"),
                                                                                                                        0))),
                                                                                                                  #(#"if",
                                                                                                                    #"g2215",
                                                                                                                    #(#"return-from",
                                                                                                                      #"g2224", 
                                                                                                                      #(#"values",
                                                                                                                        #"t",
                                                                                                                        #(#"svref",
                                                                                                                          #"g2218",
                                                                                                                          #"g2215"),
                                                                                                                        #"g2216",
                                                                                                                        #(#"car",
                                                                                                                          #"g2213"))),
                                                                                                                    #(#"g2221", 
                                                                                                                      #"external"))));
                                                                                                            end if,
                                                                                                            if (member?(inherited: #(#"external")))
                                                                                                              backq-list(backq-list(#"inherited",
                                                                                                                                    #(#"flet",
                                                                                                                                      #(#(#"g2223",
                                                                                                                                          #(#"number"),
                                                                                                                                          #(#"when",
                                                                                                                                            #(#"g2222",
                                                                                                                                              #"number"),
                                                                                                                                            #(#"let*",
                                                                                                                                              #(#(#"p",
                                                                                                                                                  #(#"position",
                                                                                                                                                    #"number",
                                                                                                                                                    #(#"the",
                                                                                                                                                      #"hash-vector",
                                                                                                                                                      #"g2217"),
                                                                                                                                                    #"start",
                                                                                                                                                    #(#"if",
                                                                                                                                                      #"g2215",
                                                                                                                                                      #(#"1+",
                                                                                                                                                        #"g2215"),
                                                                                                                                                      0))),
                                                                                                                                                #(#"s",
                                                                                                                                                  #(#"svref",
                                                                                                                                                    #"g2218",
                                                                                                                                                    #"p"))),
                                                                                                                                              #(#"eql",
                                                                                                                                                #(#"nth-value",
                                                                                                                                                  1,
                                                                                                                                                  #(#"find-symbol",
                                                                                                                                                    #(#"symbol-name",
                                                                                                                                                      #"s"),
                                                                                                                                                    #(#"car",
                                                                                                                                                      #"g2213"))),
                                                                                                                                                #"inherited"))))),
                                                                                                                                      #(#"setf",
                                                                                                                                        #"g2215",
                                                                                                                                        #(#"when",
                                                                                                                                          #"g2217",
                                                                                                                                          #(#"position-if",
                                                                                                                                            #(#"function",
                                                                                                                                              #"g2223"),
                                                                                                                                            #(#"the",
                                                                                                                                              #"hash-vector",
                                                                                                                                              #"g2217"),
                                                                                                                                            #"start",
                                                                                                                                            #(#"if",
                                                                                                                                              #"g2215",
                                                                                                                                              #(#"1+",
                                                                                                                                                #"g2215"),
                                                                                                                                              0))))),
                                                                                                                                    backq-list(#"cond",
                                                                                                                                               #(#"g2215",
                                                                                                                                                 #(#"return-from",
                                                                                                                                                   #"g2224",
                                                                                                                                                   #(#"values",
                                                                                                                                                     #"t",
                                                                                                                                                     #(#"svref",
                                                                                                                                                       #"g2218",
                                                                                                                                                       #"g2215"),
                                                                                                                                                     #"g2216",
                                                                                                                                                     #(#"car",
                                                                                                                                                       #"g2213")))),
                                                                                                                                               backq-list(#"t",
                                                                                                                                                          #(#"setf",
                                                                                                                                                            #"g2219",
                                                                                                                                                            #(#"cdr",
                                                                                                                                                              #"g2219")),
                                                                                                                                                          backq-list*(#"cond",
                                                                                                                                                                      backq-list(#(#"endp",
                                                                                                                                                                                   #"g2219"),
                                                                                                                                                                                 #(#"setf",
                                                                                                                                                                                   #"g2213",
                                                                                                                                                                                   #(#"cdr",
                                                                                                                                                                                     #"g2213")),
                                                                                                                                                                                 #(#"when",
                                                                                                                                                                                   #(#"endp",
                                                                                                                                                                                     #"g2213"),
                                                                                                                                                                                   #(#"return-from",
                                                                                                                                                                                     #"g2224")),
                                                                                                                                                                                 #(#"setf",
                                                                                                                                                                                   #"g2219",
                                                                                                                                                                                   #(#"package-%use-list",
                                                                                                                                                                                     #(#"car",
                                                                                                                                                                                       #"g2213"))),
                                                                                                                                                                                 backq-list(#"g2220",
                                                                                                                                                                                            head(#(#"external")))),
                                                                                                                                                                      #(#(#"t",
                                                                                                                                                                          #(#"g2220",
                                                                                                                                                                            #"inherited"),
                                                                                                                                                                          #(#"setf",
                                                                                                                                                                            #"g2215",
                                                                                                                                                                            #()))))))));
                                                                                                            end if))))))(),
                                      begin
                                        let loop-list-head-2211 = list(#f);
                                        let loop-list-tail-2212
                                            = loop-list-head-2211;
                                        block (return)
                                          local method go-end-loop ()
                                                  return-from-nil(tail(loop-list-head-2211));
                                                end method go-end-loop,
                                                method go-next-loop ()
                                                  if (~ begin
                                                          let (#rest _loop-it-2210,
                                                               _symbol)
                                                              = loop-pkgsym-next-2209();
                                                          (loop-it-2210
                                                            := _loop-it-2210);
                                                          (symbol := _symbol);
                                                        end)
                                                    go-end-loop();
                                                  end if;
                                                  tail(loop-list-tail-2212)
                                                   := (loop-list-tail-2212
                                                        := list(pair(symbol,
                                                                     concatenate-as(<string>,
                                                                                    "#",
                                                                                    as-lowercase!(symbol)))));
                                                  go-next-loop();
                                                  go-end-loop();
                                                end method go-next-loop;
                                          go-next-loop();
                                        end block;
                                      end);
                           end block;
                         end block;
                       end if);
            end block;
          end block;
        end block;
  define method hyperdoc-lookup (symbol, type)
    tail(cl-assoc(symbol, exported-symbols-alist, test: \==));
  end method hyperdoc-lookup;
end;

