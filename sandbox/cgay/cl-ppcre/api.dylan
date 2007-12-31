//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/api.lisp,v 1.75 2007/09/13 07:52:13 edi Exp $
//  The external API for creating and using scanners.
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

// Accepts a regular expression - either as a
// parse-tree or as a string - and returns a scan closure which will scan
// strings for this regular expression and a list mapping registers to
// their names (NIL stands for unnamed ones). The "mode" keyboard
// arguments are equivalent to the imsx modifiers in Perl. If DESTRUCTIVE
// is not NIL the function is allowed to destructively modify its first
// argument (but only if it's a parse tree).
define generic create-scanner (regex, #key case-insensitive-mode,
                               multi-line-mode, single-line-mode,
                               extended-mode, destructive)
;

define method create-scanner (regex-string :: <string>,
                              #key case-insensitive-mode, multi-line-mode,
                              single-line-mode, extended-mode, destructive)
  fluid-bind (*extended-mode-p* = extended-mode)
    let quoted-regex-string
        = if (*allow-quoting*)
            quote-sections(clean-comments(regex-string, extended-mode));
          else
            regex-string;
          end if;
    fluid-bind (*syntax-error-string* = copy-sequence(quoted-regex-string))
      //  wrap the result with :GROUP to avoid infinite loops for
      //  constant strings
      create-scanner(pair(group: list(parse-string(quoted-regex-string))),
                     case-insensitive-mode: case-insensitive-mode,
                     multi-line-mode: multi-line-mode,
                     single-line-mode: single-line-mode, destructive: #t);
    end fluid-bind;
  end fluid-bind;
end method create-scanner;

define method create-scanner (scanner :: <function>,
                              #key case-insensitive-mode, multi-line-mode,
                              single-line-mode, extended-mode, destructive)
  if (case-insensitive-mode | multi-line-mode | single-line-mode
       | extended-mode)
    error(// LTD: Can't convert type specification.
          #"ppcre-invocation-error",
          format-control: "You can't use the keyword arguments to modify an existing scanner.",
          format-arguments: list());
  end if;
  scanner;
end method create-scanner;

define method create-scanner (parse-tree :: <object>,
                              #key case-insensitive-mode, multi-line-mode,
                              single-line-mode, extended-mode, destructive)
  if (extended-mode)
    error(// LTD: Can't convert type specification.
          #"ppcre-invocation-error",
          format-control: "Extended mode doesn't make sense in parse trees.",
          format-arguments: list());
  end if;
  //  convert parse-tree into internal representation REGEX and at the
  //  same time compute the number of registers and the constant string
  //  (or anchor) the regex starts with (if any)
  if (~ destructive)
    parse-tree
     := // LTD: Function COPY-TREE not yet implemented.
        copy-tree(parse-tree);
  end if;
  let flags = #f;
  if (single-line-mode) push!(single-line-mode-p: flags); end if;
  if (multi-line-mode) push!(multi-line-mode-p: flags); end if;
  if (case-insensitive-mode) push!(case-insensitive-p: flags); end if;
  if (flags)
    parse-tree := list(group: pair(flags: flags), parse-tree);
  end if;
  fluid-bind (*syntax-error-string* = #f)
    let (regex, reg-num, starts-with, reg-names) = convert(parse-tree);
    let regex = gather-strings(flatten(regex));
    //  set the MIN-REST slots of the REPETITION objects
    compute-min-rest(regex, 0);
    //  set the OFFSET slots of the STR objects
    compute-offsets(regex, 0);
    let end-string-offset = #f;
    let end-anchored-p = #f;
    let end-string = end-string(regex);
    let end-string-test
        = end-string & positive?(end-string.len)
           & if (1 = end-string.len)
               create-char-searcher(str(end-string)[0],
                                                    end-string
                                                    .case-insensitive-p);
             else
               create-bmh-matcher(str(end-string),
                                  end-string.case-insensitive-p);
             end if;
    fluid-bind (*rep-num* = 0)
      fluid-bind (*zero-length-num* = 0)
        let match-fn = create-matcher-aux(regex, identity);
        let start-string-test
            = instance?(starts-with, <str>) & positive?(starts-with.len)
               & if (1 = starts-with.len)
                   create-char-searcher(str(starts-with)[0],
                                                         starts-with
                                                         .case-insensitive-p);
                 else
                   create-bmh-matcher(str(starts-with),
                                      starts-with.case-insensitive-p);
                 end if;
        //  now create the scanner and return it
        values(create-scanner-aux(match-fn, regex-min-length(regex),
                                  start-anchored-p(regex)
                                   | //  a dot in single-line-mode also
                                     //  implicitly anchors the regex at
                                     //  the start, i.e. if we can't match
                                     //  from the first position we won't
                                     //  match at all
                                  (instance?(starts-with, <everything>)
                                    & starts-with.single-line-p),
                                  starts-with, start-string-test,
                                  //  only mark regex as end-anchored if we
                                  //  found a non-zero-length string before
                                  //  the anchor
                                  end-string-test & end-anchored-p,
                                  end-string-test,
                                  if (end-string-test)
                                    end-string.len;
                                  else
                                    #f;
                                  end if,
                                  end-string-offset, *rep-num*,
                                  *zero-length-num*, reg-num),
               reg-names);
      end fluid-bind;
    end fluid-bind;
  end fluid-bind;
end method create-scanner;

// Searches TARGET-STRING from START to END and tries
// to match REGEX.  On success returns four values - the start of the
// match, the end of the match, and two arrays denoting the beginnings
// and ends of register matches.  On failure returns NIL.  REGEX can be a
// string which will be parsed according to Perl syntax, a parse tree, or
// a pre-compiled scanner created by CREATE-SCANNER.  TARGET-STRING will
// be coerced to a simple string if it isn't one already.  The
// REAL-START-POS parameter should be ignored - it exists only for
// internal purposes.
define generic scan (regex, target-string, #key start, end, real-start-pos) ;

define method scan (regex-string :: <string>, target-string, #key start = 0,
                    end = size(target-string),
                    real-start-pos: *real-start-pos* = #f)
  //  note that the scanners are optimized for simple strings so we
  //  have to coerce TARGET-STRING into one if it isn't already
  (create-scanner(regex-string))(begin
                                   let =string=2014 = target-string;
                                   if (simple-string-p(=string=2014))
                                     =string=2014;
                                   else
                                     as(<simple-string>, =string=2014);
                                   end if;
                                 end,
                                 start, end);
end method scan;

define method scan (scanner :: <function>, target-string, #key start = 0,
                    end = size(target-string),
                    real-start-pos: *real-start-pos* = #f)
  scanner(begin
            let =string=2015 = target-string;
            if (simple-string-p(=string=2015))
              =string=2015;
            else
              as(<simple-string>, =string=2015);
            end if;
          end,
          start, end);
end method scan;

define method scan (parse-tree :: <object>, target-string, #key start = 0,
                    end = size(target-string),
                    real-start-pos: *real-start-pos* = #f)
  (create-scanner(parse-tree))(begin
                                 let =string=2016 = target-string;
                                 if (simple-string-p(=string=2016))
                                   =string=2016;
                                 else
                                   as(<simple-string>, =string=2016);
                                 end if;
                               end,
                               start, end);
end method scan;

%define-compiler-macro(#"scan",
                       method (whole2017, environment2018)
                         let cmacro-&whole2019 = whole2017;
                         let whole2017
                             = if (~ (#"funcall" == head(whole2017)))
                                 whole2017;
                               else
                                 whole2017 := tail(whole2017);
                               end if;
                         let args2020
                             = if (#"funcall" == head(whole2017))
                                 tail(tail(whole2017));
                               else
                                 tail(whole2017);
                               end if;
                         if (~ list-of-length-at-least-p(args2020, 2))
                           arg-count-error(#"define-compiler-macro", #"scan",
                                           args2020,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"target-string",
                                             #"&rest", #"rest"),
                                           2, #f);
                         end if;
                         let env = environment2018;
                         let form = cmacro-&whole2019;
                         let regex = head(tail(whole2017));
                         let target-string = head(tail(tail(whole2017)));
                         let rest = tail(tail(tail(whole2017)));
                         if (constant?(regex, env))
                           backq-list*(#"scan",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       target-string, rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"target-string", #"&rest", #"rest"),
                       "Make sure that constant forms are compiled into scanners at compile time.",
                       #(#"compiler-macro-function", #"scan"));

define method scan-to-strings (regex, target-string, #key start = 0,
                               end = size(target-string), sharedp)
  // Like SCAN but returns substrings of TARGET-STRING instead of
  // positions, i.e. this function returns two values on success: the whole
  // match as a string plus an array of substrings (or NILs) corresponding
  // to the matched registers. If SHAREDP is true, the substrings may share
  // structure with TARGET-STRING.
  block (return-from-scan-to-strings)
    let (match-start, match-end, reg-starts, reg-ends)
        = scan(regex, target-string, start: start, end: end);
    if (~ match-start) return-from-scan-to-strings(#f); end if;
    let substr-fn = if (sharedp) nsubseq; else copy-sequence; end if;
    values(substr-fn(target-string, match-start, match-end),
           map-as(<vector>,
                  method (reg-start, reg-end)
                    if (reg-start)
                      substr-fn(target-string, reg-start, reg-end);
                    else
                      #f;
                    end if;
                  end method,
                  reg-starts, reg-ends));
  end block;
end method scan-to-strings;

%define-compiler-macro(#"scan-to-strings",
                       method (whole2021, environment2022)
                         let cmacro-&whole2023 = whole2021;
                         let whole2021
                             = if (~ (#"funcall" == head(whole2021)))
                                 whole2021;
                               else
                                 whole2021 := tail(whole2021);
                               end if;
                         let args2024
                             = if (#"funcall" == head(whole2021))
                                 tail(tail(whole2021));
                               else
                                 tail(whole2021);
                               end if;
                         if (~ list-of-length-at-least-p(args2024, 2))
                           arg-count-error(#"define-compiler-macro",
                                           #"scan-to-strings", args2024,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"target-string",
                                             #"&rest", #"rest"),
                                           2, #f);
                         end if;
                         let env = environment2022;
                         let form = cmacro-&whole2023;
                         let regex = head(tail(whole2021));
                         let target-string = head(tail(tail(whole2021)));
                         let rest = tail(tail(tail(whole2021)));
                         if (constant?(regex, env))
                           backq-list*(#"scan-to-strings",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       target-string, rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"target-string", #"&rest", #"rest"),
                       "Make sure that constant forms are compiled into scanners at compile time.",
                       #(#"compiler-macro-function", #"scan-to-strings"));

// LTD: No macros.
#"register-groups-bind";

// LTD: No macros.
#"do-scans";

// LTD: No macros.
#"do-matches";

// LTD: No macros.
#"do-matches-as-strings";

// LTD: No macros.
#"do-register-groups";

define method all-matches (regex, target-string, #key start = 0,
                           end = size(target-string))
  // Returns a list containing the start and end positions of all
  // matches of REGEX against TARGET-STRING, i.e. if there are N matches
  // the list contains (* 2 N) elements. If REGEX matches an empty string
  // the scan is continued one position behind this match.
  let result-list = #f;
  let target-string2027 = target-string;
  let %start2028 = start | 0;
  let %end2029 = end | size(target-string2027);
  let %regex2030 = regex;
  let scanner2031
      = select (%regex2030 by instance?)
          function
             => %regex2030;
          #t
             => create-scanner(%regex2030);
        end select;
  target-string2027
   := begin
        let =string=2034 = target-string2027;
        if (simple-string-p(=string=2034))
          =string=2034;
        else
          as(<simple-string>, =string=2034);
        end if;
      end;
  block (return-from-block-name2033)
    local method go-loop-tag2032 ()
            let (match-start, match-end, reg-starts2025, reg-ends2026)
                = scan(scanner2031, target-string2027, start: %start2028,
                       end: %end2029, real-start-pos: start | 0);
            if (~ match-start)
              return-from-block-name2033(reverse!(result-list));
            end if;
            begin
              push!(match-start, result-list);
              push!(match-end, result-list);
            end;
            %start2028
             := if (match-start = match-end)
                  match-end + 1;
                else
                  match-end;
                end if;
            go-loop-tag2032();
          end method go-loop-tag2032;
    go-loop-tag2032();
  end block;
end method all-matches;

%define-compiler-macro(#"all-matches",
                       method (whole2035, environment2036)
                         let cmacro-&whole2037 = whole2035;
                         let whole2035
                             = if (~ (#"funcall" == head(whole2035)))
                                 whole2035;
                               else
                                 whole2035 := tail(whole2035);
                               end if;
                         let args2038
                             = if (#"funcall" == head(whole2035))
                                 tail(tail(whole2035));
                               else
                                 tail(whole2035);
                               end if;
                         if (~ list-of-length-at-least-p(args2038, 1))
                           arg-count-error(#"define-compiler-macro",
                                           #"all-matches", args2038,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"&rest", #"rest"),
                                           1, #f);
                         end if;
                         let env = environment2036;
                         let form = cmacro-&whole2037;
                         let regex = head(tail(whole2035));
                         let rest = tail(tail(whole2035));
                         if (constant?(regex, env))
                           backq-list*(#"all-matches",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"&rest", #"rest"),
                       "Make sure that constant forms are compiled into scanners at\ncompile time.",
                       #(#"compiler-macro-function", #"all-matches"));

define method all-matches-as-strings (regex, target-string, #key start = 0,
                                      end = size(target-string), sharedp)
  // Returns a list containing all substrings of TARGET-STRING which
  // match REGEX. If REGEX matches an empty string the scan is continued
  // one position behind this match. If SHAREDP is true, the substrings may
  // share structure with TARGET-STRING.
  let result-list = #f;
  let target-string2039 = target-string;
  let substr-fn2042 = if (sharedp) nsubseq; else copy-sequence; end if;
  let target-string2045 = target-string2039;
  let %start2046 = start | 0;
  let %end2047 = end | size(target-string2045);
  let %regex2048 = regex;
  let scanner2049
      = select (%regex2048 by instance?)
          function
             => %regex2048;
          #t
             => create-scanner(%regex2048);
        end select;
  target-string2045
   := begin
        let =string=2052 = target-string2045;
        if (simple-string-p(=string=2052))
          =string=2052;
        else
          as(<simple-string>, =string=2052);
        end if;
      end;
  block (return-from-block-name2051)
    local method go-loop-tag2050 ()
            let (match-start2040, match-end2041, reg-starts2043, reg-ends2044)
                = scan(scanner2049, target-string2045, start: %start2046,
                       end: %end2047, real-start-pos: start | 0);
            if (~ match-start2040)
              return-from-block-name2051(reverse!(result-list));
            end if;
            let match
                = substr-fn2042(target-string2039, match-start2040,
                                match-end2041);
            push!(match, result-list);
            %start2046
             := if (match-start2040 = match-end2041)
                  match-end2041 + 1;
                else
                  match-end2041;
                end if;
            go-loop-tag2050();
          end method go-loop-tag2050;
    go-loop-tag2050();
  end block;
end method all-matches-as-strings;

%define-compiler-macro(#"all-matches-as-strings",
                       method (whole2053, environment2054)
                         let cmacro-&whole2055 = whole2053;
                         let whole2053
                             = if (~ (#"funcall" == head(whole2053)))
                                 whole2053;
                               else
                                 whole2053 := tail(whole2053);
                               end if;
                         let args2056
                             = if (#"funcall" == head(whole2053))
                                 tail(tail(whole2053));
                               else
                                 tail(whole2053);
                               end if;
                         if (~ list-of-length-at-least-p(args2056, 1))
                           arg-count-error(#"define-compiler-macro",
                                           #"all-matches-as-strings",
                                           args2056,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"&rest", #"rest"),
                                           1, #f);
                         end if;
                         let env = environment2054;
                         let form = cmacro-&whole2055;
                         let regex = head(tail(whole2053));
                         let rest = tail(tail(whole2053));
                         if (constant?(regex, env))
                           backq-list*(#"all-matches-as-strings",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"&rest", #"rest"),
                       "Make sure that constant forms are compiled into scanners at\ncompile time.",
                       #(#"compiler-macro-function",
                         #"all-matches-as-strings"));

define method split (regex, target-string, #key start = 0,
                     end = size(target-string), limit, with-registers-p,
                     omit-unmatched-p, sharedp)
  // Matches REGEX against TARGET-STRING as often as possible and
  // returns a list of the substrings between the matches. If
  // WITH-REGISTERS-P is true, substrings corresponding to matched
  // registers are inserted into the list as well. If OMIT-UNMATCHED-P is
  // true, unmatched registers will simply be left out, otherwise they will
  // show up as NIL. LIMIT limits the number of elements returned -
  // registers aren't counted. If LIMIT is NIL (or 0 which is equivalent),
  // trailing empty strings are removed from the result list.  If REGEX
  // matches an empty string the scan is continued one position behind this
  // match. If SHAREDP is true, the substrings may share structure with
  // TARGET-STRING.
  let pos-list = list(start);
  let counter = 0;
  block (return)
    //  how would Larry Wall do it?
    if (limit == 0) limit := #f; end if;
    let target-string2057 = target-string;
    block (return)
      begin
        let %start2058 = start | 0;
        let %end2059 = end | size(target-string2057);
        let %regex2060 = regex;
        let scanner2061
            = select (%regex2060 by instance?)
                function
                   => %regex2060;
                #t
                   => create-scanner(%regex2060);
              end select;
        target-string2057
         := begin
              let =string=2064 = target-string2057;
              if (simple-string-p(=string=2064))
                =string=2064;
              else
                as(<simple-string>, =string=2064);
              end if;
            end;
        block (return-from-block-name2063)
          local method go-loop-tag2062 ()
                  let (match-start, match-end, reg-starts, reg-ends)
                      = scan(scanner2061, target-string2057,
                             start: %start2058, end: %end2059,
                             real-start-pos: start | 0);
                  if (~ match-start) return-from-block-name2063(#f); end if;
                  if (~ (match-start = match-end
                          & match-start = head(pos-list)))
                    if (limit & inc!(counter) >= limit) return(#f); end if;
                    push!(match-start, pos-list);
                    if (with-registers-p)
                      for (reg-start in reg-starts, reg-end in reg-ends)
                        if (reg-start)
                          push!(reg-start, pos-list);
                          push!(reg-end, pos-list);
                        else
                          if (~ omit-unmatched-p)
                            push!(#f, pos-list);
                            push!(#f, pos-list);
                          end if;
                        end if;
                      end for;
                    end if;
                    push!(match-end, pos-list);
                  end if;
                  %start2058
                   := if (match-start = match-end)
                        match-end + 1;
                      else
                        match-end;
                      end if;
                  go-loop-tag2062();
                end method go-loop-tag2062;
          go-loop-tag2062();
        end block;
      end;
    end block;
    //  end of whole string
    push!(end, pos-list);
    //  now collect substrings
    reverse!(block (return)
               let substr-fn
                   = if (sharedp) nsubseq; else copy-sequence; end if;
               block (return)
                 let string-seen = #f;
                 block (return)
                   let g2065 = pos-list;
                   let this-end = #f;
                   let this-start = #f;
                   let loop-ignore-2066 = #f;
                   block (return)
                     let loop-list-head-2067 = list(#f);
                     let loop-list-tail-2068 = loop-list-head-2067;
                     block (return)
                       local method go-end-loop ()
                               return-from-nil(tail(loop-list-head-2067));
                             end method go-end-loop,
                             method go-next-loop ()
                               if (not(instance?(g2065, <list>)))
                                 go-end-loop();
                               end if;
                               let loop-desetq-temp = g2065;
                               this-end := head(loop-desetq-temp);
                               loop-desetq-temp := tail(loop-desetq-temp);
                               this-start := head(loop-desetq-temp);
                               g2065 := tail(tail(g2065));
                               if (limit
                                    | (string-seen
                                        := string-seen
                                            | (this-start
                                                & this-end > this-start)))
                                 tail(loop-list-tail-2068)
                                  := (loop-list-tail-2068
                                       := list(if (this-start)
                                                 substr-fn(target-string,
                                                           this-start,
                                                           this-end);
                                               else
                                                 #f;
                                               end if));
                               end if;
                               go-next-loop();
                               go-end-loop();
                             end method go-next-loop;
                       go-next-loop();
                     end block;
                   end block;
                 end block;
               end block;
             end block);
  end block;
end method split;

%define-compiler-macro(#"split",
                       method (whole2069, environment2070)
                         let cmacro-&whole2071 = whole2069;
                         let whole2069
                             = if (~ (#"funcall" == head(whole2069)))
                                 whole2069;
                               else
                                 whole2069 := tail(whole2069);
                               end if;
                         let args2072
                             = if (#"funcall" == head(whole2069))
                                 tail(tail(whole2069));
                               else
                                 tail(whole2069);
                               end if;
                         if (~ list-of-length-at-least-p(args2072, 2))
                           arg-count-error(#"define-compiler-macro", #"split",
                                           args2072,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"target-string",
                                             #"&rest", #"rest"),
                                           2, #f);
                         end if;
                         let env = environment2070;
                         let form = cmacro-&whole2071;
                         let regex = head(tail(whole2069));
                         let target-string = head(tail(tail(whole2069)));
                         let rest = tail(tail(tail(whole2069)));
                         if (constant?(regex, env))
                           backq-list*(#"split",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       target-string, rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"target-string", #"&rest", #"rest"),
                       "Make sure that constant forms are compiled into scanners at compile time.",
                       #(#"compiler-macro-function", #"split"));

define method string-case-modifier (str, from :: <integer>, to :: <integer>,
                                    start :: <integer>, end :: <integer>)
  // Checks whether all words in STR between FROM and TO are upcased,
  // downcased or capitalized and returns a function which applies a
  // corresponding case modification to strings. Returns #'IDENTITY
  // otherwise, especially if words in the target area extend beyond FROM
  // or TO. STR is supposed to be bounded by START and END. It is assumed
  // that (<= START FROM TO END).
  select (if (or(to <= from,
                 and(start < from, alphanumericp(char(str, 1-(from))),
                     alphanumericp(char(str, from))),
                 and(to < end, alphanumericp(char(str, to)),
                     alphanumericp(char(str, 1-(to))))))
            //  if it's a zero-length string or if words extend beyond FROM
            //  or TO we return NIL, i.e. #'IDENTITY
            nil;
            //  otherwise we loop through STR from FROM to TO
            loop(with, last-char-both-case, with, current-result, for, index,
                 of-type, fixnum, from, from, below, to, for, chr, \=,
                 char(str, index), do,
                 cond((not(both-case-p(chr)))(//  this character doesn't have a case so we
                                              //  consider it as a word boundary (note that
                                              //  this differs from how \b works in Perl)
                                              setq(last-char-both-case, nil)),
                      (upper-case-p(chr))(//  an uppercase character
                                          setq(current-result,
                                               if (last-char-both-case)
                                                 //  not the first character in a
                                                 case
                                                   current-result;
                                                   (#"undecided"())(#"upcase");
                                                   (#"downcase"(#"capitalize"))(return(nil));
                                                   (#"upcase"())(current-result);
                                                 end case;
                                                 case
                                                   current-result;
                                                   (nil())(#"undecided");
                                                   (#"downcase"())(return(nil));
                                                   (#"capitalize"(#"upcase"))(current-result);
                                                 end case;
                                               end if,
                                               last-char-both-case, t)),
                      t(//  a lowercase character
                        setq(current-result,
                             case
                               current-result;
                               (nil())(#"downcase");
                               (#"undecided"())(#"capitalize");
                               (#"downcase"())(current-result);
                               (#"capitalize"())(if (last-char-both-case)
                                                   current-result;
                                                   return(nil);
                                                 end if);
                               (#"upcase"())(return(nil));
                             end case,
                             last-char-both-case, t))),
                 finally, return(current-result));
          end if)
    (#())
       => identity;
    (#"undecided", #"upcase")
       => // LTD: Can't convert complex function STRING-UPCASE.
           string-upcase;
    (#"downcase")
       => // LTD: Can't convert complex function STRING-DOWNCASE.
           string-downcase;
    (#"capitalize")
       => string-capitalize;
    otherwise
       => #f;
  end select;
end method string-case-modifier;

//  first create a scanner to identify the special parts of the
//  replacement string (eat your own dog food...)
// Converts a replacement string for REGEX-REPLACE or
// REGEX-REPLACE-ALL into a replacement template which is an
// S-expression.
define generic build-replacement-template (replacement-string) ;

begin
  fluid-bind (*use-bmh-matchers* = #f)
    let reg-scanner = create-scanner("\\\\(?:\\\\|\\{\\d+\\}|\\d+|&|`|')");
    define method build-replacement-template (replacement-string :: <string>)
      let from = 0;
      let collector = #"()";
      let target-string2075 = replacement-string;
      let %start2076 = #f | 0;
      let %end2077 = #f | size(target-string2075);
      let %regex2078 = reg-scanner;
      let scanner2079
          = select (%regex2078 by instance?)
              function
                 => %regex2078;
              #t
                 => create-scanner(%regex2078);
            end select;
      target-string2075
       := begin
            let =string=2082 = target-string2075;
            if (simple-string-p(=string=2082))
              =string=2082;
            else
              as(<simple-string>, =string=2082);
            end if;
          end;
      block (return-from-block-name2081)
        local method go-loop-tag2080 ()
                let (match-start, match-end, reg-starts2073, reg-ends2074)
                    = scan(scanner2079, target-string2075, start: %start2076,
                           end: %end2077, real-start-pos: #f | 0);
                if (~ match-start) return-from-block-name2081(#f); end if;
                begin
                  if (from < match-start)
                    push!(copy-sequence(replacement-string, from,
                                        match-start),
                          collector);
                  end if;
                  let parse-start
                      = find-key(copy-subsequence(replacement-string,
                                                  start: match-start,
                                                  end: match-end),
                                 digit-char?);
                  let token
                      = if (parse-start)
                          // LTD: Function PARSE-INTEGER not yet implemented.
                          parse-integer(replacement-string,
                                        start: parse-start, junk-allowed: #t)
                           - 1;
                        else
                          select (char(replacement-string, 1+(match-start)))
                            ('&')
                               => #"match";
                            ('`')
                               => #"before-match";
                            ('\'')
                               => #"after-match";
                            ('\\')
                               => #"backslash";
                            otherwise
                               => #f;
                          end select;
                        end if;
                  if (instance?(token, <number>) & token < 0)
                    error(// LTD: Can't convert type specification.
                          #"ppcre-invocation-error",
                          format-control: "Illegal substring ~S in replacement string",
                          format-arguments: list(copy-sequence(replacement-string,
                                                               match-start,
                                                               match-end)));
                  end if;
                  push!(token, collector);
                  from := match-end;
                end;
                %start2076
                 := if (match-start = match-end)
                      match-end + 1;
                    else
                      match-end;
                    end if;
                go-loop-tag2080();
              end method go-loop-tag2080;
        go-loop-tag2080();
      end block;
      if (from < size(replacement-string))
        //  push the rest of the replacement string onto the list
        push!(copy-sequence(replacement-string, from), collector);
      end if;
      reverse!(collector);
    end method build-replacement-template;
  end fluid-bind;
end;

define method build-replacement-template (replacement-function :: <function>)
  list(replacement-function);
end method build-replacement-template;

define method build-replacement-template (replacement-function-symbol
                                           :: <symbol>)
  list(replacement-function-symbol);
end method build-replacement-template;

define method build-replacement-template (replacement-list :: <list>)
  replacement-list;
end method build-replacement-template;

//  Corman Lisp's methods can't be closures... :(
//
#f;

define method replace-aux (target-string, replacement, pos-list, reg-list,
                           start, end, preserve-case, simple-calls,
                           element-type)
  // Auxiliary function used by REGEX-REPLACE and
  // REGEX-REPLACE-ALL. POS-LIST contains a list with the start and end
  // positions of all matches while REG-LIST contains a list of arrays
  // representing the corresponding register start and end positions.
  let replacement-template = build-replacement-template(replacement);
  let s
      = // LTD: Function MAKE-STRING-OUTPUT-STREAM not yet implemented.
        make-string-output-stream(element-type: element-type);
  block (nil)
    begin
      let g2083 = concatenate(list(start), pos-list, list(end));
      let from = #f;
      let to = #f;
      let loop-ignore-2084 = #f;
      let replace = #f;
      let reg-starts = #f;
      let reg-ends = #f;
      let curr-replacement = #f;
      let loop-not-first-time = #f;
      local method go-end-loop () #f; end method go-end-loop,
            method go-next-loop ()
              if (not(instance?(g2083, <list>))) go-end-loop(); end if;
              let loop-desetq-temp = g2083;
              from := head(loop-desetq-temp);
              loop-desetq-temp := tail(loop-desetq-temp);
              to := head(loop-desetq-temp);
              g2083 := tail(g2083);
              if (loop-not-first-time)
                replace := ~ replace & to;
              else
                loop-not-first-time := #t;
                replace := #f;
              end if;
              reg-starts := if (replace) pop!(reg-list); else #f; end if;
              reg-ends := if (replace) pop!(reg-list); else #f; end if;
              curr-replacement
               := if (replace)
                    build-replacement(replacement-template, target-string,
                                      start, end, from, to, reg-starts,
                                      reg-ends, simple-calls, element-type);
                  else
                    #f;
                  end if;
              if (~ to) go-end-loop(); end if;
              if (replace)
                write(s,
                      if (preserve-case)
                        (string-case-modifier(target-string, from, to, start,
                                              end))(curr-replacement);
                      else
                        curr-replacement;
                      end if);
              else
                write(s,
                      copy-subsequence(target-string, start: from, end: to));
              end if;
              go-next-loop();
              go-end-loop();
            end method go-next-loop;
      go-next-loop();
    end;
  cleanup
    close(s);
  end block;
  // LTD: Function GET-OUTPUT-STREAM-STRING not yet implemented.
  get-output-stream-string(s);
end method replace-aux;

define method regex-replace (regex, target-string, replacement,
                             #key start = 0, end = size(target-string),
                             preserve-case, simple-calls,
                             element-type = #"character")
  // Try to match TARGET-STRING between START and END against REGEX and
  // replace the first match with REPLACEMENT.  Two values are returned;
  // the modified string, and T if REGEX matched or NIL otherwise.
  // 
  //   REPLACEMENT can be a string which may contain the special substrings
  // "\&" for the whole match, "\`" for the part of TARGET-STRING
  // before the match, "\'" for the part of TARGET-STRING after the
  // match, "\N" or "\{N}" for the Nth register where N is a positive
  // integer.
  // 
  //   REPLACEMENT can also be a function designator in which case the
  // match will be replaced with the result of calling the function
  // designated by REPLACEMENT with the arguments TARGET-STRING, START,
  // END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
  // REG-ENDS are arrays holding the start and end positions of matched
  // registers or NIL - the meaning of the other arguments should be
  // obvious.)
  // 
  //   Finally, REPLACEMENT can be a list where each element is a string,
  // one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
  // corresponding to "\&", "\`", and "\'" above -, an integer N -
  // representing register (1+ N) -, or a function designator.
  // 
  //   If PRESERVE-CASE is true, the replacement will try to preserve the
  // case (all upper case, all lower case, or capitalized) of the
  // match. The result will always be a fresh string, even if REGEX doesn't
  // match.
  // 
  //   ELEMENT-TYPE is the element type of the resulting string.
  let (match-start, match-end, reg-starts, reg-ends)
      = scan(regex, target-string, start: start, end: end);
  if (match-start)
    values(replace-aux(target-string, replacement,
                       list(match-start, match-end),
                       list(reg-starts, reg-ends), start, end, preserve-case,
                       simple-calls, element-type),
           #t);
  else
    values(copy-sequence(target-string, start, end), #f);
  end if;
end method regex-replace;

%define-compiler-macro(#"regex-replace",
                       method (whole2085, environment2086)
                         let cmacro-&whole2087 = whole2085;
                         let whole2085
                             = if (~ (#"funcall" == head(whole2085)))
                                 whole2085;
                               else
                                 whole2085 := tail(whole2085);
                               end if;
                         let args2088
                             = if (#"funcall" == head(whole2085))
                                 tail(tail(whole2085));
                               else
                                 tail(whole2085);
                               end if;
                         if (~ list-of-length-at-least-p(args2088, 3))
                           arg-count-error(#"define-compiler-macro",
                                           #"regex-replace", args2088,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"target-string",
                                             #"replacement", #"&rest",
                                             #"rest"),
                                           3, #f);
                         end if;
                         let env = environment2086;
                         let form = cmacro-&whole2087;
                         let regex = head(tail(whole2085));
                         let target-string = head(tail(tail(whole2085)));
                         let replacement = head(tail(tail(tail(whole2085))));
                         let rest = tail(tail(tail(tail(whole2085))));
                         if (constant?(regex, env))
                           backq-list*(#"regex-replace",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       target-string, replacement, rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"target-string", #"replacement", #"&rest",
                         #"rest"),
                       "Make sure that constant forms are compiled into scanners at compile time.",
                       #(#"compiler-macro-function", #"regex-replace"));

define method regex-replace-all (regex, target-string, replacement,
                                 #key start = 0, end = size(target-string),
                                 preserve-case, simple-calls,
                                 element-type = #"character")
  // Try to match TARGET-STRING between START and END against REGEX and
  // replace all matches with REPLACEMENT.  Two values are returned; the
  // modified string, and T if REGEX matched or NIL otherwise.
  // 
  //   REPLACEMENT can be a string which may contain the special substrings
  // "\&" for the whole match, "\`" for the part of TARGET-STRING
  // before the match, "\'" for the part of TARGET-STRING after the
  // match, "\N" or "\{N}" for the Nth register where N is a positive
  // integer.
  // 
  //   REPLACEMENT can also be a function designator in which case the
  // match will be replaced with the result of calling the function
  // designated by REPLACEMENT with the arguments TARGET-STRING, START,
  // END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
  // REG-ENDS are arrays holding the start and end positions of matched
  // registers or NIL - the meaning of the other arguments should be
  // obvious.)
  // 
  //   Finally, REPLACEMENT can be a list where each element is a string,
  // one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
  // corresponding to "\&", "\`", and "\'" above -, an integer N -
  // representing register (1+ N) -, or a function designator.
  // 
  //   If PRESERVE-CASE is true, the replacement will try to preserve the
  // case (all upper case, all lower case, or capitalized) of the
  // match. The result will always be a fresh string, even if REGEX doesn't
  // match.
  // 
  //   ELEMENT-TYPE is the element type of the resulting string.
  let pos-list = #"()";
  let reg-list = #"()";
  let target-string2089 = target-string;
  let %start2090 = start | 0;
  let %end2091 = end | size(target-string2089);
  let %regex2092 = regex;
  let scanner2093
      = select (%regex2092 by instance?)
          function
             => %regex2092;
          #t
             => create-scanner(%regex2092);
        end select;
  target-string2089
   := begin
        let =string=2096 = target-string2089;
        if (simple-string-p(=string=2096))
          =string=2096;
        else
          as(<simple-string>, =string=2096);
        end if;
      end;
  block (return-from-block-name2095)
    local method go-loop-tag2094 ()
            let (match-start, match-end, reg-starts, reg-ends)
                = scan(scanner2093, target-string2089, start: %start2090,
                       end: %end2091, real-start-pos: start | 0);
            if (~ match-start) return-from-block-name2095(#f); end if;
            begin
              push!(match-start, pos-list);
              push!(match-end, pos-list);
              push!(reg-starts, reg-list);
              push!(reg-ends, reg-list);
            end;
            %start2090
             := if (match-start = match-end)
                  match-end + 1;
                else
                  match-end;
                end if;
            go-loop-tag2094();
          end method go-loop-tag2094;
    go-loop-tag2094();
  end block;
  if (pos-list)
    values(replace-aux(target-string, replacement, reverse!(pos-list),
                       reverse!(reg-list), start, end, preserve-case,
                       simple-calls, element-type),
           #t);
  else
    values(copy-sequence(target-string, start, end), #f);
  end if;
end method regex-replace-all;

%define-compiler-macro(#"regex-replace-all",
                       method (whole2097, environment2098)
                         let cmacro-&whole2099 = whole2097;
                         let whole2097
                             = if (~ (#"funcall" == head(whole2097)))
                                 whole2097;
                               else
                                 whole2097 := tail(whole2097);
                               end if;
                         let args2100
                             = if (#"funcall" == head(whole2097))
                                 tail(tail(whole2097));
                               else
                                 tail(whole2097);
                               end if;
                         if (~ list-of-length-at-least-p(args2100, 3))
                           arg-count-error(#"define-compiler-macro",
                                           #"regex-replace-all", args2100,
                                           #(#"&whole", #"form",
                                             #"&environment", #"env",
                                             #"regex", #"target-string",
                                             #"replacement", #"&rest",
                                             #"rest"),
                                           3, #f);
                         end if;
                         let env = environment2098;
                         let form = cmacro-&whole2099;
                         let regex = head(tail(whole2097));
                         let target-string = head(tail(tail(whole2097)));
                         let replacement = head(tail(tail(tail(whole2097))));
                         let rest = tail(tail(tail(tail(whole2097))));
                         if (constant?(regex, env))
                           backq-list*(#"regex-replace-all",
                                       backq-list(#"load-time-value",
                                                  backq-list(#"create-scanner",
                                                             regex)),
                                       target-string, replacement, rest);
                         else
                           form;
                         end if;
                       end method,
                       #(#"&whole", #"form", #"&environment", #"env",
                         #"regex", #"target-string", #"replacement", #"&rest",
                         #"rest"),
                       "Make sure that constant forms are compiled into scanners at compile time.",
                       #(#"compiler-macro-function", #"regex-replace-all"));

// LTD: No macros.
#"regex-apropos-aux";

//  The following two functions were provided by Karsten Poeck
// 
//
#f;

define method print-symbol-info (symbol)
  // Auxiliary function used by REGEX-APROPOS. Tries to print some
  // meaningful information about a symbol.
  // LTD: Function HANDLER-CASE not yet implemented.
  handler-case(begin
                 let output-list = #"()";
                 if (special-operator-p(symbol))
                   push!("[special operator]", output-list);
                 elseif (// LTD: Function MACRO-FUNCTION not yet implemented.
                         macro-function(symbol))
                   push!("[macro]", output-list);
                 elseif (// LTD: Function FBOUNDP not yet implemented.
                         fboundp(symbol))
                   begin
                     let function = symbol;
                     let compiledp = instance?(function, <function>);
                     let (lambda-expr, closurep)
                         = // LTD: Function FUNCTION-LAMBDA-EXPRESSION not yet implemented.
                           function-lambda-expression(function);
                     push!((method (stream,
                                    #key format-arg-2101
                                          = error(// LTD: Can't convert type specification.
                                                  #"format-error",
                                                  complaint: "required argument missing",
                                                  control-string: "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]",
                                                  offset: 3),
                                    format-arg-2102
                                     = error(// LTD: Can't convert type specification.
                                             #"format-error",
                                             complaint: "required argument missing",
                                             control-string: "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]",
                                             offset: 19),
                                    format-arg-2103
                                     = error(// LTD: Can't convert type specification.
                                             #"format-error",
                                             complaint: "required argument missing",
                                             control-string: "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]",
                                             offset: 42),
                                    #rest args)
                              begin
                                write(stream, "[");
                                if (format-arg-2101)
                                  write(stream, "compiled ");
                                else
                                  #f;
                                end if;
                                if (format-arg-2102)
                                  write(stream, "closure");
                                else
                                  write(stream, "function");
                                end if;
                                write(stream, "]");
                                if (format-arg-2103)
                                  write(stream, " ");
                                  print(if (args)
                                          pop!(args);
                                        else
                                          error(// LTD: Can't convert type specification.
                                                #"format-error",
                                                complaint: "no more arguments",
                                                control-string: "[~:[~;compiled ~]~:[function~;closure~]]~:[~; ~A~]",
                                                offset: 47);
                                        end if,
                                        stream);
                                else
                                  #f;
                                end if;
                              end;
                              args;
                            end method)(#f, compiledp, closurep, lambda-expr,
                                        second(lambda-expr)),
                           output-list);
                   end;
                 end if;
                 let class = symbol;
                 if (class)
                   push!(format(#f, "[class] %=", class), output-list);
                 end if;
                 if (instance?(symbol, <symbol>))
                   push!("[keyword]", output-list);
                 elseif (constant?(symbol))
                   push!((method (stream,
                                  #key format-arg-2105
                                        = error(// LTD: Can't convert type specification.
                                                #"format-error",
                                                complaint: "required argument missing",
                                                control-string: "[constant]~:[~; value: ~S~]",
                                                offset: 12),
                                  #rest args)
                            begin
                              write(stream, "[constant]");
                              if (format-arg-2105)
                                write(stream, " value: ");
                                print(if (args)
                                        pop!(args);
                                      else
                                        error(// LTD: Can't convert type specification.
                                              #"format-error",
                                              complaint: "no more arguments",
                                              control-string: "[constant]~:[~; value: ~S~]",
                                              offset: 24);
                                      end if,
                                      stream);
                              else
                                #f;
                              end if;
                            end;
                            args;
                          end method)(#f,
                                      // LTD: Function BOUNDP not yet implemented.
                                      boundp(symbol),
                                      symbol),
                         output-list);
                 elseif (// LTD: Function BOUNDP not yet implemented.
                         boundp(symbol))
                   push!(format(#f, "[variable] value: %=", symbol),
                         output-list);
                 end if;
                 (method (stream,
                          #key format-arg-2107
                                = error(// LTD: Can't convert type specification.
                                        #"format-error",
                                        complaint: "required argument missing",
                                        control-string: "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>",
                                        offset: 3),
                          format-arg-2108
                           = error(// LTD: Can't convert type specification.
                                   #"format-error",
                                   complaint: "required argument missing",
                                   control-string: "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>",
                                   offset: 6),
                          #rest args)
                    begin
                      write-element(stream, '\n');
                      print(format-arg-2107, stream);
                      write(stream, " ");
                      let arg = format-arg-2108;
                      let with-pretty-stream-2114
                          = method (stream)
                              let g2110 = arg;
                              if (instance?(g2110, <list>))
                                local method with-circularity-detection-body-2116 ()
                                        let g2117
                                            = method ()
                                                let pprint-logical-block-length-2112
                                                     :: <index>
                                                    = 0;
                                                start-logical-block(stream,
                                                                    "", #f,
                                                                    "");
                                                block (return-from-pprint-logical-block-2111)
                                                  begin
                                                    let pprint-pop-2113
                                                        = method ()
                                                            if (~ instance?(g2110,
                                                                            <list>))
                                                              write(stream,
                                                                    ". ");
                                                              output-object(g2110,
                                                                            stream);
                                                              return-from-pprint-logical-block-2111(#f);
                                                            end if;
                                                            if (~ *print-readably*
                                                                 & pprint-logical-block-length-2112
                                                                    == *print-length*)
                                                              write(stream,
                                                                    "...");
                                                              return-from-pprint-logical-block-2111(#f);
                                                            end if;
                                                            if (g2110
                                                                 & positive?(pprint-logical-block-length-2112)
                                                                 & check-for-circularity(g2110,
                                                                                         #f,
                                                                                         #"logical-block"))
                                                              write(stream,
                                                                    ". ");
                                                              output-object(g2110,
                                                                            stream);
                                                              return-from-pprint-logical-block-2111(#f);
                                                            end if;
                                                            inc!(pprint-logical-block-length-2112);
                                                            pop!(g2110);
                                                          end method;
                                                    // LTD: Function MACROLET not yet implemented.
                                                    macrolet((// LTD: Function PPRINT-POP not yet implemented.
                                                              pprint-pop(#f,
                                                                         #(#"pprint-pop-2113")))(// LTD: Function PPRINT-EXIT-IF-LIST-EXHAUSTED not yet implemented.
                                                                                                 pprint-exit-if-list-exhausted(#f,
                                                                                                                               #(#"when",
                                                                                                                                 #(#"null",
                                                                                                                                   #"g2110"),
                                                                                                                                 #(#"return-from",
                                                                                                                                   #"pprint-logical-block-2111",
                                                                                                                                   #())))),
                                                             #f,
                                                             begin
                                                               let args = arg;
                                                               let orig-args
                                                                   = arg;
                                                               block (return)
                                                                 begin
                                                                   if (begin
                                                                         let g966
                                                                             = #f;
                                                                         let g967
                                                                             = #f;
                                                                         let g968
                                                                             = #f;
                                                                         if (g968)
                                                                           g966
                                                                            <= g967
                                                                            & g967
                                                                               <= g968;
                                                                         elseif (g967)
                                                                           g966
                                                                            == g967;
                                                                         elseif (g966)
                                                                           g966
                                                                            == 0;
                                                                         else
                                                                           empty?(args);
                                                                         end if;
                                                                       end)
                                                                     return(#f);
                                                                   end if;
                                                                   print(begin
                                                                           if (empty?(args))
                                                                             error(// LTD: Can't convert type specification.
                                                                                   #"format-error",
                                                                                   complaint: "no more arguments",
                                                                                   control-string: "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>",
                                                                                   offset: 12);
                                                                           end if;
                                                                           // LTD: Function PPRINT-POP not yet implemented.
                                                                           pprint-pop();
                                                                           pop!(args);
                                                                         end,
                                                                         stream);
                                                                   block (return)
                                                                     while (#t)
                                                                       if (empty?(args))
                                                                         return(#f);
                                                                       end if;
                                                                       // LTD: Function PPRINT-NEWLINE not yet implemented.
                                                                       pprint-newline(mandatory: stream);
                                                                       print(begin
                                                                               if (empty?(args))
                                                                                 error(// LTD: Can't convert type specification.
                                                                                       #"format-error",
                                                                                       complaint: "no more arguments",
                                                                                       control-string: "~&~S ~<~;~^~A~@{~:@_~A~}~;~:>",
                                                                                       offset: 21);
                                                                               end if;
                                                                               // LTD: Function PPRINT-POP not yet implemented.
                                                                               pprint-pop();
                                                                               pop!(args);
                                                                             end,
                                                                             stream);
                                                                     end while;
                                                                   end block;
                                                                 end;
                                                               end block;
                                                             end);
                                                  end;
                                                end block;
                                                end-logical-block(stream);
                                              end method;
                                        if (empty?(*print-readably*)
                                             & *print-level*
                                             & *current-level-in-print*
                                                >= *print-level*)
                                          write-element(stream, '#');
                                        else
                                          begin
                                            fluid-bind (*current-level-in-print*
                                                         = *current-level-in-print*
                                                            + 1)
                                              g2117();
                                            end fluid-bind;
                                          end;
                                        end if;
                                      end method with-circularity-detection-body-2116;
                                if (~ *print-circle*)
                                  with-circularity-detection-body-2116();
                                elseif (*circularity-hash-table*)
                                  begin
                                    let with-circularity-detection-2115
                                        = check-for-circularity(g2110, #t,
                                                                #"logical-block");
                                    if (with-circularity-detection-2115)
                                      if (handle-circularity(with-circularity-detection-2115,
                                                             stream))
                                        with-circularity-detection-body-2116();
                                      end if;
                                    else
                                      with-circularity-detection-body-2116();
                                    end if;
                                  end;
                                else
                                  begin
                                    fluid-bind (*circularity-hash-table*
                                                 = make(<object-table>,
                                                        test: \==))
                                      output-object(g2110,
                                                    // LTD: Function MAKE-BROADCAST-STREAM not yet implemented.
                                                    make-broadcast-stream());
                                      fluid-bind (*circularity-counter* = 0)
                                        let with-circularity-detection-2115
                                            = check-for-circularity(g2110, #t,
                                                                    #"logical-block");
                                        if (with-circularity-detection-2115)
                                          handle-circularity(with-circularity-detection-2115,
                                                             stream);
                                        end if;
                                        with-circularity-detection-body-2116();
                                      end fluid-bind;
                                    end fluid-bind;
                                  end;
                                end if;
                              else
                                output-object(g2110, stream);
                              end if;
                            end method;
                      let stream
                          = begin
                              let once-only-2109 = stream;
                              select (once-only-2109)
                                (#())
                                   => *standard-output*;
                                (#"t")
                                   => *terminal-io*;
                                otherwise
                                   => once-only-2109;
                              end select;
                            end;
                      if (pretty-stream-p(stream))
                        with-pretty-stream-2114(stream);
                      else
                        block (line-limit-abbreviation-happened)
                          let stream = make-pretty-stream(stream);
                          with-pretty-stream-2114(stream);
                          force-pretty-output(stream);
                        end block;
                      end if;
                      #f;
                    end;
                    args;
                  end method)(#t, symbol, output-list);
               end,
               condition(#(),
                         //  this seems to be necessary due to some errors I encountered
                         //  with LispWorks
                         format-out("\n%= [an error occured while trying to print more info]",
                                    symbol)));
end method print-symbol-info;

define method regex-apropos (regex, #key packages, case-insensitive = #t)
  // Similar to the standard function APROPOS but returns a list of all
  // symbols which match the regular expression REGEX. If CASE-INSENSITIVE
  // is true and REGEX isn't already a scanner, a case-insensitive scanner
  // is used.
  let regex2118 = regex;
  let scanner2119
      = create-scanner(regex2118,
                       case-insensitive-mode: case-insensitive
                                               & ~ instance?(regex2118,
                                                             <function>));
  let %packages2120
      = packages
         | // LTD: Function LIST-ALL-PACKAGES not yet implemented.
           list-all-packages();
  let g2124 = %packages2120;
  let g2123
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
            if (instance?(g2124, <pair>)) g2124; else list(g2124); end if);
  let g2125 = #f;
  let g2126 = head(g2123);
  let g2127 = #f;
  let g2128 = #f;
  let g2129 = #f;
  g2129 := package-%use-list(head(g2123));
  // LTD: Function MACROLET not yet implemented.
  macrolet((g2130(next-kind(), #f,
                  begin
                    let symbols = generate-symbol();
                    backq-list(#"progn",
                               backq-list(#"setf", #"g2126", next-kind),
                               #(#"setf", #"g2125", #()),
                               select (next-kind)
                                 #"internal"
                                    => backq-list(#"let",
                                                  backq-list(backq-cons(symbols,
                                                                        #(#(#"package-internal-symbols",
                                                                            #(#"car",
                                                                              #"g2123"))))),
                                                  backq-list(#"when", symbols,
                                                             backq-list(#"setf",
                                                                        #"g2128",
                                                                        backq-list(#"package-hashtable-table",
                                                                                   symbols)),
                                                             backq-list(#"setf",
                                                                        #"g2127",
                                                                        backq-list(#"package-hashtable-hash",
                                                                                   symbols))));
                                 #"external"
                                    => backq-list(#"let",
                                                  backq-list(backq-cons(symbols,
                                                                        #(#(#"package-external-symbols",
                                                                            #(#"car",
                                                                              #"g2123"))))),
                                                  backq-list(#"when", symbols,
                                                             backq-list(#"setf",
                                                                        #"g2128",
                                                                        backq-list(#"package-hashtable-table",
                                                                                   symbols)),
                                                             backq-list(#"setf",
                                                                        #"g2127",
                                                                        backq-list(#"package-hashtable-hash",
                                                                                   symbols))));
                                 #"inherited"
                                    => backq-list(#"let",
                                                  backq-list(backq-cons(symbols,
                                                                        #(#(#"and",
                                                                            #"g2129",
                                                                            #(#"package-external-symbols",
                                                                              #(#"car",
                                                                                #"g2129")))))),
                                                  backq-list(#"when", symbols,
                                                             backq-list(#"setf",
                                                                        #"g2128",
                                                                        backq-list(#"package-hashtable-table",
                                                                                   symbols)),
                                                             backq-list(#"setf",
                                                                        #"g2127",
                                                                        backq-list(#"package-hashtable-hash",
                                                                                   symbols))));
                                 otherwise
                                    => #f;
                               end select);
                  end))(g2131(this-kind(),
                              begin
                                let next-kind
                                    = second(member?(this-kind,
                                                     #(#"internal",
                                                       #"external",
                                                       #"inherited")));
                                if (next-kind)
                                  backq-list(#"g2130", next-kind);
                                else
                                  backq-list(#"if",
                                             #(#"endp",
                                               #(#"setf", #"g2123",
                                                 #(#"cdr", #"g2123"))),
                                             #(#"return-from", #"g2134"),
                                             backq-list(#"g2130",
                                                        head(#(#"internal",
                                                               #"external",
                                                               #"inherited"))));
                                end if;
                              end)),
           if (g2123)
             #f;
             #f;
             g2130(#"internal");
             let g2132 = method (number) number > 1; end method;
             // LTD: Function MACROLET not yet implemented.
             macrolet((next2121(#f, #f,
                                backq-list(#"block", #"g2134",
                                           backq-list(#"loop",
                                                      backq-list*(#"case",
                                                                  #"g2126",
                                                                  backq-append(if (member?(internal: #(#"internal",
                                                                                                       #"external",
                                                                                                       #"inherited")))
                                                                                 #(#(#"internal",
                                                                                     #(#"setf",
                                                                                       #"g2125",
                                                                                       #(#"position-if",
                                                                                         #(#"function",
                                                                                           #"g2132"),
                                                                                         #(#"the",
                                                                                           #"hash-vector",
                                                                                           #"g2127"),
                                                                                         #"start",
                                                                                         #(#"if",
                                                                                           #"g2125",
                                                                                           #(#"1+",
                                                                                             #"g2125"),
                                                                                           0))),
                                                                                     #(#"if",
                                                                                       #"g2125",
                                                                                       #(#"return-from",
                                                                                         #"g2134",
                                                                                         #(#"values",
                                                                                           #"t",
                                                                                           #(#"svref",
                                                                                             #"g2128",
                                                                                             #"g2125"),
                                                                                           #"g2126",
                                                                                           #(#"car",
                                                                                             #"g2123"))),
                                                                                       #(#"g2131",
                                                                                         #"internal"))));
                                                                               end if,
                                                                               if (member?(external: #(#"internal",
                                                                                                       #"external",
                                                                                                       #"inherited")))
                                                                                 #(#(#"external",
                                                                                     #(#"setf",
                                                                                       #"g2125",
                                                                                       #(#"position-if",
                                                                                         #(#"function",
                                                                                           #"g2132"),
                                                                                         #(#"the",
                                                                                           #"hash-vector",
                                                                                           #"g2127"),
                                                                                         #"start",
                                                                                         #(#"if",
                                                                                           #"g2125",
                                                                                           #(#"1+",
                                                                                             #"g2125"),
                                                                                           0))),
                                                                                     #(#"if",
                                                                                       #"g2125",
                                                                                       #(#"return-from",
                                                                                         #"g2134",
                                                                                         #(#"values",
                                                                                           #"t",
                                                                                           #(#"svref",
                                                                                             #"g2128",
                                                                                             #"g2125"),
                                                                                           #"g2126",
                                                                                           #(#"car",
                                                                                             #"g2123"))),
                                                                                       #(#"g2131",
                                                                                         #"external"))));
                                                                               end if,
                                                                               if (member?(inherited: #(#"internal",
                                                                                                        #"external",
                                                                                                        #"inherited")))
                                                                                 backq-list(backq-list(#"inherited",
                                                                                                       #(#"flet",
                                                                                                         #(#(#"g2133",
                                                                                                             #(#"number"),
                                                                                                             #(#"when",
                                                                                                               #(#"g2132",
                                                                                                                 #"number"),
                                                                                                               #(#"let*",
                                                                                                                 #(#(#"p",
                                                                                                                     #(#"position",
                                                                                                                       #"number",
                                                                                                                       #(#"the",
                                                                                                                         #"hash-vector",
                                                                                                                         #"g2127"),
                                                                                                                       #"start",
                                                                                                                       #(#"if", 
                                                                                                                         #"g2125",
                                                                                                                         #(#"1+",
                                                                                                                           #"g2125"),
                                                                                                                         0))),
                                                                                                                   #(#"s",
                                                                                                                     #(#"svref",
                                                                                                                       #"g2128",
                                                                                                                       #"p"))), 
                                                                                                                 #(#"eql",
                                                                                                                   #(#"nth-value",
                                                                                                                     1,
                                                                                                                     #(#"find-symbol",
                                                                                                                       #(#"symbol-name",
                                                                                                                         #"s"), 
                                                                                                                       #(#"car",
                                                                                                                         #"g2123"))),
                                                                                                                   #"inherited"))))),
                                                                                                         #(#"setf",
                                                                                                           #"g2125",
                                                                                                           #(#"when",
                                                                                                             #"g2127",
                                                                                                             #(#"position-if",
                                                                                                               #(#"function",
                                                                                                                 #"g2133"),
                                                                                                               #(#"the",
                                                                                                                 #"hash-vector",
                                                                                                                 #"g2127"),
                                                                                                               #"start",
                                                                                                               #(#"if",
                                                                                                                 #"g2125",
                                                                                                                 #(#"1+",
                                                                                                                   #"g2125"),
                                                                                                                 0))))),
                                                                                                       backq-list(#"cond",
                                                                                                                  #(#"g2125",
                                                                                                                    #(#"return-from",
                                                                                                                      #"g2134", 
                                                                                                                      #(#"values",
                                                                                                                        #"t",
                                                                                                                        #(#"svref",
                                                                                                                          #"g2128",
                                                                                                                          #"g2125"),
                                                                                                                        #"g2126",
                                                                                                                        #(#"car",
                                                                                                                          #"g2123")))),
                                                                                                                  backq-list(#"t",
                                                                                                                             #(#"setf",
                                                                                                                               #"g2129",
                                                                                                                               #(#"cdr",
                                                                                                                                 #"g2129")),
                                                                                                                             backq-list*(#"cond",
                                                                                                                                         backq-list(#(#"endp",
                                                                                                                                                      #"g2129"),
                                                                                                                                                    #(#"setf",
                                                                                                                                                      #"g2123",
                                                                                                                                                      #(#"cdr",
                                                                                                                                                        #"g2123")),
                                                                                                                                                    #(#"when",
                                                                                                                                                      #(#"endp",
                                                                                                                                                        #"g2123"),
                                                                                                                                                      #(#"return-from",
                                                                                                                                                        #"g2134")),
                                                                                                                                                    #(#"setf",
                                                                                                                                                      #"g2129",
                                                                                                                                                      #(#"package-%use-list",
                                                                                                                                                        #(#"car",
                                                                                                                                                          #"g2123"))),
                                                                                                                                                    backq-list(#"g2130",
                                                                                                                                                               head(#(#"internal",
                                                                                                                                                                      #"external",
                                                                                                                                                                      #"inherited")))),
                                                                                                                                         #(#(#"t",
                                                                                                                                             #(#"g2130",
                                                                                                                                               #"inherited"),
                                                                                                                                             #(#"setf",
                                                                                                                                               #"g2125",
                                                                                                                                               #()))))))));
                                                                               end if))))))(),
                      block (return)
                        while (#t)
                          let (morep2122, symbol) = next2121();
                          if (~ morep2122) return(#f); end if;
                          if (scan(scanner2119, as(<string>, symbol)))
                            print-symbol-info(symbol);
                          end if;
                        end while;
                      end block);
           end if);
  values();
end method regex-apropos;

begin
  fluid-bind (*use-bmh-matchers* = #f)
    let non-word-char-scanner = create-scanner("[^a-zA-Z_0-9]");
    define method quote-meta-chars (string, #key start = 0,
                                    end = size(string))
      // Quote, i.e. prefix with #\\, all non-word characters in STRING.
      regex-replace-all(non-word-char-scanner, string, "\\\\\\&",
                        start: start, end: end);
    end method quote-meta-chars;
  end fluid-bind;
end;

begin
  fluid-bind (*use-bmh-matchers* = #f)
    fluid-bind (*allow-quoting* = #f)
      let quote-char-scanner = create-scanner("\\\\Q");
      let section-scanner
          = create-scanner("\\\\Q((?:[^\\\\]|\\\\(?!Q))*?)(?:\\\\E|$)");
      define method quote-sections (string)
        // Replace sections inside of STRING which are enclosed by \Q and
        // \E with the quoted equivalent of these sections (see
        // QUOTE-META-CHARS). Repeat this as long as there are such
        // sections. These sections may nest.
        let quote-substring
            = method (target-string, start, end, match-start, match-end,
                      reg-starts, reg-ends)
                quote-meta-chars(target-string, start: reg-starts[0],
                                                                  end: reg-ends[0]);
              end method;
        block (return)
          for (result = string then regex-replace-all(section-scanner, result,
                                                      quote-substring),
               while scan(quote-char-scanner, result))
          finally
            return(result);
            #f;
          end for;
        end block;
      end method quote-sections;
    end fluid-bind;
  end fluid-bind;
end;

begin
  fluid-bind (*use-bmh-matchers* = #f)
    let comment-scanner = create-scanner("(?s)\\(\\?#.*?\\)");
    let extended-comment-scanner
        = create-scanner("(?m:#.*?$)|(?s:\\(\\?#.*?\\))");
    let quote-token-scanner = "\\\\[QE]";
    let quote-token-replace-scanner = "\\\\([QE])";
    define method clean-comments (string, #key extended-mode)
      // Clean (?#...) comments within STRING for quoting, i.e. convert
      // \Q to Q and \E to E. If EXTENDED-MODE is true, also clean
      // end-of-line comments, i.e. those starting with #\# and ending with
      // #\Newline.
      let remove-tokens
          = method (target-string, start, end, match-start, match-end,
                    reg-starts, reg-ends)
              block (return)
                for (result = nsubseq(target-string, match-start,
                                      match-end) then regex-replace-all(quote-token-replace-scanner,
                                                                        result,
                                                                        "\\1"),
                     while scan(quote-token-scanner, result))
                finally
                  return(result);
                  #f;
                end for;
              end block;
            end method;
      regex-replace-all(if (extended-mode)
                          extended-comment-scanner;
                        else
                          comment-scanner;
                        end if,
                        string, remove-tokens);
    end method clean-comments;
  end fluid-bind;
end;

define method parse-tree-synonym (symbol)
  // Returns the parse tree the SYMBOL symbol is a synonym for. Returns
  // NIL is SYMBOL wasn't yet defined to be a synonym.
  symbol-get-property(symbol, #"parse-tree-synonym");
end method parse-tree-synonym;

define method setf(parse-tree-synonym) (new-parse-tree, symbol)
  // Defines SYMBOL to be a synonm for the parse tree NEW-PARSE-TREE.
  symbol-get-property(symbol, #"parse-tree-synonym") := new-parse-tree;
end method setf(parse-tree-synonym);

// LTD: No macros.
#"define-parse-tree-synonym";

