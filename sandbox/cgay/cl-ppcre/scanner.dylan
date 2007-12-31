//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/scanner.lisp,v 1.28 2007/01/01 23:43:11 edi Exp $
//  Here the scanner for the actual regex as well as utility scanners
//  for the constant start and end strings are created.
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

// LTD: No macros.
#"bmh-matcher-aux";

define method create-bmh-matcher (pattern, case-insensitive-p)
  // Returns a Boyer-Moore-Horspool matcher which searches the (special)
  // simple-string *STRING* for the first occurence of the substring
  // PATTERN. The search starts at the position START-POS within *STRING*
  // and stops before *END-POS* is reached. Depending on the second
  // argument the search is case-insensitive or not. If the special
  // variable *USE-BMH-MATCHERS* is NIL, use the standard SEARCH function
  // instead. (BMH matchers are faster but need much more space.)
  block (return-from-create-bmh-matcher)
    //  see <http://www-igm.univ-mlv.fr/~lecroq/string/node18.html> for
    //  details
    if (~ *use-bmh-matchers*)
      let test = if (case-insensitive-p) char-equal?; else \=; end if;
      return-from-create-bmh-matcher(method (start-pos :: <integer>)
                                       ~ negative?(start-pos)
                                        & subsequence-position(pattern,
                                                               copy-subsequence(*string*,
                                                                                start: start-pos,
                                                                                end: *end-pos*));
                                     end method);
    end if;
    let m :: <integer> = size(pattern);
    let skip = make(<array>, dimensions: *regex-char-code-limit*);
    for (k :: <integer> from 0 below m)
      if (case-insensitive-p)
        begin
          skip[as(<integer>, as-uppercase(pattern[k]))] := m - k - 1;
          skip[as(<integer>, as-lowercase(pattern[k]))] := m - k - 1;
        end;
      else
        skip[as(<integer>, pattern[k])] := m - k - 1;
      end if;
    end for;
    if (case-insensitive-p)
      method (start-pos :: <integer>)
        if (negative?(start-pos) | start-pos + m > *end-pos*)
          #f;
        else
          block (return-from-bmh-matcher)
            for (k
                  :: <integer> = start-pos + m
                                  + -1 then k
                                             + max(1,
                                                   skip[as(<integer>,
                                                           *string*[k])]),
                 while k < *end-pos*)
              for (j :: <integer> from m - 1 by -1,
                   i :: <integer> from k by -1,
                   while j >= 0 & char-equal?(*string*[i], pattern[j]))
              finally
                if (negative?(j)) return-from-bmh-matcher(i + 1); end if;
                #f;
              end for;
            end for;
          end block;
        end if;
      end method;
    else
      method (start-pos :: <integer>)
        if (negative?(start-pos) | start-pos + m > *end-pos*)
          #f;
        else
          block (return-from-bmh-matcher)
            for (k
                  :: <integer> = start-pos + m
                                  + -1 then k
                                             + max(1,
                                                   skip[as(<integer>,
                                                           *string*[k])]),
                 while k < *end-pos*)
              for (j :: <integer> from m - 1 by -1,
                   i :: <integer> from k by -1,
                   while j >= 0 & *string*[i] = pattern[j])
              finally
                if (negative?(j)) return-from-bmh-matcher(i + 1); end if;
                #f;
              end for;
            end for;
          end block;
        end if;
      end method;
    end if;
  end block;
end method create-bmh-matcher;

// LTD: No macros.
#"char-searcher-aux";

#f;

// LTD: No macros.
#"insert-advance-fn";

define method create-scanner-aux (match-fn, min-len :: <integer>,
                                  start-anchored-p, starts-with,
                                  start-string-test, end-anchored-p,
                                  end-string-test, end-string-len,
                                  end-string-offset, rep-num :: <integer>,
                                  zero-length-num :: <integer>,
                                  reg-num :: <integer>)
  // Auxiliary function to create and return a scanner (which is
  // actually a closure). Used by CREATE-SCANNER.
  let starts-with-len
      = if (instance?(starts-with, <str>)) starts-with.len; end if;
  let starts-with-everything = instance?(starts-with, <everything>);
  if (start-string-test & end-string-test & end-string-offset)
    //  we know that the regular expression has constant start and
    //  end strings and we know the end string's offset (from the
    //  left)
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  block (return-from-advance-fn)
                                    while (#t)
                                      if (~ (pos := start-string-test(pos)))
                                        return-from-scan(#f);
                                      end if;
                                      begin
                                        if (pos
                                             = *end-string-pos*
                                                - end-string-offset)
                                          return-from-advance-fn(pos);
                                        end if;
                                        let try-pos = pos + starts-with-len;
                                        block (return)
                                          while (#t)
                                            if (~ (*end-string-pos*
                                                    := end-string-test(try-pos)))
                                              return-from-scan(#f);
                                            end if;
                                            let new-pos :: <integer>
                                                = *end-string-pos*
                                                   - end-string-offset;
                                            if (new-pos = pos)
                                              return-from-advance-fn(pos);
                                            elseif (new-pos > pos)
                                              pos := new-pos;
                                              return(#f);
                                            else
                                              try-pos := *end-string-pos* + 1;
                                            end if;
                                          end while;
                                        end block;
                                      end;
                                    end while;
                                  end block;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (starts-with-everything & end-string-test & end-string-offset)
    //  we know that the regular expression starts with ".*" (which
    //  is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
    //  with a constant end string and we know the end string's
    //  offset (from the left)
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  block (return-from-advance-fn)
                                    while (#t)
                                      if (~ (pos := newline-skipper(pos)))
                                        return-from-scan(#f);
                                      end if;
                                      begin
                                        if (pos
                                             = *end-string-pos*
                                                - end-string-offset)
                                          return-from-advance-fn(pos);
                                        end if;
                                        let try-pos = pos;
                                        block (return)
                                          while (#t)
                                            if (~ (*end-string-pos*
                                                    := end-string-test(try-pos)))
                                              return-from-scan(#f);
                                            end if;
                                            let new-pos :: <integer>
                                                = *end-string-pos*
                                                   - end-string-offset;
                                            if (new-pos = pos)
                                              return-from-advance-fn(pos);
                                            elseif (new-pos > pos)
                                              pos := new-pos;
                                              return(#f);
                                            else
                                              try-pos := *end-string-pos* + 1;
                                            end if;
                                          end while;
                                        end block;
                                      end;
                                    end while;
                                  end block;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (start-string-test & end-string-test)
    //  we know that the regular expression has constant start and
    //  end strings; similar to the first case but we only need to
    //  check for the end string, it doesn't provide enough
    //  information to advance POS
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  block (return-from-advance-fn)
                                    if (~ (pos := start-string-test(pos)))
                                      return-from-scan(#f);
                                    end if;
                                    if (pos <= *end-string-pos*)
                                      return-from-advance-fn(pos);
                                    end if;
                                    if (~ (*end-string-pos*
                                            := end-string-test(pos)))
                                      return-from-scan(#f);
                                    end if;
                                    pos;
                                  end block;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (starts-with-everything & end-string-test)
    //  we know that the regular expression starts with ".*" (which
    //  is not in single-line-mode, see CREATE-SCANNER-AUX) and ends
    //  with a constant end string; similar to the second case but we
    //  only need to check for the end string, it doesn't provide
    //  enough information to advance POS
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  block (return-from-advance-fn)
                                    if (~ (pos := newline-skipper(pos)))
                                      return-from-scan(#f);
                                    end if;
                                    if (pos <= *end-string-pos*)
                                      return-from-advance-fn(pos);
                                    end if;
                                    if (~ (*end-string-pos*
                                            := end-string-test(pos)))
                                      return-from-scan(#f);
                                    end if;
                                    pos;
                                  end block;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (start-string-test)
    //  just check for constant start string candidate
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  if (~ (pos := start-string-test(pos)))
                                    return-from-scan(#f);
                                  end if;
                                  pos;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (starts-with-everything)
    //  just advance POS with NEWLINE-SKIPPER
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  if (~ (pos := newline-skipper(pos)))
                                    return-from-scan(#f);
                                  end if;
                                  pos;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  elseif (end-string-test)
    //  just check for the next end string candidate if POS has
    //  advanced beyond the last one
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  block (return-from-advance-fn)
                                    if (pos <= *end-string-pos*)
                                      return-from-advance-fn(pos);
                                    end if;
                                    if (~ (*end-string-pos*
                                            := end-string-test(pos)))
                                      return-from-scan(#f);
                                    end if;
                                    pos;
                                  end block;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  else
    //  not enough optimization information about the regular
    //  expression to optimize so we just return POS
    method (string, start, end)
      block (return-from-scan)
        fluid-bind (*string* = string)
          fluid-bind (*start-pos* = start)
            fluid-bind (*end-pos* = end)
              fluid-bind (*end-string-pos* = *start-pos* - 1)
                fluid-bind (*repeat-counters* = *repeat-counters*)
                  fluid-bind (*last-pos-stores* = *last-pos-stores*)
                    fluid-bind (*reg-starts* = *reg-starts*)
                      fluid-bind (*regs-maybe-start* = *regs-maybe-start*)
                        fluid-bind (*reg-ends* = *reg-ends*)
                          let scan-start-pos :: <integer> = *start-pos*;
                          let starts-with-str
                              = if (start-string-test)
                                  str(starts-with);
                                else
                                  #f;
                                end if;
                          let max-end-pos = *end-pos* - min-len;
                          local method advance-fn (pos)
                                  pos;
                                end method advance-fn;
                          if (positive?(rep-num))
                            *repeat-counters*
                             := make(<array>, dimensions: rep-num);
                          end if;
                          if (positive?(zero-length-num))
                            *last-pos-stores*
                             := make(<array>, dimensions: zero-length-num);
                          end if;
                          if (positive?(reg-num))
                            begin
                              *reg-starts*
                               := make(<array>, dimensions: reg-num);
                              *regs-maybe-start*
                               := make(<array>, dimensions: reg-num);
                              *reg-ends*
                               := make(<array>, dimensions: reg-num);
                            end;
                          end if;
                          if (end-anchored-p)
                            let end-test-pos :: <integer>
                                = *end-pos* - end-string-len;
                            if (~ (*end-string-pos*
                                    := end-string-test(end-test-pos)))
                              if (1 = end-anchored-p
                                   & *end-pos* > scan-start-pos
                                   & '\n' = *string*[*end-pos* - 1])
                                *end-string-pos*
                                 := end-string-test(end-test-pos - 1);
                              end if;
                            end if;
                            if (~ (*end-string-pos*
                                    & *start-pos* <= *end-string-pos*))
                              return-from-scan(#f);
                            end if;
                            if (end-string-offset)
                              scan-start-pos
                               := max(scan-start-pos,
                                      *end-string-pos* - end-string-offset);
                            end if;
                          end if;
                          if (start-anchored-p)
                            if (*start-pos* ~= scan-start-pos
                                 | max-end-pos < *start-pos*)
                              return-from-scan(#f);
                            end if;
                            if (starts-with-str)
                              if (starts-with.case-insensitive-p
                                   & ~ *string*-equal(starts-with-str,
                                                      *start-pos*,
                                                      (*start-pos*
                                                        + starts-with-len),
                                                      0, starts-with-len))
                                return-from-scan(#f);
                              elseif (~ starts-with.case-insensitive-p
                                       & ~ *string*=(starts-with-str,
                                                     *start-pos*,
                                                     (*start-pos*
                                                       + starts-with-len),
                                                     0, starts-with-len))
                                return-from-scan(#f);
                              else
                                #f;
                              end if;
                            end if;
                            if (end-string-test & ~ end-anchored-p)
                              block (return-from-end-string-loop)
                                begin
                                  *end-string-pos* := *start-pos*;
                                  while (#t)
                                    if (~ (*end-string-pos*
                                            := end-string-test(*end-string-pos*)))
                                      return-from-scan(#f);
                                    end if;
                                    if (~ end-string-offset)
                                      return-from-end-string-loop(#f);
                                    end if;
                                    let maybe-start-pos
                                        = *end-string-pos*
                                           - end-string-offset;
                                    if (maybe-start-pos = *start-pos*)
                                      return-from-end-string-loop(#f);
                                    elseif (maybe-start-pos < *start-pos*
                                             & *end-string-pos*
                                                + end-string-len
                                                < *end-pos*)
                                      inc!(*end-string-pos*);
                                    else
                                      return-from-scan(#f);
                                    end if;
                                  end while;
                                end;
                              end block;
                            end if;
                            begin
                              let next-pos = match-fn(*start-pos*);
                              if (next-pos)
                                values(if (next-pos)
                                         *start-pos*;
                                       else
                                         #f;
                                       end if,
                                       next-pos, *reg-starts*, *reg-ends*);
                              end if;
                            end;
                          else
                            for (pos = if (starts-with-everything)
                                         scan-start-pos;
                                       else
                                         advance-fn(scan-start-pos);
                                       end if then advance-fn(pos),
                                 while pos & pos <= max-end-pos)
                              begin
                                let next-pos = match-fn(pos);
                                if (next-pos)
                                  return-from-scan(pos, next-pos,
                                                   *reg-starts*, *reg-ends*);
                                end if;
                                inc!(pos);
                              end;
                            end for;
                          end if;
                        end fluid-bind;
                      end fluid-bind;
                    end fluid-bind;
                  end fluid-bind;
                end fluid-bind;
              end fluid-bind;
            end fluid-bind;
          end fluid-bind;
        end fluid-bind;
      end block;
    end method;
  end if;
end method create-scanner-aux;

