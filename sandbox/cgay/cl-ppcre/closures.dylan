//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/closures.lisp,v 1.34 2007/01/15 23:57:41 edi Exp $
//  Here we create the closures which together build the final
//  scanner.
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

#f;

define method *string*= (string2, start1 :: <integer>, end1 :: <integer>,
                         start2 :: <integer>, end2 :: <integer>)
  // Like STRING=, i.e. compares the special string *STRING* from START1
  // to END1 with STRING2 from START2 to END2. Note that there's no
  // boundary check - this has to be implemented by the caller.
  block (return)
    for (string1-idx :: <integer> from start1 below end1,
         string2-idx :: <integer> from start2 below end2)
      if (~ (*string*[string1-idx] = string2[string2-idx]))
        return(#());
      end if;
    finally
      #t;
    end for;
  end block;
end method *string*=;

define method *string*-equal (string2, start1 :: <integer>, end1 :: <integer>,
                              start2 :: <integer>, end2 :: <integer>)
  // Like STRING-EQUAL, i.e. compares the special string *STRING* from
  // START1 to END1 with STRING2 from START2 to END2. Note that there's no
  // boundary check - this has to be implemented by the caller.
  block (return)
    for (string1-idx :: <integer> from start1 below end1,
         string2-idx :: <integer> from start2 below end2)
      if (~ char-equal?(*string*[string1-idx], string2[string2-idx]))
        return(#());
      end if;
    finally
      #t;
    end for;
  end block;
end method *string*-equal;

// Creates a closure which takes one parameter,
// START-POS, and tests whether REGEX can match *STRING* at START-POS
// such that the call to NEXT-FN after the match would succeed.
define generic create-matcher-aux (regex, next-fn) ;

define method create-matcher-aux (seq :: <seq>, next-fn)
  //  the closure for a SEQ is a chain of closures for the elements of
  //  this sequence which call each other in turn; the last closure
  //  calls NEXT-FN
  block (return)
    for (element in reverse(elements(seq)),
         curr-matcher = next-fn then next-matcher,
         next-matcher = create-matcher-aux(element,
                                           curr-matcher) then create-matcher-aux(element,
                                                                                 curr-matcher))
    finally
      return(next-matcher);
      #f;
    end for;
  end block;
end method create-matcher-aux;

define method create-matcher-aux (register :: <register>, next-fn)
  let num :: <integer> = num(register);
  let store-end-of-reg
      = method (start-pos :: <integer>)
          begin
            *reg-starts*[num] := *regs-maybe-start*[num];
            *reg-ends*[num] := start-pos;
          end;
          next-fn(start-pos);
        end method;
  let inner-matcher :: <function>
      = create-matcher-aux(register.regex, store-end-of-reg);
  //  here comes the actual closure for REGISTER
  method (start-pos :: <integer>)
    let old-*reg-starts* = *reg-starts*[num];
    let old-*regs-maybe-start* = *regs-maybe-start*[num];
    let old-*reg-ends* = *reg-ends*[num];
    //  we cannot use *REGS-START* here because Perl allows
    //  regular expressions like /(a|\1x)*/
    *regs-maybe-start*[num] := start-pos;
    let next-pos = inner-matcher(start-pos);
    if (~ next-pos)
      //  restore old values on failure
      begin
        *reg-starts*[num] := old-*reg-starts*;
        *regs-maybe-start*[num] := old-*regs-maybe-start*;
        *reg-ends*[num] := old-*reg-ends*;
      end;
    end if;
    next-pos;
  end method;
end method create-matcher-aux;

define method create-matcher-aux (lookahead :: <lookahead>, next-fn)
  let test-matcher :: <function>
      = create-matcher-aux(lookahead.regex, identity);
  if (lookahead.positivep)
    //  positive look-ahead: check success of inner regex, then call
    //  NEXT-FN
    method (start-pos)
      test-matcher(start-pos) & next-fn(start-pos);
    end method;
  else
    //  negative look-ahead: check failure of inner regex, then call
    //  NEXT-FN
    method (start-pos)
      ~ test-matcher(start-pos) & next-fn(start-pos);
    end method;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (lookbehind :: <lookbehind>, next-fn)
  let len :: <integer> = lookbehind.len;
  let test-matcher :: <function>
      = create-matcher-aux(lookbehind.regex, identity);
  if (lookbehind.positivep)
    //  positive look-behind: check success of inner regex (if we're
    //  far enough from the start of *STRING*), then call NEXT-FN
    method (start-pos :: <integer>)
      start-pos - (*real-start-pos* | *start-pos*) >= len
       & test-matcher(start-pos - len)
       & next-fn(start-pos);
    end method;
  else
    //  negative look-behind: check failure of inner regex (if we're
    //  far enough from the start of *STRING*), then call NEXT-FN
    method (start-pos :: <integer>)
      start-pos - (*real-start-pos* | *start-pos*) < len
       | ~ test-matcher((start-pos - len))
       & next-fn(start-pos);
    end method;
  end if;
end method create-matcher-aux;

// LTD: No macros.
#"insert-char-class-tester";

define method create-matcher-aux (str :: <str>, next-fn :: <function>)
  let len :: <integer> = str.len;
  let case-insensitive-p = str.case-insensitive-p;
  let start-of-end-string-p = start-of-end-string-p(str);
  let skip = skip(str);
  let str = str(str);
  let chr = str[0];
  let end-string = end-string & str(end-string);
  let end-string-len = if (end-string) size(end-string); else #f; end if;
  if (start-of-end-string-p & case-insensitive-p)
    //  closure for the first STR which belongs to the constant
    //  string at the end of the regular expression;
    //  case-insensitive version
    method (start-pos :: <integer>)
      let test-end-pos :: <integer> = start-pos + end-string-len;
      //  either we're at *END-STRING-POS* (which means that
      //  it has already been confirmed that end-string
      //  starts here) or we really have to test
      start-pos = *end-string-pos*
       | (test-end-pos <= *end-pos*
           & *string*-equal(end-string, start-pos, test-end-pos, 0,
                            end-string-len))
       & next-fn(start-pos + len);
    end method;
  elseif (start-of-end-string-p)
    //  closure for the first STR which belongs to the constant
    //  string at the end of the regular expression;
    //  case-sensitive version
    method (start-pos :: <integer>)
      let test-end-pos :: <integer> = start-pos + end-string-len;
      //  either we're at *END-STRING-POS* (which means that
      //  it has already been confirmed that end-string
      //  starts here) or we really have to test
      start-pos = *end-string-pos*
       | (test-end-pos <= *end-pos*
           & *string*=(end-string, start-pos, test-end-pos, 0, end-string-len))
       & next-fn(start-pos + len);
    end method;
  elseif (skip)
    //  a STR which can be skipped because some other function
    //  has already confirmed that it matches
    method (start-pos :: <integer>) next-fn(start-pos + len); end method;
  elseif (len = 1 & case-insensitive-p)
    //  STR represent exactly one character; case-insensitive
    //  version
    method (start-pos :: <integer>)
      start-pos < *end-pos* & char-equal?(*string*[start-pos], chr)
       & next-fn(start-pos + 1);
    end method;
  elseif (len = 1)
    //  STR represent exactly one character; case-sensitive
    //  version
    method (start-pos :: <integer>)
      start-pos < *end-pos* & *string*[start-pos] = chr
       & next-fn(start-pos + 1);
    end method;
  elseif (case-insensitive-p)
    //  general case, case-insensitive version
    method (start-pos :: <integer>)
      let next-pos :: <integer> = start-pos + len;
      next-pos <= *end-pos* & *string*-equal(str, start-pos, next-pos, 0, len)
       & next-fn(next-pos);
    end method;
  else
    //  general case, case-sensitive version
    method (start-pos :: <integer>)
      let next-pos :: <integer> = start-pos + len;
      next-pos <= *end-pos* & *string*=(str, start-pos, next-pos, 0, len)
       & next-fn(next-pos);
    end method;
  end if;
end method create-matcher-aux;

#f;

define method word-boundary-p (start-pos :: <integer>)
  // Check whether START-POS is a word-boundary within *STRING*.
  let 1-start-pos = start-pos - 1;
  fluid-bind (*start-pos* = (*real-start-pos* | *start-pos*))
    //  either the character before START-POS is a word-constituent and
    //  the character at START-POS isn't...
    start-pos = *end-pos*
     | (start-pos < *end-pos* & ~ word-char-p(*string*[start-pos]))
     & (1-start-pos < *end-pos* & *start-pos* <= 1-start-pos
         & word-char-p(*string*[1-start-pos]))
     | //  ...or vice versa
    (start-pos = *start-pos*
      | (1-start-pos < *end-pos* & *start-pos* <= 1-start-pos
          & ~ word-char-p(*string*[1-start-pos]))
      & (start-pos < *end-pos* & word-char-p(*string*[start-pos])));
  end fluid-bind;
end method word-boundary-p;

define method create-matcher-aux (word-boundary :: <word-boundary>,
                                  next-fn :: <function>)
  if (word-boundary.negatedp)
    method (start-pos)
      ~ word-boundary-p(start-pos) & next-fn(start-pos);
    end method;
  else
    method (start-pos)
      word-boundary-p(start-pos) & next-fn(start-pos);
    end method;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (everything :: <everything>,
                                  next-fn :: <function>)
  if (everything.single-line-p)
    //  closure for single-line-mode: we really match everything, so we
    //  just advance the index into *STRING* by one and carry on
    method (start-pos :: <integer>)
      start-pos < *end-pos* & next-fn(start-pos + 1);
    end method;
  else
    //  not single-line-mode, so we have to make sure we don't match
    //  #\Newline
    method (start-pos :: <integer>)
      start-pos < *end-pos* & /=(*string*[start-pos], '\n')
       & next-fn(start-pos + 1);
    end method;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (anchor :: <anchor>, next-fn :: <function>)
  let startp = anchor.startp;
  let multi-line-p = anchor.multi-line-p;
  if (anchor.no-newline-p)
    //  this must be an end-anchor and it must be modeless, so
    //  we just have to check whether START-POS equals
    //  *END-POS*
    method (start-pos :: <integer>)
      start-pos = *end-pos* & next-fn(start-pos);
    end method;
  elseif (startp & multi-line-p)
    //  a start-anchor in multi-line-mode: check if we're at
    //  *START-POS* or if the last character was #\Newline
    method (start-pos :: <integer>)
      fluid-bind (*start-pos* = (*real-start-pos* | *start-pos*))
        start-pos = *start-pos*
         | (start-pos <= *end-pos* & start-pos > *start-pos*
             & '\n' = *string*[start-pos - 1])
         & next-fn(start-pos);
      end fluid-bind;
    end method;
  elseif (startp)
    //  a start-anchor which is not in multi-line-mode, so just
    //  check whether we're at *START-POS*
    method (start-pos :: <integer>)
      start-pos = (*real-start-pos* | *start-pos*) & next-fn(start-pos);
    end method;
  elseif (multi-line-p)
    //  an end-anchor in multi-line-mode: check if we're at
    //  *END-POS* or if the character we're looking at is
    //  #\Newline
    method (start-pos :: <integer>)
      start-pos = *end-pos*
       | (start-pos < *end-pos* & '\n' = *string*[start-pos])
       & next-fn(start-pos);
    end method;
  else
    //  an end-anchor which is not in multi-line-mode, so just
    //  check if we're at *END-POS* or if we're looking at
    //  #\Newline and there's nothing behind it
    method (start-pos :: <integer>)
      start-pos = *end-pos*
       | (start-pos = *end-pos* - 1 & '\n' = *string*[start-pos])
       & next-fn(start-pos);
    end method;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (back-reference :: <back-reference>,
                                  next-fn :: <function>)
  let num = num(back-reference);
  if (back-reference.case-insensitive-p)
    //  the case-insensitive version
    method (start-pos :: <integer>)
      let reg-start = *reg-starts*[num];
      let reg-end = *reg-ends*[num];
      //  only bother to check if the corresponding REGISTER as
      //  matched successfully already
      reg-start
       & begin
           let next-pos :: <integer> = start-pos + (reg-end - reg-start);
           (next-pos <= *end-pos*
             & *string*-equal(*string*, start-pos, next-pos, reg-start,
                              reg-end)
             & next-fn(next-pos));
         end;
    end method;
  else
    //  the case-sensitive version
    method (start-pos :: <integer>)
      let reg-start = *reg-starts*[num];
      let reg-end = *reg-ends*[num];
      //  only bother to check if the corresponding REGISTER as
      //  matched successfully already
      reg-start
       & begin
           let next-pos :: <integer> = start-pos + (reg-end - reg-start);
           (next-pos <= *end-pos*
             & *string*=(*string*, start-pos, next-pos, reg-start, reg-end)
             & next-fn(next-pos));
         end;
    end method;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (branch :: <branch>, next-fn)
  let test = branch.test;
  let then-matcher :: <function>
      = create-matcher-aux(branch.then-regex, next-fn);
  let else-matcher :: <function>
      = create-matcher-aux(else-regex(branch), next-fn);
  if (instance?(test, <number>))
    method (start-pos)
      if (test < size(*reg-starts*) & *reg-starts*[test])
        then-matcher(start-pos);
      else
        else-matcher(start-pos);
      end if;
    end method;
  else
    begin
      let test-matcher :: <function> = create-matcher-aux(test, identity);
      method (start-pos)
        if (test-matcher(start-pos))
          then-matcher(start-pos);
        else
          else-matcher(start-pos);
        end if;
      end method;
    end;
  end if;
end method create-matcher-aux;

define method create-matcher-aux (standalone :: <standalone>, next-fn)
  let inner-matcher :: <function>
      = create-matcher-aux(standalone.regex, identity);
  method (start-pos)
    let next-pos = inner-matcher(start-pos);
    next-pos & next-fn(next-pos);
  end method;
end method create-matcher-aux;

define method create-matcher-aux (filter :: <filter>, next-fn)
  let fn = fn(filter);
  method (start-pos)
    let next-pos = fn(start-pos);
    next-pos & next-fn(next-pos);
  end method;
end method create-matcher-aux;

define method create-matcher-aux (void :: <void>, next-fn)
  //  optimize away VOIDs: don't create a closure, just return NEXT-FN
  next-fn;
end method create-matcher-aux;

