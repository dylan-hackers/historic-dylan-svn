//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/regex-class.lisp,v 1.32 2007/03/24 23:52:44 edi Exp $
//  This file defines the REGEX class and some utility methods for
//  this class. REGEX objects are used to represent the (transformed)
//  parse trees internally
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

//  Genera need the eval-when, here, or the types created by the class
//  definitions aren't seen by the typep calls later in the file.
begin
  define class <regex> (<object>); end class <regex>;
  define class <seq> (<regex>)
    // A list of REGEX objects.
    slot elements :: <pair>, init-keyword: #"elements";
  end class <seq>;
  define class <alternation> (<regex>)
    // A list of REGEX objects
    slot choices :: <pair>, init-keyword: #"choices";
  end class <alternation>;
  define class <lookahead> (<regex>)
    // The REGEX object we're checking.
    slot regex, init-keyword: #"regex";
    // Whether this assertion is positive.
    slot positivep, init-keyword: #"positivep";
  end class <lookahead>;
  define class <lookbehind> (<regex>)
    // The REGEX object we're checking.
    slot regex, init-keyword: #"regex";
    // Whether this assertion is positive.
    slot positivep, init-keyword: #"positivep";
    // The (fixed) length of the enclosed regex.
    slot len :: <integer>, init-keyword: #"len";
  end class <lookbehind>;
  define class <repetition> (<regex>)
    // The REGEX that's repeated.
    slot regex, init-keyword: #"regex";
    // Whether the repetition is greedy.
    slot greedyp, init-keyword: #"greedyp";
    // The minimal number of repetitions.
    slot minimum :: <integer>, init-keyword: #"minimum";
    // The maximal number of repetitions.
    // Can be NIL for unbounded.
    slot maximum, init-keyword: #"maximum";
    // The minimal length of the enclosed regex.
    slot min-len, init-keyword: #"min-len";
    // The length of the enclosed regex. NIL
    // if unknown.
    slot len, init-keyword: #"len";
    // The minimal number of characters which must
    // appear after this repetition.
    slot min-rest :: <integer> = 0;
    // If the regex contains a register.
    slot contains-register-p, init-keyword: #"contains-register-p";
  end class <repetition>;
  define class <register> (<regex>)
    // The inner regex.
    slot regex, init-keyword: #"regex";
    // The number of this register, starting from 0.
    // This is the index into *REGS-START* and *REGS-END*.
    slot num :: <integer>, init-keyword: #"num";
    // Name of this register or NIL.
    slot name, init-keyword: #"name";
  end class <register>;
  define class <standalone> (<regex>)
    // The inner regex.
    slot regex, init-keyword: #"regex";
  end class <standalone>;
  define class <back-reference> (<regex>)
    // The number of the register this
    // reference refers to.
    slot num :: <integer>, init-keyword: #"num";
    // The name of the register this
    // reference refers to or NIL.
    slot name, init-keyword: #"name";
    // Whether we check
    // case-insensitively.
    slot case-insensitive-p, init-keyword: #"case-insensitive-p";
  end class <back-reference>;
  define class <char-class> (<regex>)
    // A hash table the keys of which are the
    // characters; the values are always T.
    slot hash :: type-union(<table>, singleton(#f)), init-keyword: #"hash";
    // If the char class
    // case-insensitive.
    slot case-insensitive-p, init-keyword: #"case-insensitive-p";
    // Whether we mean the inverse of
    // the char class.
    slot invertedp, init-keyword: #"invertedp";
    // Whether this CHAR CLASS
    // represents the special class WORD-CHAR-CLASS.
    slot word-char-class-p, init-keyword: #"word-char-class-p";
  end class <char-class>;
  define class <str> (<regex>)
    // The actual string.
    slot str :: <string>, init-keyword: #"str";
    // The length of the string.
    slot len :: <integer> = 0;
    // If we match case-insensitively.
    slot case-insensitive-p, init-keyword: #"case-insensitive-p";
    // Offset from the left of the whole
    // parse tree. The first regex has offset 0. NIL if unknown, i.e. behind
    // a variable-length regex.
    slot offset = #f;
    // If we can avoid testing for this
    // string because the SCAN function has done this already.
    slot skip = #f, init-keyword: #"skip";
    // If this is the unique
    // STR which starts END-STRING (a slot of MATCHER).
    slot start-of-end-string-p = #f;
  end class <str>;
  define class <anchor> (<regex>)
    // Whether this is a "start anchor".
    slot startp, init-keyword: #"startp";
    // Whether we're in multi-line mode,
    // i.e. whether each #\Newline is surrounded by anchors.
    slot multi-line-p, init-keyword: #"multi-line-p";
    // Whether we ignore #\Newline at the end.
    slot no-newline-p, init-keyword: #"no-newline-p";
  end class <anchor>;
  define class <everything> (<regex>)
    // Whether we're in single-line mode,
    // i.e. whether we also match #\Newline.
    slot single-line-p, init-keyword: #"single-line-p";
  end class <everything>;
  define class <word-boundary> (<regex>)
    // Whether we mean the opposite,
    // i.e. no word-boundary.
    slot negatedp, init-keyword: #"negatedp";
  end class <word-boundary>;
  define class <branch> (<regex>)
    // The test of this branch, one of
    // LOOKAHEAD, LOOKBEHIND, or a number.
    slot test, init-keyword: #"test";
    // The regex that's to be matched if the
    // test succeeds.
    slot then-regex, init-keyword: #"then-regex";
    // The regex that's to be matched if the
    // test fails.
    slot else-regex = make(<void>), init-keyword: #"else-regex";
  end class <branch>;
  define class <filter> (<regex>)
    // The user-defined function.
    slot fn :: type-union(<function>, <symbol>), init-keyword: #"fn";
    // The fixed length of this filter or NIL.
    slot len, init-keyword: #"len";
  end class <filter>;
  define class <void> (<regex>); end class <void>;
end;

// LTD: Defmethod qualifier INITIALIZE-INSTANCE ignored.
define method initialize (declare, optimize :: <speed>)
  // Make large hash tables smaller, if possible.
  let hash = get-property!(init-args, #"hash");
  if (hash & *regex-char-code-limit* > 256
       & size(hash) > *regex-char-code-limit* / 2)
    begin
      char-class.hash := merge-inverted-hash(make(<table>), hash);
      char-class.invertedp := ~ char-class.invertedp;
    end;
  end if;
end method initialize;

// LTD: Defmethod qualifier INITIALIZE-INSTANCE ignored.
define method initialize (declare, optimize :: <speed>)
  // Automatically computes the length of a STR after initialization.
  let str-slot = str.str;
  if (~ instance?(str-slot, <simple-string>))
    str.str := as(<simple-string>, str-slot);
  end if;
  str.len := size(str(str));
end method initialize;

//  The following four methods allow a VOID object to behave like a
//  zero-length STR object (only readers needed)
define method len (void :: <void>) 0; end method len;

define method str (void :: <void>) ""; end method str;

define method skip (void :: <void>) #f; end method skip;

define method start-of-end-string-p (void :: <void>)
  #f;
end method start-of-end-string-p;

// Utility function used by the optimizer (see GATHER-STRINGS).
// Returns a keyword denoting the case-(in)sensitivity of a STR or its
// second argument if the STR has length 0. Returns NIL for REGEX objects
// which are not of type STR.
define generic case-mode (regex, old-case-mode) ;

define method case-mode (str :: <str>, old-case-mode)
  if (zero?(str.len))
    old-case-mode;
  elseif (str.case-insensitive-p)
    #"case-insensitive";
  else
    #"case-sensitive";
  end if;
end method case-mode;

define method case-mode (regex :: <regex>, old-case-mode)
  #f;
end method case-mode;

// Implements a deep copy of a REGEX object.
define generic copy-regex (regex) ;

define method copy-regex (anchor :: <anchor>)
  make(<anchor>, startp: anchor.startp, multi-line-p: anchor.multi-line-p,
       no-newline-p: anchor.no-newline-p);
end method copy-regex;

define method copy-regex (everything :: <everything>)
  make(<everything>, single-line-p: everything.single-line-p);
end method copy-regex;

define method copy-regex (word-boundary :: <word-boundary>)
  make(<word-boundary>, negatedp: word-boundary.negatedp);
end method copy-regex;

define method copy-regex (void :: <void>) make(<void>); end method copy-regex;

define method copy-regex (lookahead :: <lookahead>)
  make(<lookahead>, regex: copy-regex(lookahead.regex),
       positivep: lookahead.positivep);
end method copy-regex;

define method copy-regex (seq :: <seq>)
  make(<seq>, elements: map(copy-regex, elements(seq)));
end method copy-regex;

define method copy-regex (alternation :: <alternation>)
  make(<alternation>, choices: map(copy-regex, choices(alternation)));
end method copy-regex;

define method copy-regex (branch :: <branch>)
  make(<branch>,
       test: if (instance?(branch.test, <regex>))
               copy-regex(branch.test);
             else
               branch.test;
             end if,
       then-regex: copy-regex(branch.then-regex),
       else-regex: copy-regex(else-regex(branch)));
end method copy-regex;

define method copy-regex (lookbehind :: <lookbehind>)
  make(<lookbehind>, regex: copy-regex(lookbehind.regex),
       positivep: lookbehind.positivep, len: lookbehind.len);
end method copy-regex;

define method copy-regex (repetition :: <repetition>)
  make(<repetition>, regex: copy-regex(repetition.regex),
       greedyp: repetition.greedyp, minimum: minimum(repetition),
       maximum: repetition.maximum, min-len: repetition.min-len,
       len: repetition.len,
       contains-register-p: repetition.contains-register-p);
end method copy-regex;

define method copy-regex (register :: <register>)
  make(<register>, regex: copy-regex(register.regex), num: num(register),
       name: register.name);
end method copy-regex;

define method copy-regex (standalone :: <standalone>)
  make(<standalone>, regex: copy-regex(standalone.regex));
end method copy-regex;

define method copy-regex (back-reference :: <back-reference>)
  make(<back-reference>, num: num(back-reference),
       case-insensitive-p: back-reference.case-insensitive-p);
end method copy-regex;

define method copy-regex (char-class :: <char-class>)
  make(<char-class>, hash: hash(char-class),
       case-insensitive-p: char-class.case-insensitive-p,
       invertedp: char-class.invertedp,
       word-char-class-p: char-class.word-char-class-p);
end method copy-regex;

define method copy-regex (str :: <str>)
  make(<str>, str: str(str), case-insensitive-p: str.case-insensitive-p);
end method copy-regex;

define method copy-regex (filter :: <filter>)
  make(<filter>, fn: fn(filter), len: filter.len);
end method copy-regex;

//  Note that COPY-REGEX and REMOVE-REGISTERS could have easily been
//  wrapped into one function. Maybe in the next release...
//  Further note that this function is used by CONVERT to factor out
//  complicated repetitions, i.e. cases like
//    (a)* -> (?:a*(a))?
//  This won't work for, say,
//    ((a)|(b))* -> (?:(?:a|b)*((a)|(b)))?
//  and therefore we stop REGISTER removal once we see an ALTERNATION.
// Returns a deep copy of a REGEX (see COPY-REGEX) and
// optionally removes embedded REGISTER objects if possible and if the
// special variable REMOVE-REGISTERS-P is true.
define generic remove-registers (regex) ;

define method remove-registers (register :: <register>)
  if (remove-registers-p)
    remove-registers(register.regex);
  else
    //  mark REG-SEEN as true so enclosing REPETITION objects
    //  (see method below) know if they contain a register or not
    reg-seen := #t;
    copy-regex(register);
  end if;
end method remove-registers;

define method remove-registers (repetition :: <repetition>)
  let reg-seen = #f;
  let inner-regex = remove-registers(repetition.regex);
  make(<repetition>, regex: inner-regex, greedyp: repetition.greedyp,
       minimum: minimum(repetition), maximum: repetition.maximum,
       min-len: repetition.min-len, len: repetition.len,
       contains-register-p: reg-seen);
end method remove-registers;

define method remove-registers (standalone :: <standalone>)
  make(<standalone>, regex: remove-registers(standalone.regex));
end method remove-registers;

define method remove-registers (lookahead :: <lookahead>)
  make(<lookahead>, regex: remove-registers(lookahead.regex),
       positivep: lookahead.positivep);
end method remove-registers;

define method remove-registers (lookbehind :: <lookbehind>)
  make(<lookbehind>, regex: remove-registers(lookbehind.regex),
       positivep: lookbehind.positivep, len: lookbehind.len);
end method remove-registers;

define method remove-registers (branch :: <branch>)
  make(<branch>,
       test: if (instance?(branch.test, <regex>))
               remove-registers(branch.test);
             else
               branch.test;
             end if,
       then-regex: remove-registers(branch.then-regex),
       else-regex: remove-registers(else-regex(branch)));
end method remove-registers;

define method remove-registers (alternation :: <alternation>)
  //  an ALTERNATION, so we can't remove REGISTER objects further down
  remove-registers-p := #f;
  copy-regex(alternation);
end method remove-registers;

define method remove-registers (regex :: <regex>)
  copy-regex(regex);
end method remove-registers;

define method remove-registers (seq :: <seq>)
  make(<seq>, elements: map(remove-registers, elements(seq)));
end method remove-registers;

// Returns an EVERYTHING object if REGEX is equivalent
// to this object, otherwise NIL. So, "(.){1}" would return true
// (i.e. the object corresponding to ".", for example.
define generic everythingp (regex) ;

define method everythingp (seq :: <seq>)
  let cleaned-elements
      = choose(complement(method (element)
                            instance?(element, <void>);
                          end method),
               elements(seq));
  1 = size(cleaned-elements) & everythingp(first(cleaned-elements));
end method everythingp;

define method everythingp (alternation :: <alternation>)
  1 = size(alternation.choices)
   & //  this is unlikely to happen for human-generated regexes,
     //  but machine-generated ones might look like this
  everythingp(first(choices));
end method everythingp;

define method everythingp (repetition :: <repetition>)
  repetition.maximum
   & begin
       let g2172 = repetition.minimum;
       (1 = g2172 & g2172 = repetition.maximum);
     end
   & //  treat "<regex>{1,1}" like "<regex>"
  everythingp(regex);
end method everythingp;

define method everythingp (register :: <register>)
  everythingp(register.regex);
end method everythingp;

define method everythingp (standalone :: <standalone>)
  everythingp(standalone.regex);
end method everythingp;

define method everythingp (everything :: <everything>)
  everything;
end method everythingp;

define method everythingp (regex :: <regex>)
  //  the general case for ANCHOR, BACK-REFERENCE, BRANCH, CHAR-CLASS,
  //  LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY
  #f;
end method everythingp;

// Return the length of REGEX if it is fixed, NIL otherwise.
define generic regex-length (regex) ;

define method regex-length (seq :: <seq>)
  let _acc = 0;
  block (return)
    for (sub-regex in elements(seq),
         len = regex-length(sub-regex) then regex-length(sub-regex))
      if (~ len) return(#f); end if;
      inc!(_acc, len);
    finally
      _acc;
    end for;
  end block;
end method regex-length;

define method regex-length (alternation :: <alternation>)
  //  only return a true value if all inner lengths are non-NIL and
  //  mutually equal
  block (return)
    for (sub-regex in choices(alternation), old-len = %f then len,
         len = regex-length(sub-regex) then regex-length(sub-regex))
      if (~ len | (old-len & len ~= old-len)) return(#f); end if;
    finally
      return(len);
      #f;
    end for;
  end block;
end method regex-length;

define method regex-length (branch :: <branch>)
  let then-length = regex-length(branch.then-regex);
  then-length & then-length == regex-length(else-regex(branch)) & then-length;
end method regex-length;

define method regex-length (repetition :: <repetition>)
  //  we can only compute the length of a REPETITION object if the
  //  number of repetitions is fixed; note that we don't call
  //  REGEX-LENGTH for the inner regex, we assume that the LEN slot is
  //  always set correctly
  if (repetition.len & repetition.minimum == repetition.maximum)
    repetition.minimum * repetition.len;
  else
    #f;
  end if;
end method regex-length;

define method regex-length (register :: <register>)
  regex-length(register.regex);
end method regex-length;

define method regex-length (standalone :: <standalone>)
  regex-length(standalone.regex);
end method regex-length;

define method regex-length (back-reference :: <back-reference>)
  //  with enough effort we could possibly do better here, but
  //  currently we just give up and return NIL
  #f;
end method regex-length;

define method regex-length (char-class :: <char-class>)
  1;
end method regex-length;

define method regex-length (everything :: <everything>)
  1;
end method regex-length;

define method regex-length (str :: <str>) str.len; end method regex-length;

define method regex-length (filter :: <filter>)
  filter.len;
end method regex-length;

define method regex-length (regex :: <regex>)
  //  the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  //  WORD-BOUNDARY (which all have zero-length)
  0;
end method regex-length;

// Returns the minimal length of REGEX.
define generic regex-min-length (regex) ;

define method regex-min-length (seq :: <seq>)
  let _acc = 0;
  for (sub-regex in elements(seq),
       len = regex-min-length(sub-regex) then regex-min-length(sub-regex))
    inc!(_acc, len);
  finally
    _acc;
  end for;
end method regex-min-length;

define method regex-min-length (alternation :: <alternation>)
  let _acc = #f;
  for (sub-regex in choices(alternation),
       len = regex-min-length(sub-regex) then regex-min-length(sub-regex))
    _acc := if (_acc) min(_acc, len); else len; end if;
  finally
    _acc;
  end for;
end method regex-min-length;

define method regex-min-length (branch :: <branch>)
  //  minimal length of both alternations
  min(regex-min-length(branch.then-regex),
      regex-min-length(else-regex(branch)));
end method regex-min-length;

define method regex-min-length (repetition :: <repetition>)
  //  obviously the product of the inner minimal length and the minimal
  //  number of repetitions
  minimum(repetition) * repetition.min-len;
end method regex-min-length;

define method regex-min-length (register :: <register>)
  regex-min-length(register.regex);
end method regex-min-length;

define method regex-min-length (standalone :: <standalone>)
  regex-min-length(standalone.regex);
end method regex-min-length;

define method regex-min-length (char-class :: <char-class>)
  1;
end method regex-min-length;

define method regex-min-length (everything :: <everything>)
  1;
end method regex-min-length;

define method regex-min-length (str :: <str>)
  str.len;
end method regex-min-length;

define method regex-min-length (filter :: <filter>)
  filter.len | 0;
end method regex-min-length;

define method regex-min-length (regex :: <regex>)
  //  the general case for ANCHOR, BACK-REFERENCE, LOOKAHEAD,
  //  LOOKBEHIND, VOID, and WORD-BOUNDARY
  0;
end method regex-min-length;

// Returns the offset the following regex would have
// relative to START-POS or NIL if we can't compute it. Sets the OFFSET
// slot of REGEX to START-POS if REGEX is a STR. May also affect OFFSET
// slots of STR objects further down the tree.
define generic compute-offsets (regex, start-pos) ;

//  note that we're actually only interested in the offset of
//  "top-level" STR objects (see ADVANCE-FN in the SCAN function) so we
//  can stop at variable-length alternations and don't need to descend
//  into repetitions
define method compute-offsets (seq :: <seq>, start-pos)
  block (return)
    for (element in elements(seq), pos = start-pos then curr-offset,
         curr-offset = compute-offsets(element,
                                       pos) then compute-offsets(element,
                                                                 pos),
         while curr-offset)
    finally
      return(curr-offset);
      #f;
    end for;
  end block;
end method compute-offsets;

define method compute-offsets (alternation :: <alternation>, start-pos)
  block (return)
    for (choice in choices(alternation), old-offset = %f then curr-offset,
         curr-offset = compute-offsets(choice,
                                       start-pos) then compute-offsets(choice,
                                                                       start-pos))
      if (~ curr-offset | (old-offset & curr-offset ~= old-offset))
        return(#f);
      end if;
    finally
      return(curr-offset);
      #f;
    end for;
  end block;
end method compute-offsets;

define method compute-offsets (branch :: <branch>, start-pos)
  let then-offset = compute-offsets(branch.then-regex, start-pos);
  then-offset & then-offset == compute-offsets(else-regex(branch), start-pos)
   & then-offset;
end method compute-offsets;

define method compute-offsets (repetition :: <repetition>, start-pos)
  //  no need to descend into the inner regex
  if (repetition.len & repetition.minimum == repetition.maximum)
    //  fixed number of repetitions, so we know how to proceed
    start-pos + minimum * len;
  else
    //  otherwise return NIL
    #f;
  end if;
end method compute-offsets;

define method compute-offsets (register :: <register>, start-pos)
  compute-offsets(register.regex, start-pos);
end method compute-offsets;

define method compute-offsets (standalone :: <standalone>, start-pos)
  compute-offsets(standalone.regex, start-pos);
end method compute-offsets;

define method compute-offsets (char-class :: <char-class>, start-pos)
  start-pos + 1;
end method compute-offsets;

define method compute-offsets (everything :: <everything>, start-pos)
  start-pos + 1;
end method compute-offsets;

define method compute-offsets (str :: <str>, start-pos)
  offset(str) := start-pos;
  start-pos + str.len;
end method compute-offsets;

define method compute-offsets (back-reference :: <back-reference>, start-pos)
  #f;
end method compute-offsets;

define method compute-offsets (filter :: <filter>, start-pos)
  let len = filter.len;
  if (len) start-pos + len; else #f; end if;
end method compute-offsets;

define method compute-offsets (regex :: <regex>, start-pos)
  //  the general case for ANCHOR, LOOKAHEAD, LOOKBEHIND, VOID, and
  //  WORD-BOUNDARY (which all have zero-length)
  start-pos;
end method compute-offsets;

