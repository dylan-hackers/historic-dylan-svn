//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/optimize.lisp,v 1.30 2007/01/01 23:43:10 edi Exp $
//  This file contains optimizations which can be applied to converted
//  parse trees.
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

// Merges adjacent sequences and alternations, i.e. it
// transforms #<SEQ #<STR "a"> #<SEQ #<STR "b"> #<STR "c">>> to
// #<SEQ #<STR "a"> #<STR "b"> #<STR "c">>. This is a destructive
// operation on REGEX.
define generic flatten (regex) ;

define method flatten (seq :: <seq>)
  let elements-rest = elements(seq);
  block (return)
    while (#t)
      if (~ elements-rest) return(#f); end if;
      let flattened-element = flatten(head(elements-rest));
      let next-elements-rest = tail(elements-rest);
      if (instance?(flattened-element, <seq>))
        //  FLATTENED-ELEMENT is a SEQ object, so we "splice"
        //  it into out list of elements
        begin
          let flattened-element-elements = elements(flattened-element);
          begin
            head(elements-rest) := head(flattened-element-elements);
            tail(elements-rest)
             := concatenate!(tail(flattened-element-elements),
                             tail(elements-rest));
          end;
        end;
      else
        //  otherwise we just replace the current element with
        //  its flattened counterpart
        head(elements-rest) := flattened-element;
      end if;
      elements-rest := next-elements-rest;
    end while;
  end block;
  let elements = elements(seq);
  if (second(elements))
    seq;
  elseif (tail(elements))
    first(elements);
  else
    make(<void>);
  end if;
end method flatten;

define method flatten (alternation :: <alternation>)
  let choices-rest = choices(alternation);
  block (return)
    while (#t)
      if (~ choices-rest) return(#f); end if;
      let flattened-choice = flatten(head(choices-rest));
      let next-choices-rest = tail(choices-rest);
      if (instance?(flattened-choice, <alternation>))
        begin
          let flattened-choice-choices = choices(flattened-choice);
          begin
            head(choices-rest) := head(flattened-choice-choices);
            tail(choices-rest)
             := concatenate!(tail(flattened-choice-choices),
                             tail(choices-rest));
          end;
        end;
      else
        head(choices-rest) := flattened-choice;
      end if;
      choices-rest := next-choices-rest;
    end while;
  end block;
  let choices = choices(alternation);
  if (second(choices))
    alternation;
  elseif (tail(choices))
    first(choices);
  else
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: #f,
          format-control: "Encountered alternation without choices.",
          format-arguments: list());
  end if;
end method flatten;

define method flatten (branch :: <branch>)
  begin
    begin
      branch.test
       := if (instance?(branch.test, <number>))
            branch.test;
          else
            flatten(branch.test);
          end if;
      branch.then-regex := flatten(branch.then-regex);
      branch.else-regex := flatten(branch.else-regex);
    end;
    branch;
  end;
end method flatten;

define method flatten (regex :: <regex>)
  select (regex by instance?)
    repetition | register | lookahead | lookbehind | standalone
       => //  if REGEX contains exactly one inner REGEX object flatten it
           regex.regex := flatten(regex.regex);
           regex;
    #t
       => //  otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
          //  LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
          //  do nothing
          /regex;
  end select;
end method flatten;

// Collects adjacent strings or characters into one
// string provided they have the same case mode. This is a destructive
// operation on REGEX.
define generic gather-strings (regex) ;

define method gather-strings (seq :: <seq>)
  let start-point = pair(#f, elements(seq));
  let curr-point = start-point;
  let old-case-mode = #f;
  let collector = #f;
  let collector-start = #f;
  let collector-length :: <integer> = 0;
  let skip = #f;
  block (return)
    while (#t)
      let elements-rest = tail(curr-point);
      if (~ elements-rest) return(#f); end if;
      let element = head(elements-rest);
      let case-mode = case-mode(element, old-case-mode);
      if (case-mode & case-mode == old-case-mode)
        //  if ELEMENT is a STR and we have collected a STR of
        //  the same case mode in the last iteration we
        //  concatenate ELEMENT onto COLLECTOR and remember the
        //  value of its SKIP slot
        begin
          let old-collector-length = collector-length;
          if (~ (instance?(collector, <stretchy-vector>)
                  & array-has-fill-pointer-p(collector)))
            begin
              collector
               := begin
                    let _vector = make(<array>, dimensions: collector-length);
                    (size(_vector) := #t);
                    _vector;
                  end;
              collector-start := #f;
            end;
          end if;
          // LTD: Function ADJUST-ARRAY not yet implemented.
          adjust-array(collector, inc!(collector-length, element.len),
                       fill-pointer: #t);
          begin
            copy-sequence(collector, old-collector-length) := str(element);
            //  it suffices to remember the last SKIP slot
            //  because due to the way MAYBE-ACCUMULATE
            //  works adjacent STR objects have the same
            //  SKIP value
            skip
             := skip(element);
          end;
        end;
        tail(curr-point) := tail(elements-rest);
      else
        begin
          let collected-string
              = if (collector-start)
                  collector-start;
                elseif (collector)
                  //  if we have collected something already
                  //  we convert it into a STR
                  make(<str>, skip: skip, str: collector,
                       case-insensitive-p: old-case-mode
                                            == #"case-insensitive");
                else
                  #f;
                end if;
          if (case-mode)
            //  if ELEMENT is a string with a different case
            //  mode than the last one we have either just
            //  converted COLLECTOR into a STR or COLLECTOR
            //  is still empty; in both cases we can now
            //  begin to fill it anew
            begin
              collector := str(element);
              collector-start := element;
              //  and we remember the SKIP value as above
              skip
               := skip(element);
              collector-length := element.len;
            end;
            if (collected-string)
              begin
                head(elements-rest) := collected-string;
                curr-point := tail(curr-point);
              end;
            else
              tail(curr-point) := tail(elements-rest);
            end if;
          else
            //  otherwise this is not a STR so we apply
            //  GATHER-STRINGS to it and collect it directly
            //  into RESULT
            if (collected-string)
              begin
                head(elements-rest) := collected-string;
                curr-point := tail(curr-point);
                tail(curr-point)
                 := pair(gather-strings(element), tail(curr-point));
                curr-point := tail(curr-point);
              end;
            else
              begin
                head(elements-rest) := gather-strings(element);
                curr-point := tail(curr-point);
              end;
            end if;
            //  we also have to empty COLLECTOR here in case
            //  it was still filled from the last iteration
            begin collector := #f; collector-start := #f; end;
          end if;
        end;
      end if;
      old-case-mode := case-mode;
    end while;
  end block;
  if (collector)
    tail(curr-point)
     := pair(make(<str>, skip: skip, str: collector,
                  case-insensitive-p: old-case-mode == #"case-insensitive"),
             #f);
  end if;
  elements(seq) := tail(start-point);
  seq;
end method gather-strings;

define method gather-strings (alternation :: <alternation>)
  //  loop ON the choices of ALTERNATION so we can modify them directly
  for (choices-rest = choices(alternation) then tail(choices-rest),
       until empty?(choices-rest), while choices-rest)
    head(choices-rest) := gather-strings(head(choices-rest));
  end for;
  alternation;
end method gather-strings;

define method gather-strings (branch :: <branch>)
  begin
    begin
      branch.test
       := if (instance?(branch.test, <number>))
            branch.test;
          else
            gather-strings(branch.test);
          end if;
      branch.then-regex := gather-strings(branch.then-regex);
      branch.else-regex := gather-strings(branch.else-regex);
    end;
    branch;
  end;
end method gather-strings;

define method gather-strings (regex :: <regex>)
  select (regex by instance?)
    repetition | register | lookahead | lookbehind | standalone
       => //  if REGEX contains exactly one inner REGEX object apply
          //  GATHER-STRINGS to it
          /regex.regex := gather-strings(regex.regex);
          /regex;
    #t
       => //  otherwise (ANCHOR, BACK-REFERENCE, CHAR-CLASS, EVERYTHING,
          //  LOOKAHEAD, LOOKBEHIND, STR, VOID, FILTER, and WORD-BOUNDARY)
          //  do nothing
          /regex;
  end select;
end method gather-strings;

//  Note that START-ANCHORED-P will be called after FLATTEN and GATHER-STRINGS.
// Returns T if REGEX starts with a "real" start
// anchor, i.e. one that's not in multi-line mode, NIL otherwise. If
// IN-SEQ-P is true the function will return :ZERO-LENGTH if REGEX is a
// zero-length assertion.
define generic start-anchored-p (regex, #key in-seq-p) ;

define method start-anchored-p (seq :: <seq>, #key in-seq-p)
  //  note that START-ANCHORED-P is to be applied after FLATTEN and
  //  GATHER-STRINGS, i.e. SEQ cannot be empty and cannot contain
  //  embedded SEQ objects
  block (return)
    for (element in elements(seq),
         anchored-p = start-anchored-p(element,
                                       #t) then start-anchored-p(element, #t),
         while anchored-p == #"zero-length")
    finally
      return(anchored-p & ~ (anchored-p == #"zero-length"));
      #f;
    end for;
  end block;
end method start-anchored-p;

define method start-anchored-p (alternation :: <alternation>, #key in-seq-p)
  //  clearly an alternation can only be start-anchored if all of its
  //  choices are start-anchored
  block (return)
    for (choice in choices(alternation))
      if (~ start-anchored-p(choice)) return(#()); end if;
    finally
      #t;
    end for;
  end block;
end method start-anchored-p;

define method start-anchored-p (branch :: <branch>, #key in-seq-p)
  start-anchored-p(branch.then-regex) & start-anchored-p(else-regex(branch));
end method start-anchored-p;

define method start-anchored-p (repetition :: <repetition>, #key in-seq-p)
  //  well, this wouldn't make much sense, but anyway...
  positive?(minimum(repetition)) & start-anchored-p(repetition.regex);
end method start-anchored-p;

define method start-anchored-p (register :: <register>, #key in-seq-p)
  start-anchored-p(register.regex);
end method start-anchored-p;

define method start-anchored-p (standalone :: <standalone>, #key in-seq-p)
  start-anchored-p(standalone.regex);
end method start-anchored-p;

define method start-anchored-p (anchor :: <anchor>, #key in-seq-p)
  anchor.startp & ~ anchor.multi-line-p;
end method start-anchored-p;

define method start-anchored-p (regex :: <regex>, #key in-seq-p)
  select (regex by instance?)
    lookahead | lookbehind | word-boundary | void
       => //  zero-length assertions
           if (in-seq-p) #"zero-length"; else #f; end if;
    filter
       => if (in-seq-p & regex.len & zero?(regex.len))
            #"zero-length";
          else
            #f;
          end if;
    #t
       => //  BACK-REFERENCE, CHAR-CLASS, EVERYTHING, and STR
           #f;
  end select;
end method start-anchored-p;

//  Note that END-STRING-AUX will be called after FLATTEN and GATHER-STRINGS.
// Returns the constant string (if it exists) REGEX
// ends with wrapped into a STR object, otherwise NIL.
// OLD-CASE-INSENSITIVE-P is the CASE-INSENSITIVE-P slot of the last STR
// collected or :VOID if no STR has been collected yet. (This is a helper
// function called by END-STRIN.)
define generic end-string-aux (regex, #key old-case-insensitive-p) ;

define method end-string-aux (str :: <str>,
                              #key old-case-insensitive-p = #"void")
  if (~ skip(str)
       & //  avoid constituents of STARTS-WITH
         //  only use STR if nothing has been collected yet or if
         //  the collected string has the same value for
         //  CASE-INSENSITIVE-P
      (old-case-insensitive-p == #"void"
        | str.case-insensitive-p == old-case-insensitive-p))
    begin
      last-str := str;
      //  set the SKIP property of this STR
      skip(str)
       := #t;
    end;
    str;
  else
    #f;
  end if;
end method end-string-aux;

define method end-string-aux (seq :: <seq>,
                              #key old-case-insensitive-p = #"void")
  let case-insensitive-p = #f;
  let concatenated-string = #f;
  let concatenated-start = #f;
  let concatenated-length :: <integer> = 0;
  let element = #f;
  let loop-list-2155 :: <list> = reverse(elements(seq));
  let loop-old-case-insensitive-p = #f;
  let element-end = #f;
  let skip = #f;
  local method go-end-loop () #f; end method go-end-loop,
        method go-next-loop ()
          element-end := end-string-aux(element, loop-old-case-insensitive-p);
          skip := if (element-end) zero?(element-end.len); else #f; end if;
          if (~ element-end) continuep := #f; end if;
          if (~ element-end) go-end-loop(); end if;
          if (~ skip)
            if (concatenated-string)
              if (concatenated-start)
                begin
                  concatenated-string
                   := begin
                        let _vector
                            = make(<array>, dimensions: concatenated-length);
                        (size(_vector) := #t);
                        _vector;
                      end;
                  concatenated-start := #f;
                end;
              end if;
              begin
                let len :: <integer> = element-end.len;
                let str = str(element-end);
                inc!(concatenated-length, len);
                for (i :: <integer> from len - 1 to 0 by -1)
                  add!(concatenated-string, str[i]);
                end for;
              end;
            else
              begin
                concatenated-string := #t;
                concatenated-start := element-end;
                concatenated-length := element-end.len;
                case-insensitive-p := element-end.case-insensitive-p;
              end;
            end if;
          end if;
          if (~ continuep) go-end-loop(); end if;
          if (not(pair?(loop-list-2155))) go-end-loop(); end if;
          element := head(loop-list-2155);
          loop-list-2155 := tail(loop-list-2155);
          loop-old-case-insensitive-p
           := if (skip)
                loop-old-case-insensitive-p;
              else
                element-end.case-insensitive-p;
              end if;
          go-next-loop();
          go-end-loop();
        end method go-next-loop;
  if (not(pair?(loop-list-2155))) go-end-loop(); end if;
  element := head(loop-list-2155);
  loop-list-2155 := tail(loop-list-2155);
  loop-old-case-insensitive-p := old-case-insensitive-p;
  go-next-loop();
  if (zero?(concatenated-length))
    //  don't bother to return zero-length strings
    #f;
  elseif (concatenated-start)
    concatenated-start;
  else
    make(<str>, str: reverse!(concatenated-string),
         case-insensitive-p: case-insensitive-p);
  end if;
end method end-string-aux;

define method end-string-aux (register :: <register>,
                              #key old-case-insensitive-p = #"void")
  end-string-aux(register.regex, old-case-insensitive-p);
end method end-string-aux;

define method end-string-aux (standalone :: <standalone>,
                              #key old-case-insensitive-p = #"void")
  end-string-aux(standalone.regex, old-case-insensitive-p);
end method end-string-aux;

define method end-string-aux (regex :: <regex>,
                              #key old-case-insensitive-p = #"void")
  select (regex by instance?)
    anchor | lookahead | lookbehind | word-boundary | void
       => //  a zero-length REGEX object - for the sake of END-STRING-AUX
          //  this is a zero-length string
          /if (instance?(regex, <anchor>) & ~ regex.startp
          /     & (regex.no-newline-p | ~ regex.multi-line-p)
          /     & old-case-insensitive-p == #"void")
          /  //  if this is a "real" end-anchor and we haven't collected
          /  //  anything so far we can set END-ANCHORED-P (where 1 or 0
          /  //  indicate whether we accept a #\Newline at the end or not)
          /  end-anchored-p := if (regex.no-newline-p) 0; else 1; end if;
          /end if;
          /make(<str>, str: "", case-insensitive-p: #"void");
    #t
       => //  (ALTERNATION, BACK-REFERENCE, BRANCH, CHAR-CLASS, EVERYTHING,
          //  REPETITION, FILTER)
          /#f;
  end select;
end method end-string-aux;

define method end-string (regex)
  // Returns the constant string (if it exists) REGEX ends with wrapped
  // into a STR object, otherwise NIL.
  let continuep = #t;
  let last-str = #f;
  let _ = end-string-aux(regex);
  if (last-str)
    //  if we've found something set the START-OF-END-STRING-P of
    //  the leftmost STR collected accordingly and remember the
    //  OFFSET of this STR (in a special variable provided by the
    //  caller of this function)
    begin
      start-of-end-string-p(last-str) := #t;
      end-string-offset := offset(last-str);
    end;
  end if;
  _;
end method end-string;

// Returns the minimal length of REGEX plus
// CURRENT-MIN-REST. This is similar to REGEX-MIN-LENGTH except that it
// recurses down into REGEX and sets the MIN-REST slots of REPETITION
// objects.
define generic compute-min-rest (regex, current-min-rest) ;

define method compute-min-rest (seq :: <seq>, current-min-rest)
  block (return)
    for (element in reverse(elements(seq)),
         last-min-rest = current-min-rest then this-min-rest,
         this-min-rest = compute-min-rest(element,
                                          last-min-rest) then compute-min-rest(element,
                                                                               last-min-rest))
    finally
      return(this-min-rest);
      #f;
    end for;
  end block;
end method compute-min-rest;

define method compute-min-rest (alternation :: <alternation>,
                                current-min-rest)
  let _acc = #f;
  for (choice in choices(alternation))
    _acc
     := if (_acc)
          min(_acc, compute-min-rest(choice, current-min-rest));
        else
          compute-min-rest(choice, current-min-rest);
        end if;
  finally
    _acc;
  end for;
end method compute-min-rest;

define method compute-min-rest (branch :: <branch>, current-min-rest)
  min(compute-min-rest(branch.then-regex, current-min-rest),
      compute-min-rest(else-regex(branch), current-min-rest));
end method compute-min-rest;

define method compute-min-rest (str :: <str>, current-min-rest)
  current-min-rest + str.len;
end method compute-min-rest;

define method compute-min-rest (filter :: <filter>, current-min-rest)
  current-min-rest + (filter.len | 0);
end method compute-min-rest;

define method compute-min-rest (repetition :: <repetition>, current-min-rest)
  min-rest(repetition) := current-min-rest;
  compute-min-rest(repetition.regex, current-min-rest);
  current-min-rest + minimum(repetition) * repetition.min-len;
end method compute-min-rest;

define method compute-min-rest (register :: <register>, current-min-rest)
  compute-min-rest(register.regex, current-min-rest);
end method compute-min-rest;

define method compute-min-rest (standalone :: <standalone>, current-min-rest)
  compute-min-rest(standalone.regex, 0);
end method compute-min-rest;

define method compute-min-rest (lookahead :: <lookahead>, current-min-rest)
  compute-min-rest(lookahead.regex, 0);
  current-min-rest;
end method compute-min-rest;

define method compute-min-rest (lookbehind :: <lookbehind>, current-min-rest)
  compute-min-rest(lookbehind.regex, current-min-rest + lookbehind.len);
  current-min-rest;
end method compute-min-rest;

define method compute-min-rest (regex :: <regex>, current-min-rest)
  select (regex by instance?)
    char-class | everything
       => current-min-rest + 1;
    #t
       => //  zero min-len and no embedded regexes (ANCHOR,
          //  BACK-REFERENCE, VOID, and WORD-BOUNDARY)
          /current-min-rest;
  end select;
end method compute-min-rest;

