//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/convert.lisp,v 1.27 2007/06/04 19:22:08 edi Exp $
//  Here the parse tree is converted into its internal representation
//  using REGEX objects.  At the same time some optimizations are
//  already applied.
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

//  The flags that represent the "ism" modifiers are always kept
//  together in a three-element list. We use the following macros to
//  access individual elements.
// LTD: No macros.
#"case-insensitive-mode-p";

// LTD: No macros.
#"multi-line-mode-p";

// LTD: No macros.
#"single-line-mode-p";

define method set-flag (token)
  // Reads a flag token and sets or unsets the corresponding entry in
  // the special FLAGS list.
  select (token)
    (#"case-insensitive-p")
       => first(flags) := #t;
    (#"case-sensitive-p")
       => first(flags) := #f;
    (#"multi-line-mode-p")
       => second(flags) := #t;
    (#"not-multi-line-mode-p")
       => second(flags) := #f;
    (#"single-line-mode-p")
       => third(flags) := #t;
    (#"not-single-line-mode-p")
       => third(flags) := #f;
    otherwise
       => error(// LTD: Can't convert type specification.
                #"ppcre-syntax-error", pos: #f,
                format-control: "Unknown flag token ~A",
                format-arguments: list(token));
  end select;
end method set-flag;

define method add-range-to-hash (hash, from, to)
  // Adds all characters from character FROM to character TO (inclusive)
  // to the char class hash HASH. Does the right thing with respect to
  // case-(in)sensitivity as specified by the special variable FLAGS.
  let from-code = as(<integer>, from);
  let to-code = as(<integer>, to);
  if (from-code > to-code)
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: #f,
          format-control: "Invalid range from ~A to ~A in char-class",
          format-arguments: list(from, to));
  end if;
  if (first(flags))
    for (code from from-code to to-code,
         chr = as(<character>, code) then as(<character>, code))
      begin hash[as-uppercase(chr)] := #t; hash[as-lowercase(chr)] := #t; end;
    end for;
  else
    for (code from from-code to to-code)
      hash[as(<character>, code)] := #t;
    end for;
  end if;
  hash;
end method add-range-to-hash;

define method maybe-split-repetition (regex, greedyp, minimum :: <integer>,
                                      maximum
                                       :: type-union(<integer>,
                                                     singleton(#f)),
                                      min-len, length, reg-seen)
  // Splits a REPETITION object into a constant and a varying part if
  // applicable, i.e. something like
  //   a{3,} -> a{3}a*
  // The arguments to this function correspond to the REPETITION slots of
  // the same name.
  block (return-from-maybe-split-repetition)
    //  note the usage of COPY-REGEX here; we can't use the same REGEX
    //  object in both REPETITIONS because they will have different
    //  offsets
    if (maximum)
      if (zero?(maximum))
        //  trivial case: don't repeat at all
        return-from-maybe-split-repetition(make(<void>));
      end if;
      if (1 = minimum & minimum = maximum)
        //  another trivial case: "repeat" exactly once
        return-from-maybe-split-repetition(regex);
      end if;
    end if;
    let constant-repetition
        = if (positive?(minimum))
            make(<repetition>, regex: copy-regex(regex), greedyp: greedyp,
                 minimum: minimum, maximum: minimum, min-len: min-len,
                 len: length, contains-register-p: reg-seen);
          else
            //  don't create garbage if minimum is 0
            #f;
          end if;
    if (maximum & maximum = minimum)
      return-from-maybe-split-repetition(//  no varying part needed because min = max
                                         constant-repetition);
    end if;
    let varying-repetition
        = make(<repetition>, regex: regex, greedyp: greedyp, minimum: 0,
               maximum: if (maximum) maximum - minimum; else #f; end if,
               min-len: min-len, len: length, contains-register-p: reg-seen);
    if (zero?(minimum))
      //  min = 0, no constant part needed
      varying-repetition;
    elseif (1 = minimum)
      //  min = 1, constant part needs no REPETITION wrapped around
      make(<seq>, elements: list(copy-regex(regex), varying-repetition));
    else
      //  general case
      make(<seq>, elements: list(constant-repetition, varying-repetition));
    end if;
  end block;
end method maybe-split-repetition;

//  During the conversion of the parse tree we keep track of the start
//  of the parse tree in the special variable STARTS-WITH which'll
//  either hold a STR object or an EVERYTHING object. The latter is the
//  case if the regex starts with ".*" which implicitly anchors the
//  regex at the start (perhaps modulo #\Newline).
define method maybe-accumulate (str)
  // Accumulate STR into the special variable STARTS-WITH if
  // ACCUMULATE-START-P (also special) is true and STARTS-WITH is either
  // NIL or a STR object of the same case mode. Always returns NIL.
  if (accumulate-start-p)
    let g2143 = starts-with;
    if (instance?(g2143, <str>))
      #f;
      if (starts-with.case-insensitive-p == str.case-insensitive-p)
        starts-with.len := starts-with.len + str.len;
        // LTD: Function ADJUST-ARRAY not yet implemented.
        adjust-array(starts-with.str, starts-with.len, fill-pointer: #t);
        begin
          copy-sequence(starts-with.str, starts-with.len - str.len)
           := str(str);
          skip(str) := #t;
        end;
      else
        accumulate-start-p := #f;
      end if;
    elseif (instance?(g2143, singleton(#f)))
      #f;
      begin
        starts-with
         := make(<str>, str: "", case-insensitive-p: str.case-insensitive-p);
        starts-with.str
         := begin
              let _vector = make(<array>, dimensions: str.len);
              (size(_vector) := #t);
              _vector;
            end;
        starts-with.len := str.len;
        skip(str) := #t;
      end;
    elseif (instance?(g2143, <everything>))
      #f;
      accumulate-start-p := #f;
    else
      error(// LTD: Can't convert type specification.
            #"case-failure", name: #"etypecase", datum: g2143,
            expected-type: #(#"or", #"everything", #"null", #"str"),
            possibilities: #(#"everything", #"null", #"str"));
    end if;
  end if;
  #f;
end method maybe-accumulate;

define method convert (parse-tree)
  // Converts the parse tree PARSE-TREE into an equivalent REGEX object
  // and returns three values: the REGEX object, the number of registers
  // seen and an object the regex starts with which is either a STR object
  // or an EVERYTHING object (if the regex starts with something like
  // ".*") or NIL.
  let flags = list(#f, #f, #f);
  let reg-num = 0;
  let reg-names = #f;
  let named-reg-seen = #f;
  let accumulate-start-p = #t;
  let starts-with = #f;
  let max-back-ref = 0;
  let converted-parse-tree = convert-aux(parse-tree);
  //  make sure we don't reference registers which aren't there
  if (max-back-ref > reg-num)
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: #f,
          format-control: "Backreference to register ~A which has not been defined",
          format-arguments: list(max-back-ref));
  end if;
  if (instance?(starts-with, <str>))
    starts-with.str := as(<simple-string>, starts-with.str);
  end if;
  values(converted-parse-tree, reg-num, starts-with,
         //  we can't simply use *ALLOW-NAMED-REGISTERS*
         //  since parse-tree syntax ignores it
         if (named-reg-seen) reverse!(reg-names); end if);
end method convert;

