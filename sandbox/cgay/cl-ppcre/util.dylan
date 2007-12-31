//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/util.lisp,v 1.36 2007/07/24 21:32:38 edi Exp $
//  Utility functions and constants dealing with the hash-tables
//  we use to encode character classes
//  Hash-tables are treated like sets, i.e. a character C is a member of the
//  hash-table H iff (GETHASH C H) is true.
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
#"with-unique-names";

// LTD: No macros.
#"with-rebinding";

//  the following DEFCONSTANT statements are wrapped with
//  (UNLESS (BOUNDP ...) ...) to make SBCL happy
if (~ // LTD: Function BOUNDP not yet implemented.
      boundp(#"+digit-hash+"))
  // Hash-table containing the digits from 0 to 9.
  define constant +digit-hash+ =
    make-char-hash(method (chr) \<=('0', chr, '9'); end method);
end if;

if (~ // LTD: Function BOUNDP not yet implemented.
      boundp(#"+word-char-hash+"))
  // Hash-table containing all "word" characters.
  define constant +word-char-hash+ = make-char-hash(word-char-p);
end if;

if (~ // LTD: Function BOUNDP not yet implemented.
      boundp(#"+whitespace-char-hash+"))
  // Hash-table containing all whitespace characters.
  define constant +whitespace-char-hash+ = make-char-hash(whitespacep);
end if;

define method merge-inverted-hash (hash1, hash2)
  // Returns the "sum" of HASH1 and the "inverse" of HASH2. This is
  // a destructive operation on HASH1.
  for (c :: <integer> from 0 below *regex-char-code-limit*,
       chr = as(<character>, c) then as(<character>, c))
    if (chr & ~ hash2[chr]) hash1[chr] := #t; end if;
  end for;
  hash1;
end method merge-inverted-hash;

// LTD: No macros.
#"maybe-coerce-to-simple-string";

#f;

define method nsubseq (sequence, start, #key end = size(sequence))
  // Return a subsequence by pointing to location in original sequence.
  make(<array>, dimensions: end - start);
end method nsubseq;

define method normalize-var-list (var-list)
  // Utility function for REGISTER-GROUPS-BIND and
  // DO-REGISTER-GROUPS. Creates the long form (a list of (FUNCTION VAR)
  // entries) out of the short form of VAR-LIST.
  let _acc = #();
  for (element in var-list)
    if (instance?(element, <pair>))
      _acc
       := concatenate!(_acc,
                       begin
                         let _acc = make(<deque>);
                         for (var in tail(element))
                           push-last(_acc, list(first(element), var));
                         finally
                           _acc;
                         end for;
                       end);
    else
      push-last(_acc, list(#(#"function", #"identity"), element));
    end if;
  finally
    _acc;
  end for;
end method normalize-var-list;

define method string-list-to-simple-string (string-list)
  // Concatenates a list of strings to one simple-string.
  let total-size :: <integer> = 0;
  for (string in string-list) inc!(total-size, size(string)); end for;
  let result-string = make(<simple-string>, size: total-size, fill: #f);
  let curr-pos :: <integer> = 0;
  for (string in string-list)
    replace-subsequence!(copy-subsequence(result-string, start: curr-pos),
                         string);
    inc!(curr-pos, size(string));
  end for;
  result-string;
end method string-list-to-simple-string;

