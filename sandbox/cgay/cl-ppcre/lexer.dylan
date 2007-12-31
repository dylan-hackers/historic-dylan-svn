//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/lexer.lisp,v 1.27 2007/03/24 23:52:44 edi Exp $
//  The lexer's responsibility is to convert the regex string into a
//  sequence of tokens which are in turn consumed by the parser.
// 
//  The lexer is aware of Perl's 'extended mode' and it also 'knows'
//  (with a little help from the parser) how many register groups it
//  has opened so far. (The latter is necessary for interpreting
//  strings like "\\10" correctly.)
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

define method map-char-to-special-char-class (chr)
  // Maps escaped characters like "\d" to the tokens which represent
  // their associated character classes.
  select (chr)
    ('d')
       => #"digit-class";
    ('D')
       => #"non-digit-class";
    ('w')
       => #"word-char-class";
    ('W')
       => #"non-word-char-class";
    ('s')
       => #"whitespace-char-class";
    ('S')
       => #"non-whitespace-char-class";
    otherwise
       => #f;
  end select;
end method map-char-to-special-char-class;

// LEXER structures are used to hold the regex string which is
// currently lexed and to keep track of the lexer's state.
define class <lexer> (<object>)
  slot lexer-str :: <string> = "", init-keyword: #"lexer-str";
  slot lexer-len :: <integer> = 0, init-keyword: #"lexer-len";
  slot lexer-reg :: <integer> = 0, init-keyword: #"lexer-reg";
  slot lexer-pos :: <integer> = 0, init-keyword: #"lexer-pos";
  slot lexer-last-pos :: <list> = #f, init-keyword: #"lexer-last-pos";
end class <lexer>;

define method make-lexer (string :: <string>)
  make-lexer-internal(str: begin
                             let =string=2151 = string;
                             if (simple-string-p(=string=2151))
                               =string=2151;
                             else
                               as(<simple-string>, =string=2151);
                             end if;
                           end,
                      len: size(string));
end method make-lexer;

#f;

define method end-of-string-p (lexer)
  // Tests whether we're at the end of the regex string.
  lexer.lexer-len <= lexer.lexer-pos;
end method end-of-string-p;

#f;

define method looking-at-p (lexer, chr)
  // Tests whether the next character the lexer would see is CHR.
  // Does not respect extended mode.
  ~ end-of-string-p(lexer) & lexer.lexer-str[lexer.lexer-pos] = chr;
end method looking-at-p;

#f;

define method next-char-non-extended (lexer)
  // Returns the next character which is to be examined and updates the
  // POS slot. Does not respect extended mode.
  if (end-of-string-p(lexer))
    #f;
  else
    begin
      let _ = lexer.lexer-str[lexer.lexer-pos];
      inc!(lexer.lexer-pos);
      _;
    end;
  end if;
end method next-char-non-extended;

define method next-char (lexer)
  // Returns the next character which is to be examined and updates the
  // POS slot. Respects extended mode, i.e.  whitespace, comments, and also
  // nested comments are skipped if applicable.
  let next-char = next-char-non-extended(lexer);
  let last-loop-pos = #f;
  block (return)
    while (#t)
      //  remember where we started
      last-loop-pos := lexer.lexer-pos;
      //  first we look for nested comments like (?#foo)
      if (next-char & next-char = '(' & looking-at-p(lexer, '?'))
        inc!(lexer.lexer-pos);
        if (looking-at-p(lexer, '#'))
          //  must be a nested comment - so we have to search for
          //  the closing parenthesis
          begin
            let error-pos = lexer.lexer-pos - 2;
            if (~ //  loop 'til ')' or end of regex string and
                  //  return NIL if ')' wasn't encountered
            block (return)
              for (skip-char = next-char then next-char-non-extended(lexer),
                   while (skip-char & /=(skip-char, ')')))
              finally
                return(skip-char);
                #f;
              end for;
            end block)
              error(// LTD: Can't convert type specification.
                    #"ppcre-syntax-error", pos: error-pos,
                    format-control: "Comment group not closed",
                    format-arguments: list());
            end if;
          end;
          next-char := next-char-non-extended(lexer);
        else
          //  undo effect of previous INCF if we didn't see a #
          dec!(lexer.lexer-pos);
        end if;
      end if;
      if (*extended-mode-p*)
        //  now - if we're in extended mode - we skip whitespace and
        //  comments; repeat the following loop while we look at
        //  whitespace or #\#
        for (while next-char & (next-char = '#' | whitespacep(next-char)))
          next-char
           := if (next-char = '#')
                //  if we saw a comment marker skip until
                //  we're behind #\Newline...
                block (return)
                  for (skip-char = next-char then next-char-non-extended(lexer),
                       while skip-char & /=(skip-char, '\n'))
                  finally
                    return(next-char-non-extended(lexer));
                    #f;
                  end for;
                end block;
              else
                //  ...otherwise (whitespace) skip until we
                //  see the next non-whitespace character
                block (return)
                  for (skip-char = next-char then next-char-non-extended(lexer),
                       while skip-char & whitespacep(skip-char))
                  finally
                    return(skip-char);
                    #f;
                  end for;
                end block;
              end if;
        end for;
      end if;
      //  if the position has moved we have to repeat our tests
      //  because of cases like /^a (?#xxx) (?#yyy) {3}c/x which
      //  would be equivalent to /^a{3}c/ in Perl
      if (~ (lexer.lexer-pos > last-loop-pos)) return(next-char); end if;
    end while;
  end block;
end method next-char;

#f;

define method fail (lexer)
  // Moves (LEXER-POS LEXER) back to the last position stored in
  // (LEXER-LAST-POS LEXER) and pops the LAST-POS stack.
  if (~ lexer.lexer-last-pos)
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: #f,
          format-control: "LAST-POS stack of LEXER ~A is empty",
          format-arguments: list(lexer));
  end if;
  lexer.lexer-pos := pop!(lexer.lexer-last-pos);
  #f;
end method fail;

define method get-number (lexer, #key radix = 10, max-length, no-whitespace-p)
  // Read and consume the number the lexer is currently looking at and
  // return it. Returns NIL if no number could be identified.
  // RADIX is used as in PARSE-INTEGER. If MAX-LENGTH is not NIL we'll read
  // at most the next MAX-LENGTH characters. If NO-WHITESPACE-P is not NIL
  // we don't tolerate whitespace in front of the number.
  block (return-from-get-number)
    if (end-of-string-p(lexer)
         | (no-whitespace-p & whitespacep(lexer.lexer-str[lexer.lexer-pos])))
      return-from-get-number(#f);
    end if;
    let (integer, new-pos)
        = // LTD: Function PARSE-INTEGER not yet implemented.
          parse-integer(lexer.lexer-str, start: lexer.lexer-pos,
                        end: if (max-length)
                               let end-pos = lexer.lexer-pos + max-length;
                               let lexer-len = lexer.lexer-len;
                               if (end-pos < lexer-len)
                                 end-pos;
                               else
                                 lexer-len;
                               end if;
                             else
                               lexer.lexer-len;
                             end if,
                        radix: radix, junk-allowed: #t);
    if (integer & integer >= 0)
      lexer.lexer-pos := new-pos;
      integer;
    else
      #f;
    end if;
  end block;
end method get-number;

#f;

define method try-number (lexer, #key radix = 10, max-length, no-whitespace-p)
  // Like GET-NUMBER but won't consume anything if no number is seen.
  //  remember current position
  push!(lexer.lexer-pos, lexer.lexer-last-pos);
  let number
      = get-number(lexer, radix: radix, max-length: max-length,
                   no-whitespace-p: no-whitespace-p);
  number | fail(lexer);
end method try-number;

#f;

define method make-char-from-code (number, error-pos)
  // Create character from char-code NUMBER. NUMBER can be NIL
  // which is interpreted as 0. ERROR-POS is the position where
  // the corresponding number started within the regex string.
  let code = logand(255, number | 0);
  code < $char-code-limit & as(<character>, code)
   | error(// LTD: Can't convert type specification.
           #"ppcre-syntax-error", pos: error-pos,
           format-control: "No character for hex-code ~X",
           format-arguments: list(number));
end method make-char-from-code;

define method unescape-char (lexer)
  // Convert the characters(s) following a backslash into a token
  // which is returned. This function is to be called when the backslash
  // has already been consumed. Special character classes like \W are
  // handled elsewhere.
  if (end-of-string-p(lexer))
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: #f,
          format-control: "String ends with backslash",
          format-arguments: list());
  end if;
  let chr = next-char-non-extended(lexer);
  select (chr)
    ('E')
       => //  if \Q quoting is on this is ignored, otherwise it's just an
          //  #\E
          /if (*allow-quoting*) #"void"; else 'E'; end if;
    ('c')
       => let next-char = next-char-non-extended(lexer);
           if (~ next-char)
             error(// LTD: Can't convert type specification.
                   #"ppcre-syntax-error", pos: lexer.lexer-pos,
                   format-control: "Character missing after '\\c' at position ~A",
                   format-arguments: list());
           end if;
           as(<character>,
              logxor(64, as(<integer>, as-uppercase(next-char))));
    ('x')
       => let error-pos = lexer.lexer-pos;
           let number
               = get-number(lexer, radix: 16, max-length: 2,
                            no-whitespace-p: #t);
           //  note that it is OK if \x is followed by zero digits
           make-char-from-code(number, error-pos);
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
       => let error-pos = dec!(lexer.lexer-pos);
           let number = get-number(lexer, radix: 8, max-length: 3);
           make-char-from-code(number, error-pos);
    ('t')
       => '\t';
    ('n')
       => '\n';
    ('r')
       => '\013';
    ('f')
       => '\012';
    ('b')
       => '\08';
    ('a')
       => as(<character>, 7);
    ('e')
       => as(<character>, 27);
    #"otherwise"
       => //  all other characters aren't affected by a backslash
           chr;
  end select;
end method unescape-char;

define method collect-char-class (lexer)
  // Reads and consumes characters from regex string until a right
  // bracket is seen. Assembles them into a list (which is returned) of
  // characters, character ranges, like (:RANGE #\A #\E) for a-e, and
  // tokens representing special character classes.
  block (return-from-collect-char-class)
    let start-pos = lexer.lexer-pos;
    let hyphen-seen = #f;
    let last-char = #f;
    let list = #f;
    let handle-char
        = method (c)
            // Do the right thing with character C depending on whether
            // we're inside a range or not.
            if (hyphen-seen & last-char)
              begin
                head(list) := list(range: last-char, c);
                last-char := #f;
              end;
            else
              push!(c, list);
              last-char := c;
            end if;
            hyphen-seen := #f;
          end method;
    for (first = %t then %f,
         c = next-char-non-extended(lexer) then next-char-non-extended(lexer),
         while c)
      if (c = '\\')
        //  we've seen a backslash
        begin
          let next-char = next-char-non-extended(lexer);
          select (next-char)
            ('d', 'D', 'w', 'W', 's', 'S')
               => //  a special character class
                   push!(map-char-to-special-char-class(next-char), list);
                   //  if the last character was a hyphen
                   //  just collect it literally
                   if (hyphen-seen) push!('-', list); end if;
                   //  if the next character is a hyphen do the same
                   if (looking-at-p(lexer, '-'))
                     push!('-', list);
                     inc!(lexer.lexer-pos);
                   end if;
                   hyphen-seen := #f;
            ('E')
               => //  if \Q quoting is on we ignore \E,
                  //  otherwise it's just a plain #\E
                  /if (~ *allow-quoting*) handle-char('E'); end if;
            otherwise
               => //  otherwise unescape the following character(s)
                   dec!(lexer.lexer-pos);
                   handle-char(unescape-char(lexer));
          end select;
        end;
      elseif (first)
        //  the first character must not be a right bracket
        //  and isn't treated specially if it's a hyphen
        handle-char(c);
      elseif (c = ']')
        //  end of character class
        //  make sure we collect a pending hyphen
        if (hyphen-seen) hyphen-seen := #f; handle-char('-'); end if;
        //  reverse the list to preserve the order intended
        //  by the author of the regex string
        return-from-collect-char-class(reverse!(list));
      elseif (c = '-' & last-char & ~ hyphen-seen)
        //  if the last character was 'just a character'
        //  we expect to be in the middle of a range
        hyphen-seen := #t;
      elseif (c = '-')
        //  otherwise this is just an ordinary hyphen
        handle-char('-');
      else
        //  default case - just collect the character
        handle-char(c);
      end if;
    end for;
    //  we can only exit the loop normally if we've reached the end
    //  of the regex string without seeing a right bracket
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: start-pos,
          format-control: "Missing right bracket to close character class",
          format-arguments: list());
  end block;
end method collect-char-class;

define method maybe-parse-flags (lexer)
  // Reads a sequence of modifiers (including #\- to reverse their
  // meaning) and returns a corresponding list of "flag" tokens.  The
  // "x" modifier is treated specially in that it dynamically modifies
  // the behaviour of the lexer itself via the special variable
  // *EXTENDED-MODE-P*.
  let _
      = begin
          let set = #t;
          let _acc = make(<deque>);
          for (chr = next-char-non-extended(lexer) then next-char-non-extended(lexer),
               while cl-find(chr, "-imsx", test: \=))
            if (~ chr)
              error(// LTD: Can't convert type specification.
                    #"ppcre-syntax-error", pos: #f,
                    format-control: "Unexpected end of string",
                    format-arguments: list());
            end if;
            if (chr = '-')
              set := #f;
            else
              if (chr = 'x')
                *extended-mode-p* := set;
              else
                push-last(_acc,
                          if (set)
                            select (chr)
                              ('i')
                                 => #"case-insensitive-p";
                              ('m')
                                 => #"multi-line-mode-p";
                              ('s')
                                 => #"single-line-mode-p";
                              otherwise
                                 => #f;
                            end select;
                          else
                            select (chr)
                              ('i')
                                 => #"case-sensitive-p";
                              ('m')
                                 => #"not-multi-line-mode-p";
                              ('s')
                                 => #"not-single-line-mode-p";
                              otherwise
                                 => #f;
                            end select;
                          end if);
              end if;
            end if;
          finally
            _acc;
          end for;
        end;
  dec!(lexer.lexer-pos);
  _;
end method maybe-parse-flags;

define method get-quantifier (lexer)
  // Returns a list of two values (min max) if what the lexer is looking
  // at can be interpreted as a quantifier. Otherwise returns NIL and
  // resets the lexer to its old position.
  //  remember starting position for FAIL and UNGET-TOKEN functions
  push!(lexer.lexer-pos, lexer.lexer-last-pos);
  let next-char = next-char(lexer);
  select (next-char)
    ('*')
       => //  * (Kleene star): match 0 or more times
           #(0, #());
    ('+')
       => //  +: match 1 or more times
           #(1, #());
    ('?')
       => //  ?: match 0 or 1 times
           #(0, 1);
    ('{')
       => let num1 = get-number(lexer, no-whitespace-p: #t);
           if (num1)
             let next-char = next-char-non-extended(lexer);
             select (next-char)
               (',')
                  => let num2 = get-number(lexer, no-whitespace-p: #t);
                      let next-char = next-char-non-extended(lexer);
                      select (next-char)
                        ('}')
                           => //  this is the case {n,} (NUM2 is NIL) or {n,m}
                               list(num1, num2);
                        otherwise
                           => fail(lexer);
                      end select;
               ('}')
                  => //  this is the case {n}
                      list(num1, num1);
               otherwise
                  => fail(lexer);
             end select;
           else
             //  no number following left curly brace, so we treat it
             //  like a normal character
             fail(lexer);
           end if;
    #"otherwise"
       => fail(lexer);
  end select;
end method get-quantifier;

define method parse-register-name-aux (lexer)
  // Reads and returns the name in a named register group.  It is
  // assumed that the starting #< character has already been read.  The
  // closing #> will also be consumed.
  let end-name
      = find-key(copy-subsequence(lexer.lexer-str, start: lexer.lexer-pos),
                 curry(\==, '>'));
  if (~ end-name)
    //  there has to be > somewhere, syntax error otherwise
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: lexer.lexer-pos - 1,
          format-control: "Opening #< in named group has no closing #>",
          format-arguments: list());
  end if;
  let name = copy-sequence(lexer.lexer-str, lexer.lexer-pos, end-name);
  if (~ every?(method (char) (alphanumeric?(char) | '-' = char); end method,
               name))
    //  register name can contain only alphanumeric characters or #\-
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: lexer.lexer-pos,
          format-control: "Invalid character in named register group",
          format-arguments: list());
  end if;
  //  advance lexer beyond "<name>" part
  lexer.lexer-pos := end-name + 1;
  name;
end method parse-register-name-aux;

define method get-token (lexer)
  // Returns and consumes the next token from the regex string (or NIL).
  //  remember starting position for UNGET-TOKEN function
  push!(lexer.lexer-pos, lexer.lexer-last-pos);
  let next-char = next-char(lexer);
  if (next-char)
    select (next-char)
      (')')
         => #"close-paren";
      ('|')
         => #"vertical-bar";
      ('?')
         => #"question-mark";
      ('.')
         => #"everything";
      ('^')
         => #"start-anchor";
      ('$')
         => #"end-anchor";
      ('+', '*')
         => //  quantifiers will always be consumend by
            //  GET-QUANTIFIER, they must not appear here
            /error(// LTD: Can't convert type specification.
            /      #"ppcre-syntax-error", pos: lexer.lexer-pos - 1,
            /      format-control: "Quantifier '~A' not allowed",
            /      format-arguments: list(next-char));
      ('{')
         => let this-pos = lexer.lexer-pos;
             let this-last-pos = lexer.lexer-last-pos;
             unget-token(lexer);
             if (get-quantifier(lexer))
               error(// LTD: Can't convert type specification.
                     #"ppcre-syntax-error", pos: head(this-last-pos),
                     format-control: "Quantifier '~A' not allowed",
                     format-arguments: list(copy-sequence(lexer.lexer-str,
                                                          head(this-last-pos),
                                                          lexer.lexer-pos)));
             end if;
             begin
               lexer.lexer-pos := this-pos;
               lexer.lexer-last-pos := this-last-pos;
             end;
             next-char;
      ('[')
         => //  left bracket always starts a character class
             pair(if (looking-at-p(lexer, '^'))
                    inc!(lexer.lexer-pos);
                    #"inverted-char-class";
                  else
                    #"char-class";
                  end if,
                  collect-char-class(lexer));
      ('\\')
         => let next-char = next-char-non-extended(lexer);
             select (next-char)
               ('A')
                  => #"modeless-start-anchor";
               ('Z')
                  => #"modeless-end-anchor";
               ('z')
                  => #"modeless-end-anchor-no-newline";
               ('b')
                  => #"word-boundary";
               ('B')
                  => #"non-word-boundary";
               ('k')
                  => if (*allow-named-registers* & looking-at-p(lexer, '<'))
                       //  back-referencing a named register
                       inc!(lexer.lexer-pos);
                       list(back-reference: reverse!(parse-register-name-aux(lexer)));
                     else
                       //  false alarm, just unescape \k
                       'k';
                     end if;
               ('d', 'D', 'w', 'W', 's', 'S')
                  => //  these will be treated like character classes
                      map-char-to-special-char-class(next-char);
               ('1', '2', '3', '4', '5', '6', '7', '8', '9')
                  => let old-pos = dec!(lexer.lexer-pos);
                      let backref-number :: <integer> = get-number(lexer);
                      if (backref-number > lexer.lexer-reg
                           & 10 <= backref-number)
                        //  \10 and higher are treated as octal
                        //  character codes if we haven't
                        //  opened that much register groups
                        //  yet
                        lexer.lexer-pos := old-pos;
                        //  re-read the number from the old
                        //  position and convert it to its
                        //  corresponding character
                        make-char-from-code(get-number(lexer, radix: 8,
                                                       max-length: 3),
                                            old-pos);
                      else
                        //  otherwise this must refer to a
                        //  backreference
                        list(back-reference: backref-number);
                      end if;
               ('0')
                  => let old-pos = dec!(lexer.lexer-pos);
                      make-char-from-code(get-number(lexer, radix: 8,
                                                     max-length: 3),
                                          old-pos);
               otherwise
                  => //  in all other cases just unescape the
                     //  character
                     /dec!(lexer.lexer-pos);
                     /unescape-char(lexer);
             end select;
      ('(')
         => //  an open parenthesis might mean different things
            //  depending on what follows...
            /if (looking-at-p(lexer, '?'))
            /  //  this is the case '(?' (and probably more behind)
            /  inc!(lexer.lexer-pos);
            /  //  we have to check for modifiers first
            /  //  because a colon might follow
            /  begin
            /    let flags = maybe-parse-flags(lexer);
            /    let next-char = next-char-non-extended(lexer);
            /    //  modifiers are only allowed if a colon
            /    //  or a closing parenthesis are following
            /    if (flags & ~ cl-find(next-char, ":)", test: \=))
            /      error(// LTD: Can't convert type specification.
            /            #"ppcre-syntax-error",
            /            pos: head(lexer.lexer-last-pos),
            /            format-control: "Sequence '~A' not recognized",
            /            format-arguments: list(copy-sequence(lexer.lexer-str,
            /                                                 head(lexer
            /                                                      .lexer-last-pos),
            /                                                 lexer
            /                                                 .lexer-pos)));
            /    end if;
            /    select (next-char)
            /      (#())
            /         => //  syntax error
            /             error(// LTD: Can't convert type specification.
            /                   #"ppcre-syntax-error", pos: #f,
            /                   format-control: "End of string following '(?'",
            /                   format-arguments: list());
            /      (')')
            /         => //  an empty group except for the flags
            /            //  (if there are any)
            /            /flags & pair(flags: flags) | #"void";
            /      ('(')
            /         => //  branch
            /             #"open-paren-paren";
            /      ('>')
            /         => //  standalone
            /             #"open-paren-greater";
            /      ('=')
            /         => //  positive look-ahead
            /             #"open-paren-equal";
            /      ('!')
            /         => //  negative look-ahead
            /             #"open-paren-exclamation";
            /      (':')
            /         => //  non-capturing group - return flags as
            /            //  second value
            /            /values(open-paren-colon: flags);
            /      ('<')
            /         => let next-char = next-char-non-extended(lexer);
            /             if (alpha-char?(next-char))
            /               //  we have encountered a named group
            /               //  are we supporting register naming?
            /               if (~ *allow-named-registers*)
            /                 error(// LTD: Can't convert type specification.
            /                       #"ppcre-syntax-error",
            /                       pos: lexer.lexer-pos - 1,
            /                       format-control: "Character '~A' may not follow '(?<'",
            /                       format-arguments: list(next-char));
            /               end if;
            /               //  put the letter back
            /               dec!(lexer.lexer-pos);
            /               //  named group
            /               #"open-paren-less-letter";
            /             else
            /               select (next-char)
            /                 ('=')
            /                    => //  positive look-behind
            /                        #"open-paren-less-equal";
            /                 ('!')
            /                    => //  negative look-behind
            /                        #"open-paren-less-exclamation";
            /                 (')')
            /                    => //  Perl allows "(?<)" and treats
            /                       //  it like a null string
            /                       /#"void";
            /                 (#())
            /                    => //  syntax error
            /                        error(// LTD: Can't convert type specification.
            /                              #"ppcre-syntax-error", pos: #f,
            /                              format-control: "End of string following '(?<'",
            /                              format-arguments: list());
            /                 otherwise
            /                    => //  also syntax error
            /                        error(// LTD: Can't convert type specification.
            /                              #"ppcre-syntax-error",
            /                              pos: lexer.lexer-pos - 1,
            /                              format-control: "Character '~A' may not follow '(?<'",
            /                              format-arguments: list(next-char));
            /               end select;
            /             end if;
            /      otherwise
            /         => error(// LTD: Can't convert type specification.
            /                  #"ppcre-syntax-error",
            /                  pos: lexer.lexer-pos - 1,
            /                  format-control: "Character '~A' may not follow '(?'",
            /                  format-arguments: list(next-char));
            /    end select;
            /  end;
            /else
            /  //  if next-char was not #\? (this is within
            /  //  the first COND), we've just seen an opening
            /  //  parenthesis and leave it like that
            /  #"open-paren";
            /end if;
      otherwise
         => //  all other characters are their own tokens
             next-char;
    end select;
    //  we didn't get a character (this if the "else" branch from
    //  the first IF), so we don't return a token but NIL
    else
    pop!(lexer.lexer-last-pos);
    #f;
  end if;
end method get-token;

#f;

define method unget-token (lexer)
  // Moves the lexer back to the last position stored in the LAST-POS stack.
  if (lexer.lexer-last-pos)
    lexer.lexer-pos := pop!(lexer.lexer-last-pos);
  else
    error("No token to unget (this should not happen)");
  end if;
end method unget-token;

#f;

define method start-of-subexpr-p (lexer)
  // Tests whether the next token can start a valid sub-expression, i.e.
  // a stand-alone regex.
  let pos = lexer.lexer-pos;
  let next-char = next-char(lexer);
  ~ (empty?(next-char)
      | begin
          let _ = member?(next-char, #(')', '|'), test: \=);
          (lexer.lexer-pos := pos);
          _;
        end);
end method start-of-subexpr-p;

