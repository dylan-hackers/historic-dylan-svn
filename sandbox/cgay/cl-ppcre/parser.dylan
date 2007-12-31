//  -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-PPCRE; Base: 10 -*-
//  $Header: /usr/local/cvsrep/cl-ppcre/parser.lisp,v 1.24 2007/03/24 23:52:44 edi Exp $
//  The parser will - with the help of the lexer - parse a regex
//  string and convert it into a "parse tree" (see docs for details
//  about the syntax of these trees). Note that the lexer might return
//  illegal parse trees. It is assumed that the conversion process
//  later on will track them down.
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

define method group (lexer)
  // Parses and consumes a <group>.
  // The productions are: <group> -> "("<regex>")"
  //                                 "(?:"<regex>")"
  //                                 "(?>"<regex>")"
  //                                 "(?<flags>:"<regex>")"
  //                                 "(?="<regex>")"
  //                                 "(?!"<regex>")"
  //                                 "(?<="<regex>")"
  //                                 "(?<!"<regex>")"
  //                                 "(?("<num>")"<regex>")"
  //                                 "(?("<regex>")"<regex>")"
  //                                 "(?<name>"<regex>")" (when *ALLOW-NAMED-REGISTERS* is T)
  //                                 <legal-token>
  // where <flags> is parsed by the lexer function MAYBE-PARSE-FLAGS.
  // Will return <parse-tree> or (<grouping-type> <parse-tree>) where
  // <grouping-type> is one of six keywords - see source for details.
  let (open-token, flags) = get-token(lexer);
  if (open-token == #"open-paren-paren")
    //  special case for conditional regular expressions; note
    //  that at this point we accept a couple of illegal
    //  combinations which'll be sorted out later by the
    //  converter
    begin
      let open-paren-pos :: <integer> = head(lexer.lexer-last-pos);
      let number = try-number(lexer, no-whitespace-p: #t);
      fluid-bind (*extended-mode-p* = *extended-mode-p*)
        if (number)
          //  condition is a number (i.e. refers to a
          //  back-reference)
          begin
            let inner-close-token = get-token(lexer);
            let reg-expr = reg-expr(lexer);
            let close-token = get-token(lexer);
            if (~ (inner-close-token == #"close-paren"))
              error(// LTD: Can't convert type specification.
                    #"ppcre-syntax-error", pos: open-paren-pos + 2,
                    format-control: "Opening paren has no matching closing paren",
                    format-arguments: list());
            end if;
            if (~ (close-token == #"close-paren"))
              error(// LTD: Can't convert type specification.
                    #"ppcre-syntax-error", pos: open-paren-pos,
                    format-control: "Opening paren has no matching closing paren",
                    format-arguments: list());
            end if;
            list(branch: number, reg-expr);
          end;
        else
          //  condition must be a full regex (actually a
          //  look-behind or look-ahead); and here comes a
          //  terrible kludge: instead of being cleanly
          //  separated from the lexer, the parser pushes
          //  back the lexer by one position, thereby
          //  landing in the middle of the 'token' "(?(" -
          //  yuck!!
          dec!(lexer.lexer-pos);
          begin
            let inner-reg-expr = group(lexer);
            let reg-expr = reg-expr(lexer);
            let close-token = get-token(lexer);
            if (~ (close-token == #"close-paren"))
              error(// LTD: Can't convert type specification.
                    #"ppcre-syntax-error", pos: open-paren-pos,
                    format-control: "Opening paren has no matching closing paren",
                    format-arguments: list());
            end if;
            list(branch: inner-reg-expr, reg-expr);
          end;
        end if;
      end fluid-bind;
    end;
  elseif (member?(open-token,
                  #(#"open-paren", #"open-paren-colon", #"open-paren-greater",
                    #"open-paren-equal", #"open-paren-exclamation",
                    #"open-paren-less-equal", #"open-paren-less-exclamation",
                    #"open-paren-less-letter")))
    //  make changes to extended-mode-p local
    begin
      fluid-bind (*extended-mode-p* = *extended-mode-p*)
        let open-paren-pos = head(lexer.lexer-last-pos);
        let register-name
            = if (open-token == #"open-paren-less-letter")
                parse-register-name-aux(lexer);
              end if;
        let reg-expr = reg-expr(lexer);
        let close-token = get-token(lexer);
        if (open-token == #"open-paren"
             | open-token == #"open-paren-less-letter")
          //  if this is the "("<regex>")" or "(?"<name>""<regex>")" production we have to
          //  increment the register counter of the lexer
          inc!(lexer.lexer-reg);
        end if;
        if (~ (close-token == #"close-paren"))
          //  the token following <regex> must be the closing
          //  parenthesis or this is a syntax error
          error(// LTD: Can't convert type specification.
                #"ppcre-syntax-error", pos: open-paren-pos,
                format-control: "Opening paren has no matching closing paren",
                format-arguments: list());
        end if;
        if (flags)
          //  if the lexer has returned a list of flags this must
          //  have been the "(?:"<regex>")" production
          pair(group: concatenate!(flags, list(reg-expr)));
        elseif (open-token == #"open-paren-less-letter")
          list(named-register: //  every string was reversed, so we have to
                               //  reverse it back to get the name
               reverse!(register-name), reg-expr);
        else
          list(select (open-token)
                 (#"open-paren")
                    => #"register";
                 (#"open-paren-colon")
                    => #"group";
                 (#"open-paren-greater")
                    => #"standalone";
                 (#"open-paren-equal")
                    => #"positive-lookahead";
                 (#"open-paren-exclamation")
                    => #"negative-lookahead";
                 (#"open-paren-less-equal")
                    => #"positive-lookbehind";
                 (#"open-paren-less-exclamation")
                    => #"negative-lookbehind";
                 otherwise
                    => #f;
               end select,
               reg-expr);
        end if;
      end fluid-bind;
    end;
  else
    //  this is the <legal-token> production; <legal-token> is
    //  any token which passes START-OF-SUBEXPR-P (otherwise
    //  parsing had already stopped in the SEQ method)
    open-token;
  end if;
end method group;

define method greedy-quant (lexer)
  // Parses and consumes a <greedy-quant>.
  // The productions are: <greedy-quant> -> <group> | <group><quantifier>
  // where <quantifier> is parsed by the lexer function GET-QUANTIFIER.
  // Will return <parse-tree> or (:GREEDY-REPETITION <min> <max> <parse-tree>).
  let group = group(lexer);
  let token = get-quantifier(lexer);
  if (token)
    //  if GET-QUANTIFIER returned a non-NIL value it's the
    //  two-element list (<min> <max>)
    list(greedy-repetition: first(token), second(token), group);
  else
    group;
  end if;
end method greedy-quant;

define method quant (lexer)
  // Parses and consumes a <quant>.
  // The productions are: <quant> -> <greedy-quant> | <greedy-quant>"?".
  // Will return the <parse-tree> returned by GREEDY-QUANT and optionally
  // change :GREEDY-REPETITION to :NON-GREEDY-REPETITION.
  let greedy-quant = greedy-quant(lexer);
  let pos = lexer.lexer-pos;
  let next-char = next-char(lexer);
  if (next-char)
    if (next-char = '?')
      head(greedy-quant) := #"non-greedy-repetition";
    else
      lexer.lexer-pos := pos;
    end if;
  end if;
  greedy-quant;
end method quant;

define method seq (lexer)
  // Parses and consumes a <seq>.
  // The productions are: <seq> -> <quant> | <quant><seq>.
  // Will return <parse-tree> or (:SEQUENCE <parse-tree> <parse-tree>).
  let make-array-from-two-chars
      = method (char1, char2)
          let string
              = begin
                  let _vector = make(<string>, size: 2);
                  size(_vector) := #t;
                  _vector;
                end;
          string[0] := char1;
          string[1] := char2;
          string;
        end method;
  //  Note that we're calling START-OF-SUBEXPR-P before we actually try
  //  to parse a <seq> or <quant> in order to catch empty regular
  //  expressions
  if (start-of-subexpr-p(lexer))
    let quant = quant(lexer);
    if (start-of-subexpr-p(lexer))
      let seq = seq(lexer);
      let quant-is-char-p = instance?(quant, <character>);
      let seq-is-sequence-p
          = instance?(seq, <pair>) & first(seq) == #"sequence";
      if (quant-is-char-p & instance?(seq, <character>))
        make-array-from-two-chars(seq, quant);
      elseif (quant-is-char-p & instance?(seq, <string>))
        add!(seq, quant);
        seq;
      elseif (quant-is-char-p & seq-is-sequence-p
               & instance?(second(seq), <character>))
        if (tail(tail(seq)))
          tail(seq)
           := pair(make-array-from-two-chars(second(seq), quant),
                   tail(tail(seq)));
          seq;
        else
          make-array-from-two-chars(second(seq), quant);
        end if;
      elseif (quant-is-char-p & seq-is-sequence-p
               & instance?(second(seq), <string>))
        if (tail(tail(seq)))
          tail(seq)
           := pair(begin add!(second(seq), quant); second(seq); end,
                   tail(tail(seq)));
          seq;
        else
          add!(second(seq), quant);
          second(seq);
        end if;
      elseif (seq-is-sequence-p)
        //  if <seq> is also a :SEQUENCE parse tree we merge
        //  both lists into one to avoid unnecessary consing
        tail(seq) := pair(quant, tail(seq));
        seq;
      else
        list(sequence: quant, seq);
      end if;
    else
      quant;
    end if;
  else
    #"void";
  end if;
end method seq;

define method reg-expr (lexer)
  // Parses and consumes a <regex>, a complete regular expression.
  // The productions are: <regex> -> <seq> | <seq>"|"<regex>.
  // Will return <parse-tree> or (:ALTERNATION <parse-tree> <parse-tree>).
  let pos = lexer.lexer-pos;
  select (next-char(lexer))
    (#())
       => //  if we didn't get any token we return :VOID which stands for
          //  "empty regular expression"
          /#"void";
    ('|')
       => //  now check whether the expression started with a vertical
          //  bar, i.e. <seq> - the left alternation - is empty
          /list(alternation: #"void", reg-expr(lexer));
    otherwise
       => //  otherwise un-read the character we just saw and parse a
          //  <seq> plus the character following it
          /lexer.lexer-pos := pos;
          /let seq = seq(lexer);
          /let pos = lexer.lexer-pos;
          /select (next-char(lexer))
          /  (#())
          /     => //  no further character, just a <seq>
          /         seq;
          /  ('|')
          /     => let reg-expr = reg-expr(lexer);
          /         if (instance?(reg-expr, <pair>)
          /              & first(reg-expr) == #"alternation")
          /           //  again we try to merge as above in SEQ
          /           tail(reg-expr) := pair(seq, tail(reg-expr));
          /           reg-expr;
          /         else
          /           list(alternation: seq, reg-expr);
          /         end if;
          /  otherwise
          /     => //  a character which is not a vertical bar - this is
          /        //  either a syntax error or we're inside of a group and
          /        //  the next character is a closing parenthesis; so we
          /        //  just un-read the character and let another function
          /        //  take care of it
          /        /lexer.lexer-pos := pos;
          /        /seq;
          /end select;
  end select;
end method reg-expr;

define method reverse-strings (parse-tree)
  if (instance?(parse-tree, <string>))
    reverse!(parse-tree);
  elseif (instance?(parse-tree, <pair>))
    for (parse-tree-rest = parse-tree then tail(parse-tree-rest),
         until empty?(parse-tree-rest), while parse-tree-rest)
      head(parse-tree-rest) := reverse-strings(head(parse-tree-rest));
    end for;
    parse-tree;
  else
    parse-tree;
  end if;
end method reverse-strings;

define method parse-string (string)
  // Translate the regex string STRING into a parse tree.
  let lexer = make-lexer(string);
  let parse-tree = reverse-strings(reg-expr(lexer));
  //  check whether we've consumed the whole regex string
  if (end-of-string-p(lexer))
    parse-tree;
  else
    error(// LTD: Can't convert type specification.
          #"ppcre-syntax-error", pos: lexer.lexer-pos,
          format-control: "Expected end of string", format-arguments: list());
  end if;
end method parse-string;

