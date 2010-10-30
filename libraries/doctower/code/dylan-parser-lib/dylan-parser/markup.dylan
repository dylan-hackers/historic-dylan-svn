module: dylan-parser
synopsis: Code for passing a <doc-comment-token> to the markup parser.


/** Synopsis: Parse documentation comment as markup. **/
define method markup-from-comment
   (token :: <doc-comment-token>, context :: <dylan-parse-context>)
=> (markup-content :: false-or(<markup-content-token>),
    parse-failure :: false-or(<parse-failure>))

   // Pad out first line with correct indentation from start of line.

   let (l, c) = context.line-col-position(token.parse-start);
   let comment-start-col = c - 1; // For a 0-origin column number.
   let first-line-filler = spaces(comment-start-col);
   let comment-contents = concatenate(first-line-filler, token.source-text);
   
   let base-line-col-position =
         method (pos :: <integer>) => (l :: <integer>, c :: <integer>)
            context.line-col-position(pos - first-line-filler.size + token.parse-start);
         end method;

   // Replace comment scaffolding with spaces and ensure there is an EOL for
   // markup parser.

   comment-contents := strip-doc-comment(comment-contents, token);
   if (comment-contents.last ~= '\n')
      comment-contents := add!(comment-contents, '\n');
   end if;
   
   // Parse markup.

   let text-stream = make(<string-stream>, contents: comment-contents);
   block ()
      let markup = parse-markup(text-stream, base-line-col-position, context.file-locator);
      values(markup, #f);
   cleanup
      text-stream.close;
   exception (parse-failure :: <parse-failure>)
      values(#f, parse-failure);
   end block;
end method;


/**
=== Generic: strip-doc-comment ===

Synopsis: Remove comment characters that would interfere with markup parsing.

The idea is that any documentation comment tokens and marginal decorations are
invisible scaffolding around the actual comment content. The identation of the
comment text relative to each line in the source code file does not change. That
is,

: 0123456789
:   /** Syn:
:    *  Line
:   **/

gets transformed into

: 0123456789
:       Syn:
:       Line
:

for markup parsing.
**/

define method strip-doc-comment
   (comment-text :: <string>, token :: <delim-doc-comment-token>)
=> (stripped-text :: <string>)
   let lines = split(comment-text, '\n', trim?: #f);
   
   // Find limit of left-hand margin decorations based on first line.
   let (delim-start, delim-end) = substring-position(lines.first, "/**");
   lines.first := replace-subsequence!(lines.first, spaces(3),
                                       start: delim-start, end: delim-end);
   let margin-limit = delim-end;
   
   // Remove closing comment token. Comment will end with "*/" or "**/".
   let delim-start = max(0, lines.last.size - 3);
   if (lines.last[delim-start] ~= '*')
      delim-start := delim-start + 1;
   end if;
   lines.last := copy-sequence(lines.last, end: delim-start);
   
   // Find common left-hand margin decoration of remaining lines. Margin decorations
   // might incorporate closing comment token on last line, but this is removed.
   // To keep that from screwing up the common prefix of the other lines, if the
   // last line is blank, don't include it.
   let (marginal-start, marginal-end) =
         if (lines.size > 1)
            let blank-last? = every?(curry(\=, ' '), lines.last);
            let usable-size = if (blank-last?) lines.size - 1 else lines.size end;
            values (1, usable-size);
         else
            values (0, 0);
         end if;
         
   let margin-size =
         if (marginal-end - marginal-start < 1)
            0
         else
            let margined-lines =
                  copy-sequence(lines, start: marginal-start, end: marginal-end);
            common-margin(margined-lines, margin-limit).size
         end if;
   
   // Replace margin decoration with spaces.
   let margin = spaces(margin-size);
   for (i from marginal-start below marginal-end)
      lines[i] := replace-subsequence!(lines[i], margin, end: margin.size)
   end for;

   apply(join, "\n", lines);
end method;


define method strip-doc-comment 
   (comment-text :: <string>, token :: <eol-doc-comments-token>)
=> (stripped-text :: <string>)
   // Since each line ends with "\n", there will always be at least two elements.
   let lines = split(comment-text, '\n', trim?: #f);
   
   // Replace leading triple-slashes with spaces. Last line will be empty.
   let delim-spaces = spaces(3);
   for (i from 0 below lines.size - 1)
      let (delim-start, delim-end) = substring-position(lines[i], "///");
      lines[i] := replace-subsequence!(lines[i], delim-spaces,
                                         start: delim-start, end: delim-end);
   end for;
   
   apply(join, "\n", lines);
end method;


/** Margin decorations only include spaces or asterisks **/
define method common-margin
   (lines :: <sequence> /* of <string> */, limit :: <integer>)
=> (prefix :: <string>)
   let line-limits = map(
         method (line :: <string>) => (cap :: <integer>)
            find-key(line,
                     method (char :: <character>) => (end-of-prefix?)
                        ~(char = ' ' | char = '*')
                     end method, failure: line.size)
         end method, lines);
   let shortest-line-size = apply(min, limit, line-limits);
   let end-index :: <integer> = 
         if (lines.size > 1)
            let test-lines = copy-sequence(lines, start: 1);
            block (found-mismatch)
               for (i from 0 below shortest-line-size, common-char in lines.first)
                  for (test-line in test-lines)
                     if (test-line[i] ~= common-char)
                        found-mismatch(i)
                     end if;
                  end for;
               finally
                  i
               end for;
            end block;
         else
            shortest-line-size
         end if;
   copy-sequence(lines.first, end: end-index)
end method;


define inline method spaces (n :: <integer>) => (s :: <string>)
   make(<string>, size: n)
end method;
