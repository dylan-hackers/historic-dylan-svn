module: calc
synopsis: 
author: 
copyright:

define function extract-action
    (token-string :: <byte-string>,
     token-start :: <integer>,
     token-end :: <integer>)
 => (result :: <byte-string>);
  let result = make(<byte-string>, size: token-end - token-start);
  copy-bytes(token-string, token-start, result, 0, token-end - token-start);
  result
end;

define constant $calc-tokens
  = simple-lexical-definition
      token EOF;
      
      // Whitespace and comments
      inert "([ \t\f]|/\\*([^*]|\\*+[^*/])*\\*+/)+";

      // New-line characters and line-end comments
      token NEW-LINE = "(//[^\r\n]*)?(\n|\r|\r\n)";
            
      // Identifiers
      token IDENTIFIER = "[a-zA-Z_][0-9a-zA-Z_]*",
        semantic-value-function: extract-action;
            
      // Numbers
      token NUMBER = "[0-9]+",
        semantic-value-function:
          method(string, token-start, token-end)
              string-to-integer(string, start: token-start, end: token-end);
          end;

      // Punctuation/operators
      token LPAREN = "\\(";
      token RPAREN = "\\)";
      token AMP = "&";
      token STAR = "\\*";
      token PLUS = "\\+";
      token MINUS = "-";
      token SLASH = "/";
      token PERCENT = "%";
      token EQUALS = "=";
    end;

define constant $calc-grammar-productions
 = simple-grammar-productions

     production primary-expression :: <integer> => [NUMBER] ()
       NUMBER;

     production primary-expression :: <integer>
         => [IDENTIFIER] (data :: <string-table>, srcloc)
       let value = element(data, IDENTIFIER, default: #f);
       if (value)
         value
       else
         error("%s: '%s' is not defined", srcloc, IDENTIFIER);
       end if;

     production primary-expression :: <integer>
         => [LPAREN expression RPAREN] (data)
       expression;

     production unary-expression :: <integer> => [primary-expression] (data)
       primary-expression;

     production unary-expression :: <integer>
         => [MINUS primary-expression] (data)
       -primary-expression;

     production multiplicative-expression :: <integer>
         => [unary-expression] (data)
       unary-expression;

     production multiplicative-expression :: <integer>
         => [multiplicative-expression STAR unary-expression] (data)
       multiplicative-expression * unary-expression;

     production multiplicative-expression :: <integer>
         => [multiplicative-expression SLASH unary-expression] (data)
       truncate/(multiplicative-expression, unary-expression);

     production multiplicative-expression :: <integer>
         => [multiplicative-expression PERCENT unary-expression] (data)
       remainder(multiplicative-expression,unary-expression);
            
     production additive-expression :: <integer>
         => [multiplicative-expression] (data)
       multiplicative-expression;

     production additive-expression :: <integer>
         => [additive-expression PLUS multiplicative-expression] ()
       additive-expression + multiplicative-expression;

     production additive-expression
       => [additive-expression MINUS multiplicative-expression], action:
       method(p :: <simple-parser>, data, s, e)
         p[0] - p[2]
       end;

     production AND-expression => [additive-expression], action:
       method(p :: <simple-parser>, data, s, e)
         p[0]
       end;
     production AND-expression => [AND-expression AMP additive-expression], action:
       method(p :: <simple-parser>, data, s, e)
         logand(p[0], p[2])
       end;
            
     production inclusive-OR-expression => [AND-expression], action:
       method(p :: <simple-parser>, data, s, e)
         p[0]
       end;
     production inclusive-OR-expression
       => [inclusive-OR-expression OR AND-expression], action:
       method(p :: <simple-parser>, data, s, e)
         logior(p[0], p[2])
       end;
            
     production expression => [inclusive-OR-expression], action:
       method(p :: <simple-parser>, data, s, e)
         p[0]
       end;
     production expression => [IDENTIFIER EQUALS expression], action:
       method(p :: <simple-parser>, data, s, e)
         data[p[0]] := p[2]
       end;

     production item => [expression] (data, srcloc)
       format-out("%s: result: %=\n", srcloc, expression);

     production list => [/* empty */];
     production list => [list item NEW-LINE];
   end;

define constant $calc-parser-automaton
  = simple-parser-automaton($calc-tokens, $calc-grammar-productions,
                            #[#"list"]);

define sideways method print-message
    (srcloc :: <file-source-location>, stream :: <stream>) => ();
  format(stream, "%s:", srcloc.source-file);
  if (srcloc.source-start-line = srcloc.source-end-line)
    if (srcloc.source-start-column = srcloc.source-end-column)
      format(stream, "%d.%d",
             srcloc.source-start-line, srcloc.source-start-column);
    else
      format(stream, "%d.%d-%d",
             srcloc.source-start-line, srcloc.source-start-column,
             srcloc.source-end-column);
    end if;
  else
    format(stream, "%d.%d-%d.%d",
           srcloc.source-start-line, srcloc.source-start-column,
           srcloc.source-end-line, srcloc.source-end-column);
  end if;
end;
          
define function main(name, arguments)
  let locator = as(<file-locator>, "/dev/stdin");
  let rangemap = make(<source-location-rangemap>);
  rangemap-add-line-file(rangemap, 0, 1, locator);

  let scanner
    = make(<simple-lexical-scanner>,
           definition: $calc-tokens, rangemap: rangemap);

  let stream = *standard-input*;

  let variables = make(<string-table>);

  let parser = make(<simple-parser>,
                    automaton: $calc-parser-automaton,
                    start-symbol: #"list",
                    rangemap: rangemap,
                    consumer-data: variables);

  let buf = get-input-buffer(stream);
  let text :: <byte-string>
    = make(<byte-string>, size: if (buf) buf.buffer-size else 0 end);
  iterate buf-loop (buf :: false-or(<buffer>) = buf)
    if (buf)
      let text-size :: <integer> = buf.buffer-end - buf.buffer-next;
      copy-bytes(buf, buf.buffer-next, text, 0, text-size);

      scan-tokens(scanner, simple-parser-consume-token, parser, text,
                  end: text-size, partial?: #t);
      
      buf.buffer-next := buf.buffer-end;
      buf-loop(next-input-buffer(stream));
    end if;
  end iterate;
  release-input-buffer(stream);

  scan-tokens(scanner, simple-parser-consume-token, parser, "", partial?: #f);
  let end-position = scanner.scanner-source-position;

  simple-parser-consume-token(parser, 0, #"EOF", parser,
                              end-position, end-position);
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
