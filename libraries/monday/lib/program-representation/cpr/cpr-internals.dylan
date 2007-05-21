Module: cpr-internals


define open class <C-preprocessor-dialect> (<object>)
  
  constant slot preprocessor-dialect-parent
      :: false-or(<C-preprocessor-dialect>) = #f,
    init-keyword: parent:;
  slot preprocessor-dialect-directives :: <hierarchical-table>;
            
  slot preprocessor-dialect-productions :: <sequence>,
    init-value: #[], init-keyword: productions:;
            
  slot %preprocessor-dialect-automaton;
            
end class;
                
define method initialize
    (dialect :: <C-preprocessor-dialect>,
     #key parent :: false-or(<C-preprocessor-dialect>), #all-keys)
 => ();
  next-method();
  dialect.preprocessor-dialect-directives
    := make(<hierarchical-table>,
            parent: parent & parent.preprocessor-dialect-directives);
end method;
            
define macro preprocessor-directive-productions
  { preprocessor-directive-productions ?clauses end }
    => { vector(?clauses) }

clauses:
  { } => { }

  { directive ?name:token [?symbols] (?variables:*)
      ?:body ... }
    => { make(<simple-production>,
              nonterminal: #"directive",
              derives: preprocessor-directive-derives(intern-string(?name),
                                                      ?symbols),
              action: production-user-reduce-action-function([];
                                                             [SHARP d ?symbols];
                                                             [?variables];
                                                             ?body)), ... }
symbols:
  { } => { }
  { ?symbol:name ... } => { ?symbol ... }
end macro;
            
define macro preprocessor-directive-derives
  { preprocessor-directive-derives(?name:expression, ?symbols) }
    => { vector(#"SHARP", ?name, ?symbols) }
symbols:
  { } => { }
  { ?symbol:name ... } => { ?#"symbol", ... }
end macro;
            
define constant $C90-C-preprocessor-dialect :: <C-preprocessor-dialect>
  = make(<C-preprocessor-dialect>);
                
define constant $C99-C-preprocessor-dialect :: <C-preprocessor-dialect>
  = make(<C-preprocessor-dialect>, parent: $C90-C-preprocessor-dialect);
                
define constant $MSVC6-C-preprocessor-dialect :: <C-preprocessor-dialect>
  = make(<C-preprocessor-dialect>, parent: $C90-C-preprocessor-dialect);
                
define class <C-preprocessing-translation-unit-representation> (<object>)
  constant slot preprocessing-header-search-path :: <deque>
    = make(<deque>);
  constant slot preprocessing-system-header-search-path :: <deque>
    = make(<deque>);
  
constant slot preprocessing-macro-definitions :: <object-table>
 = make(<object-table>);
            
end class;
                
define method preprocessor-define
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     identifier :: <byte-string>,
     replacement :: <byte-string>)
 => ();
  
let rangemap = make(<source-location-rangemap>);
let scanner
  = make(<simple-lexical-scanner>, definition: $C-tokens, rangemap: rangemap);
let tokens = make(<stretchy-object-vector>);
scan-tokens(scanner,
            method (tokens :: <stretchy-object-vector>,
                    token-number, token-name, token-value,
                    start-position, end-position)
              if (token-name ~== #"WHITESPACE" & token-name ~== #"NEW-LINE")
                add!(tokens,
                     make(<C-preprocessing-token>,
                          name: token-name,
                          value: token-value))
              end if;
            end,
            tokens,
            replacement);
let definition
  = make(<C-preprocessing-macro>,
           replacement: tokens);
preprocessor-add-macro-definition
  (preprocessing-translation-unit,
   intern-string(identifier),
   definition);
              
end method;
                
define method preprocessor-undefine
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     identifier :: <byte-string>)
 => ();
  
remove-key!(preprocessing-translation-unit.preprocessing-macro-definitions,
            intern-string(identifier));
              
end method;
                
define method preprocess-C-source-file
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     locator :: <file-locator>,
     consumer :: <function>,
     consumer-data,
     #key dialect = $C99-C-preprocessor-dialect)
 => ();
  
let rangemap = make(<source-location-rangemap>);
rangemap-add-line-file(rangemap, 0, 1, locator);
with-open-file(stream = locator)
  preprocess-C-stream(preprocessing-translation-unit, stream,
                      locator.locator-directory, rangemap,
                      consumer, consumer-data,
                      dialect: dialect);
end;
            
end method;
                
define method preprocess-C-header-file
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     locator :: <file-locator>,
     search-directory :: <directory-locator>,
     consumer :: <function>,
     consumer-data,
     #key dialect = $C99-C-preprocessor-dialect)
 => ();
  
do-with-C-header-file
  (locator, search-directory,
   preprocessing-translation-unit.preprocessing-header-search-path,
   method (found-locator :: <file-locator>, stream :: <stream>) => ();
     let rangemap = make(<source-location-rangemap>);
     rangemap-add-line-file(rangemap, 0, 1, found-locator);
     preprocess-C-stream(preprocessing-translation-unit,
                         stream, found-locator.locator-directory, rangemap,
                         consumer, consumer-data,
                         dialect: dialect);
   end);
            
end method;
                
define method preprocess-C-system-header-file
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     locator :: <file-locator>,
     consumer :: <function>,
     consumer-data,
     #key dialect = $C99-C-preprocessor-dialect)
 => ();
  
do-with-C-header-file
  (locator, #f,
   preprocessing-translation-unit.preprocessing-system-header-search-path,
   method (found-locator :: <file-locator>, stream :: <stream>) => ();
     let rangemap = make(<source-location-rangemap>);
     rangemap-add-line-file(rangemap, 0, 1, found-locator);
     preprocess-C-stream(preprocessing-translation-unit,
                         stream, found-locator.locator-directory, rangemap,
                         consumer, consumer-data,
                         dialect: dialect);
   end);
            
end method;
                
define generic preprocess-C-stream
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     stream :: <stream>,
     directory :: false-or(<directory-locator>),
     rangemap :: <source-location-rangemap>,
     consumer :: <function>,
     consumer-data,
     #key start-position,
          dialect = $C99-C-preprocessor-dialect)
 => (end-position :: <integer>);
                
define function preprocessor-token-string
    (token-name :: <symbol>, token-value :: <object>)
 => (string :: <byte-string>);
  select (token-name)
    
#"UNKNOWN-TOKEN" => token-value;
            
#"WHITESPACE" => " ";
            
#"IDENTIFIER" => token-value;
            
#"PP-NUMBER" => token-value;
            
#"CHARACTER-CONSTANT" => token-value;
#"STRING-LITERAL" => token-value;
            
#"SHARP" => if (token-value) "#" else "%:" end;
#"SHARPSHARP" => if (token-value) "##" else "%:%:" end;
#"LBRACK" => if (token-value) "[" else "<:" end;
#"RBRACK" => if (token-value) "]" else ":>" end;
#"LBRACE" => if (token-value) "{" else "<%" end;
#"RBRACE" => if (token-value) "}" else "%>" end;
#"LPAREN" => "(";
#"RPAREN" => ")";
#"DOT" => ".";
#"ARROW" => "->";
#"INC" => "++";
#"DEC" => "--";
#"AMP" => "&";
#"STAR" => "*";
#"PLUS" => "+";
#"MINUS" => "-";
#"TILDE" => "~";
#"BANG" => "!";
#"SLASH" => "/";
#"PERCENT" => "%";
#"SHL" => "<<";
#"SHR" => ">>";
#"LT" => "<";
#"GT" => ">";
#"LE" => "<=";
#"GE" => ">=";
#"EQ" => "==";
#"NE" => "!=";
#"HAT" => "^";
#"OR" => "|";
#"AMPAMP" => "&&";
#"OROR" => "||";
#"QUEST" => "?";
#"COLON" => ":";
#"SEMI" => ";";
#"ELIPSIS" => "...";
#"EQUALS" => "=";
#"STAR-EQUALS" => "*=";
#"SLASH-EQUALS" => "/=";
#"PERCENT-EQUALS" => "%=";
#"PLUS-EQUALS" => "+=";
#"MINUS-EQUALS" => "-=";
#"SHL-EQUALS" => "<<=";
#"SHR-EQUALS" => ">>=";
#"AMP-EQUALS" => "&=";
#"HAT-EQUALS" => "^=";
#"OR-EQUALS" => "|=";
#"COMMA" => ",";
            
  end select
end function;
                
define open class <C-language-dialect> (<object>)
  // 
end class;
                
define constant $C90-C-language-dialect :: <C-language-dialect>
  = make(<C-language-dialect>);
                
define constant $C99-C-language-dialect :: <C-language-dialect>
  = make(<C-language-dialect>);
                
define constant $MSVC6-C-language-dialect :: <C-language-dialect>
  = make(<C-language-dialect>);
                
define class <C-translation-unit-representation> (<object>)
  constant slot translation-unit-preprocessing-translation-unit
    :: <C-preprocessing-translation-unit-representation>,
    required-init-keyword: preprocessing-translation-unit:;
  
end class;
                
define method parse-C-source-file
    (translation-unit :: <C-translation-unit-representation>,
     locator :: <file-locator>)
 => ();
  
let rangemap = make(<source-location-rangemap>);
rangemap-add-line-file(rangemap, 0, 1, locator);
let parser = make(<simple-parser>,
                  automaton: $C99-parser-automaton,
                  start-symbol: #"translation-unit-opt",
                  rangemap: rangemap,
                  consumer-data: translation-unit);
let end-position = 
  with-open-file(stream = locator)
    preprocess-C-stream(translation-unit
                          .translation-unit-preprocessing-translation-unit,
                        stream, locator.locator-directory, rangemap,
                        C-screen-token, parser);
  end;
simple-parser-consume-token(parser, 0, #"EOF", #f, end-position, end-position)
          
end method;
                
define method parse-C-header-file
    (translation-unit :: <C-translation-unit-representation>,
     locator :: <file-locator>)
 => ();
  
end method;
                
define method parse-C-system-header-file
    (translation-unit :: <C-translation-unit-representation>,
     locator :: <file-locator>)
 => ();
  
end method;
                
define abstract class <C-type-representation> (<object>)
  
end class;
                
define class <C-integer-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-enum-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-float-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-array-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-function-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-struct/union-type-representation> (<C-type-representation>)
  
end class;
                
define class <C-pointer-type-representation> (<C-type-representation>)
  
end class;
                
define abstract class <C-expression-representation> (<source-location-mixin>)
  
end class;
                
define sealed domain initialize(<C-expression-representation>);
                
define generic expression-type
    (expression :: <C-expression-representation>)
 => (type :: <C-type-representation>);
                
define class <C-constant-expression-representation>
    (<C-expression-representation>)
  constant slot expression-value :: <object>,
    required-init-keyword: value:;
end class;
                
define sealed domain make(singleton(<C-constant-expression-representation>));
                
define class <C-string-literal-expression-representation>
    (<C-expression-representation>)
  constant slot expression-value :: <object>,
    required-init-keyword: value:;
end class;
                
define sealed domain make(singleton(<C-string-literal-expression-representation>));
                
define class <C-variable-reference-expression-representation>
    (<C-expression-representation>)
  constant slot expression-variable :: <string>, // FIXME
    required-init-keyword: variable:;
end class;
                
define sealed domain make(singleton(<C-variable-reference-expression-representation>));
                
define class <C-function-reference-expression-representation>
    (<C-expression-representation>)
  //
end class;
                
define sealed domain make(singleton(<C-function-reference-expression-representation>));
                
define constant <C-unary-expression-operator>
  = one-of(#"POSTINC", #"POSTDEC", #"PREINC", #"PREDEC", 
           #"ADDROF", #"DEREF", 
           #"PLUS", #"MINUS", #"BITWISE-NOT", #"LOGICAL-NOT",
           #"SIZEOF", #"ALIGNOF");
                
define class <C-unary-expression-representation>
    (<C-expression-representation>)
  slot expression-type :: <C-type-representation>,
    init-keyword: type:;
  constant slot expression-operator :: <C-unary-expression-operator>,
    required-init-keyword: operator:;
  constant slot expression-unary-operand :: <C-expression-representation>,
    required-init-keyword: operand:;
end class;
                
define sealed domain make(singleton(<C-unary-expression-representation>));
                
define constant <C-binary-expression-operator>
  = one-of(#"MUL", #"DIV", #"MOD", #"ADD", #"SUB",
           #"SHL", #"SHR",
           #"LT", #"LE", #"GT", #"GE", #"EQ", #"NE",
           #"BITWISE-AND", #"BITWISE-XOR", #"BITWISE-OR",
           #"LOGICAL-AND", #"LOGICAL-OR",
           #"ASSIGN", #"COMMA");
                
define class <C-binary-expression-representation>
    (<C-expression-representation>)
  slot expression-type :: <C-type-representation>,
    init-keyword: type:;
  constant slot expression-operator :: <C-binary-expression-operator>,
    required-init-keyword: operator:;
  constant slot expression-binary-right :: <C-expression-representation>,
    required-init-keyword: right:;
  constant slot expression-binary-left :: <C-expression-representation>,
    required-init-keyword: left:;
end class;
                
define sealed domain make(singleton(<C-binary-expression-representation>));
                
define class <C-conditional-expression-representation>
    (<C-expression-representation>)
  constant slot expression-conditional-condition :: <C-expression-representation>,
    required-init-keyword: condition:;
  constant slot expression-conditional-true :: <C-expression-representation>,
    required-init-keyword: true:;
  constant slot expression-conditional-false :: <C-expression-representation>,
    required-init-keyword: false:;
end class;
                
define sealed domain make(singleton(<C-conditional-expression-representation>));
                
define class <C-cast-expression-representation>
    (<C-expression-representation>)
  slot expression-type :: <C-type-representation>,
    init-keyword: type:;
  constant slot expression-cast-operand :: <C-expression-representation>,
    required-init-keyword: operand:;
end class;
                
define sealed domain make(singleton(<C-cast-expression-representation>));
                
define class <C-sizeof-type-expression-representation>
    (<C-expression-representation>)
  constant slot expression-sizeof-type :: <C-type-representation>,
    required-init-keyword: sizeof-type:;
end class;
                
define sealed domain make(singleton(<C-sizeof-type-expression-representation>));
                
define class <C-function-call-expression-representation>
    (<C-expression-representation>)
  constant slot expression-call-function :: <C-expression-representation>,
    required-init-keyword: function:;
  constant slot expression-call-arguments :: <sequence>,
    required-init-keyword: arguments:;
end class;
                
define sealed domain make(singleton(<C-function-call-expression-representation>));
                
define class <C-member-expression-representation>
    (<C-expression-representation>)
  constant slot expression-member-operand :: <C-expression-representation>,
    required-init-keyword: operand:;
  constant slot expression-member-name :: <string>,
    required-init-keyword: name:;
end class;
                
define sealed domain make(singleton(<C-member-expression-representation>));
                
define generic print-C-expression
    (expression :: <C-expression-representation>,
     stream :: <stream>,
     #key level)
 => ();
                
define constant $precedence-level-assignment-expression :: <integer>
  = 15;
                
define constant $precedence-level-expression :: <integer>
  = 16;
                
define method do-with-C-header-file
    (locator :: <file-locator>,
     search-directory :: false-or(<directory-locator>),
     search-path :: <sequence>,
     function :: <function>)
  let found-locator
    = if (~locator-relative?(locator))
        locator
      elseif (search-directory
                & file-exists?(merge-locators(locator, search-directory)))
        merge-locators(locator, search-directory)
      else
        block (found)
          for (directory in search-path)
            let merged = merge-locators(locator, directory);
            if (file-exists?(merged)) found(merged) end if;
          finally
            locator
          end for
        end block
      end if;
  let stream
    = make(<file-stream>, direction: #"input", locator: found-locator);
  block ()
    function(found-locator, stream);
  cleanup
    close(stream);
  end block;
end method;
            
define method preprocess-C-stream
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     stream :: <buffered-stream>,
     directory :: false-or(<directory-locator>),
     rangemap :: <source-location-rangemap>,
     consumer :: <function>,
     consumer-data,
     #key start-position :: <integer> = 0,
          dialect :: <C-preprocessor-dialect> = $C99-C-preprocessor-dialect)
 => (end-position :: <integer>);
  
let splice-state :: one-of(#f, #"backslash", #"backslash-cr") = #f;
            
  
let scanner
  = make(<simple-lexical-scanner>, definition: $C-tokens, rangemap: rangemap);
scanner.scanner-source-position := start-position;
            
  
let preprocessor-state
  = make(<C-preprocessor-state>,
         dialect: dialect,
         consumer: consumer,
         consumer-data: consumer-data);
let preprocessor-token-dispatcher
  = make(<C-preprocessor-token-dispatcher>,
         preprocessing-translation-unit: preprocessing-translation-unit,
         scanner: scanner,
         directory: directory,
         rangemap: rangemap,
         consumer-state-function: C-preprocess-token-linestart,
         consumer-state: preprocessor-state);
            
preprocessor-state.preprocessor-directive-parser
  := make(<simple-parser>,
          automaton: dialect.preprocessor-dialect-automaton,
          start-symbol: #"directive",
          rangemap: rangemap,
          consumer-data: preprocessor-token-dispatcher);
            
  let buf = get-input-buffer(stream);
  let text :: <byte-string>
    = make(<byte-string>, size: if (buf) buf.buffer-size else 0 end);
  iterate buf-loop (buf :: false-or(<buffer>) = buf)
    if (buf)
      let text-size :: <integer> = buf.buffer-end - buf.buffer-next;
      copy-bytes(text, 0, buf, buf.buffer-next, text-size);
      
iterate char-loop (start :: <buffer-index> = 0,
                   i :: <buffer-index> = 0)
  if (i < text-size)
    let c :: <byte-character> = as(<character>, text[i]);
    if (c == '\\')
      splice-state := #"backslash";
      char-loop(start, i + 1);
    elseif (splice-state == #f)
      char-loop(start, i + 1);
    elseif (splice-state == #"backslash")
      if (c == '\r')
        scan-C-preprocessing-tokens(preprocessor-token-dispatcher, scanner,
                                    text, start, i - 1);
        splice-state := #"backslash-cr";
        char-loop(i + 1, i + 1);
      elseif (c == '\n')
        if (i > start)
          scan-C-preprocessing-tokens(preprocessor-token-dispatcher, scanner,
                                      text, start, i - 1);
        end if;
        rangemap-add-line(rangemap, scanner.scanner-source-position, #f);
        splice-state := #f;
        char-loop(i + 1, i + 1);
      elseif (i = 0)
        scan-C-preprocessing-tokens(preprocessor-token-dispatcher, scanner,
                                    "\\", 0, 1);
        if (c ~== '\\')
          splice-state := #f;
        end if;
        char-loop(i, i + 1);
      else
        if (c ~== '\\')
          splice-state := #f;
        end if;
        char-loop(start, i + 1);  
      end if;
    else // (splice-state == #"backslash-cr")
      splice-state := #f;
      rangemap-add-line(rangemap, scanner.scanner-source-position, #f);
      if (c == '\n')
        char-loop(i + 1, i + 1);
      else
        if (c == '\\')
          splice-state := #"backslash";
        end if;
        char-loop(i, i + 1);
      end if;
    end if;
  else
    scan-C-preprocessing-tokens
      (preprocessor-token-dispatcher, scanner, text, start,
       if (splice-state == #"backslash") i - 1 else i end);
  end if;
end iterate;        
            
      buf.buffer-next := buf.buffer-end;
      buf-loop(next-input-buffer(stream));
    end if;
  end;
  release-input-buffer(stream);
  
scan-tokens(scanner, C-preprocessor-dispatch-token, preprocessor-token-dispatcher,
            "", partial?: #f);
let end-position = scanner.scanner-source-position;
C-preprocessor-dispatch-token(preprocessor-token-dispatcher, 0, #"EOF", #f,
                              end-position, end-position);

if (preprocessor-state.preprocessor-skipping?-stack.size ~= 0)
  preprocessor-error(preprocessor-token-dispatcher, end-position, end-position,
                     "Conditional directive left unterminated at end-of-file");
end if;
            
end-position
            
end method;
            
define function extract-action
    (token-string :: <byte-string>,
     token-start :: <integer>,
     token-end :: <integer>)
 => (result :: <byte-string>);
  let result = make(<byte-string>, size: token-end - token-start);
  copy-bytes(result, 0, token-string, token-start, token-end - token-start);
  result
end;
            
define function punctuation-value-function
    (length :: <integer>) => (func :: <function>);
  method
      (token-string :: <byte-string>,
       token-start :: <integer>,
       token-end :: <integer>)
   => (result :: <boolean>);
    token-end - token-start = length
  end
end function;
            
define constant $C-tokens
  = simple-lexical-definition
      token EOF;
      
// Whitespace and comments
token WHITESPACE = "([ \t\f]|/\\*([^*]|\\*+[^*/])*\\*+/)+";

// New-line characters and line-end comments
token NEW-LINE = "(//[^\r\n]*)?(\n|\r|\r\n)";
            
// Identifiers
token IDENTIFIER :: <string> = "[a-zA-Z_][0-9a-zA-Z_]*",
  semantic-value-function:
    method (token-string :: <byte-string>,
            token-start :: <integer>,
            token-end :: <integer>);
      intern-string(token-string, start: token-start, end: token-end)
    end;
            
// Preprocessing numbers
token PP-NUMBER = "\\.?[0-9]([.0-9a-zA-Z_]|[eEpP][-+])*",
  semantic-value-function: extract-action;
            
// Character constants
name c-char
  = "[^'\\\\\r\n]|\\\\(['\"?\\abfnrtv]|[0-7]([0-7][0-7]?)?|x[0-9a-fA-F]+)";
token CHARACTER-CONSTANT :: <string> = "L?'{c-char}+'", // FIXME
  semantic-value-function: extract-action;

// String literals
name s-char
  = "[^\"\\\\\r\n]|\\\\(['\"?\\abfnrtv]|[0-7]([0-7][0-7]?)?|x[0-9a-fA-F]+)";
token STRING-LITERAL :: <string> = "L?\"{s-char}*\"", // FIXME
  semantic-value-function: extract-action;
            
// Punctuation
token SHARP = "#|%:",
  semantic-value-function: punctuation-value-function(1);
token SHARPSHARP = "##|%:%:",
  semantic-value-function: punctuation-value-function(2);
token LBRACK = "\\[|<:",
  semantic-value-function: punctuation-value-function(1);
token RBRACK = "\\]|:>",
  semantic-value-function: punctuation-value-function(1);
token LBRACE = "\\{|<%",
  semantic-value-function: punctuation-value-function(1);
token RBRACE = "\\}|%>",
  semantic-value-function: punctuation-value-function(1);
token LPAREN = "\\(";
token RPAREN = "\\)";
token DOT = "\\.";
token ARROW = "->";
token INC = "\\+\\+";
token DEC = "-" "-";
token AMP = "&";
token STAR = "\\*";
token PLUS = "\\+";
token MINUS = "-";
token TILDE = "~";
token BANG = "!";
token SLASH = "/";
token PERCENT = "%";
token SHL = "<<";
token SHR = ">>";
token LT = "<";
token GT = ">";
token LE = "<=";
token GE = ">=";
token EQ = "==";
token NE = "!=";
token HAT = "^";
token OR = "\\|";
token AMPAMP = "&&";
token OROR = "\\|\\|";
token QUEST = "\\?";
token COLON = ":";
token SEMI = ";";
token ELIPSIS = "\\.\\.\\.";
token EQUALS = "=";
token STAR-EQUALS = "\\*=";
token SLASH-EQUALS = "/=";
token PERCENT-EQUALS = "%=";
token PLUS-EQUALS = "\\+=";
token MINUS-EQUALS = "-=";
token SHL-EQUALS = "<<=";
token SHR-EQUALS = ">>=";
token AMP-EQUALS = "&=";
token HAT-EQUALS = "^=";
token OR-EQUALS = "\\|=";
token COMMA = ",";
            
token DEFINED;
            
token AUTO;
token BREAK;
token _CASE;
token CHAR;
token CONST;
token CONTINUE;
token DEFAULT;
token DO;
token DOUBLE;
token _ELSE;
token ENUM;
token EXTERN;
token FLOAT;
token _FOR;
token GOTO;
token _IF;
token INLINE;
token INT;
token LONG;
token REGISTER;
token RESTRICT;
token RETURN;
token SHORT;
token SIGNED;
token SIZEOF;
token STATIC;
token STRUCT;
token SWITCH;
token TYPEDEF;
token UNION;
token UNSIGNED;
token VOID;
token VOLATILE;
token _WHILE;
token BOOL;
token COMPLEX;
token IMAGINARY;
            
token TYPEDEF-NAME :: <string>; // FIXME
token INTEGER-CONSTANT :: <integer>;
token FLOATING-CONSTANT :: <string>; // FIXME
            
      token UNKNOWN-TOKEN = ".",
        priority: -1,
        semantic-value-function: extract-action;
    end;
            
define function scan-C-preprocessing-tokens
    (preprocessor-token-dispatcher :: <C-preprocessor-token-dispatcher>,
     scanner :: <simple-lexical-scanner>,
     text :: <byte-string>,
     start :: <buffer-index>,
     _end :: <buffer-index>)
 => ();
  scan-tokens(scanner, C-preprocessor-dispatch-token, preprocessor-token-dispatcher,
              text, start: start, end: _end, partial?: #t);
end function;
            
define class <C-preprocessor-token-dispatcher> (<object>)
  constant slot token-dispatcher-preprocessing-translation-unit
      :: <C-preprocessing-translation-unit-representation>,
    required-init-keyword: preprocessing-translation-unit:;
  constant slot token-dispatcher-scanner :: <simple-lexical-scanner>,
    required-init-keyword: scanner:;
  constant slot token-dispatcher-directory :: false-or(<directory-locator>),
    required-init-keyword: directory:;
  constant slot token-dispatcher-rangemap :: <source-location-rangemap>,
    required-init-keyword: rangemap:;
  
slot token-dispatcher-state-function :: <function>,
  required-init-keyword: consumer-state-function:;
            
constant slot token-dispatcher-state :: <C-preprocessor-state>,
  required-init-keyword: consumer-state:;
            
slot token-dispatcher-macro-invocation
    :: false-or(<C-preprocessing-macro-invocation>),
  init-value: #f;
            
end class;
            
define sealed domain make(singleton(<C-preprocessor-token-dispatcher>));
            
define sealed domain initialize(<C-preprocessor-token-dispatcher>);
            
define function C-preprocessor-dispatch-token
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name, token-value, start-position, end-position)
 => ();
  token-dispatcher.token-dispatcher-state-function
    (token-dispatcher, token-number, token-name, token-value,
     start-position, end-position);
end function;
            
define macro preprocessor-state-function-definer
  { define preprocessor-state-function ?state:name
        (?token-dispatcher:variable,
         ?token-number:variable, ?token-name:variable, ?token-value:variable,
         ?start-position:variable, ?end-position:variable)
      ?:body
    end }
    => { define function "C-preprocess-token-" ## ?state
             (?token-dispatcher, ?token-number, ?token-name, ?token-value,
              ?start-position, ?end-position)
          => ();
           ?body
         end }
end macro;
            
define macro enter-state
  { enter-state(?token-dispatcher:expression, ?state:name) }
  => { ?token-dispatcher.token-dispatcher-state-function
         := "C-preprocess-token-" ## ?state }
end macro;
            
define function preprocessor-error
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     start-position :: <integer>, end-position :: <integer>,
     string :: <string>, #rest arguments)
 => (no-return :: <bottom>);
  let srcloc
    = range-source-location(token-dispatcher.token-dispatcher-rangemap,
                            start-position, end-position);
  apply(source-error, srcloc, string, arguments);
end function;
            
define class <C-preprocessor-state> (<object>)
  constant slot preprocessor-dialect :: <C-preprocessor-dialect>,
    required-init-keyword: dialect:;
  constant slot preprocessor-consumer :: <function>,
    required-init-keyword: consumer:;
  constant slot preprocessor-consumer-data,
    required-init-keyword: consumer-data:;
  slot preprocessor-skipping? :: <boolean>,
    init-value: #f, init-keyword: skipping?:;
  
slot preprocessor-directive-parser :: <simple-parser>,
  init-keyword: directive-parser:;
            
constant slot preprocessor-skipping?-stack :: <stretchy-object-vector>
  = make(<stretchy-object-vector>);
            
constant slot preprocessor-true-seen?-stack :: <stretchy-object-vector>
  = make(<stretchy-object-vector>);
constant slot preprocessor-else-seen?-stack :: <stretchy-object-vector>
  = make(<stretchy-object-vector>);
            
end class;
            
define sealed domain make(singleton(<C-preprocessor-state>));
            
define sealed domain initialize(<C-preprocessor-state>);
            
define class <C-preprocessor-directive> (<object>)
  constant slot directive-skipping-function :: <function>,
    init-value: method (dispatcher) #f end, init-keyword: skipping-function:;
  constant slot directive-macro-replaced?,
    init-value: #f, init-keyword: macro-replaced?:;
  constant slot directive-lexical-definition
      :: false-or(<simple-lexical-definition>),
    init-value: #f, init-keyword: lexical-definition:;
end class;
            
define sealed domain make(singleton(<C-preprocessor-directive>));
            
define sealed domain initialize(<C-preprocessor-directive>);
            
define method define-C-preprocessor-directive
    (dialect :: <C-preprocessor-dialect>,
     directive :: <string>,
     #rest keys)
 => ();
  dialect.preprocessor-dialect-directives[intern-string(directive)]
    := apply(make, <C-preprocessor-directive>, keys);
end method;
            
define method preprocessor-dialect-automaton
   (dialect :: <C-preprocessor-dialect>)
=> (automaton);
  if (slot-initialized?(dialect, %preprocessor-dialect-automaton))
    dialect.%preprocessor-dialect-automaton
  else
    for (the-dialect = dialect
           then the-dialect.preprocessor-dialect-parent,
         productions = #[]
           then concatenate(productions,
                            the-dialect.preprocessor-dialect-productions),
         while: the-dialect)
    finally
      dialect.%preprocessor-dialect-automaton
        := simple-parser-automaton($C-tokens, productions,
                                   #[#"directive"],
                                   end-symbol: #"NEW-LINE")
    end for
  end if
end;
            
$C90-C-preprocessor-dialect.preprocessor-dialect-productions
 := concatenate(preprocessor-directive-productions
                  
directive "define" [IDENTIFIER]
    (dispatcher :: <C-preprocessor-token-dispatcher>,
     srcloc :: <source-location>)
  let definition
    = make(<C-preprocessing-macro>,
           source-location: srcloc,
           replacement: #[]);
  preprocessor-add-macro-definition
    (dispatcher.token-dispatcher-preprocessing-translation-unit,
     IDENTIFIER,
     definition);

directive "define" [IDENTIFIER WHITESPACE pp-tokens-opt]
    (dispatcher :: <C-preprocessor-token-dispatcher>,
     srcloc :: <source-location>)
  let definition
    = make(<C-preprocessing-macro>,
           source-location: srcloc,
           replacement: pp-tokens-opt);
  preprocessor-add-macro-definition
    (dispatcher.token-dispatcher-preprocessing-translation-unit,
     IDENTIFIER,
     definition);

directive "define" [IDENTIFIER LPAREN identifier-list-opt RPAREN
                    pp-tokens-opt]
    (dispatcher :: <C-preprocessor-token-dispatcher>,
     srcloc :: <source-location>)
  let definition
    = make(<C-preprocessing-macro>,
           source-location: srcloc,
           parameters: identifier-list-opt,
           replacement: pp-tokens-opt);
  preprocessor-add-macro-definition
    (dispatcher.token-dispatcher-preprocessing-translation-unit,
     IDENTIFIER,
     definition);
              
directive "undef" [IDENTIFIER] (dispatcher :: <C-preprocessor-token-dispatcher>)
  let pp-translation-unit
    = dispatcher.token-dispatcher-preprocessing-translation-unit;
  remove-key!(pp-translation-unit.preprocessing-macro-definitions, IDENTIFIER);
              
directive "ifdef" [IDENTIFIER] (dispatcher :: <C-preprocessor-token-dispatcher>)
  let preprocessing-translation-unit
    = dispatcher.token-dispatcher-preprocessing-translation-unit;
  let definition
    = element(preprocessing-translation-unit.preprocessing-macro-definitions,
              IDENTIFIER,
              default: #f);
  preprocessor-begin-conditional(dispatcher, definition ~== #f);
              
directive "ifndef" [IDENTIFIER] (dispatcher :: <C-preprocessor-token-dispatcher>)
  let preprocessing-translation-unit
    = dispatcher.token-dispatcher-preprocessing-translation-unit;
  let definition
    = element(preprocessing-translation-unit.preprocessing-macro-definitions,
              IDENTIFIER,
              default: #f);
  preprocessor-begin-conditional(dispatcher, ~definition);
              
directive "if" [constant-expression] (dispatcher :: <C-preprocessor-token-dispatcher>)
  preprocessor-begin-conditional(dispatcher, ~zero?(constant-expression()));
              
directive "elif" [constant-expression] (dispatcher :: <C-preprocessor-token-dispatcher>, srcloc :: <source-location>)
  let preprocessor-state = dispatcher.token-dispatcher-state;
  let index = preprocessor-state.preprocessor-skipping?-stack.size - 1;
  if (index >= 0)
    if (preprocessor-state.preprocessor-else-seen?-stack[index])
      source-error(srcloc, "#else has already appeared in this if-section");
    end if;
    let flag
      = ~zero?(constant-expression())
      & ~preprocessor-state.preprocessor-true-seen?-stack[index];
    preprocessor-state.preprocessor-skipping?-stack[index] := ~flag;
    preprocessor-state.preprocessor-skipping? := ~flag;
    if (flag)
      preprocessor-state.preprocessor-true-seen?-stack[index] := #t;
    end if;
  else
    source-error(srcloc, "#elif outside of #ifdef/#ifndef/#if");
  end if;
              
directive "else" [] (dispatcher :: <C-preprocessor-token-dispatcher>, srcloc :: <source-location>)
  let preprocessor-state = dispatcher.token-dispatcher-state;
  let index = preprocessor-state.preprocessor-skipping?-stack.size - 1;
  if (index >= 0)
    if (preprocessor-state.preprocessor-else-seen?-stack[index])
      source-error(srcloc, "#else has already appeared in this if-section");
    end if;
    let skipping = preprocessor-state.preprocessor-true-seen?-stack[index];
    preprocessor-state.preprocessor-skipping?-stack[index] := skipping;
    preprocessor-state.preprocessor-skipping? := skipping;
    if (~skipping)
      preprocessor-state.preprocessor-true-seen?-stack[index] := #t;
    end if;
    preprocessor-state.preprocessor-else-seen?-stack[index] := #t;
  else
    source-error(srcloc, "#else outside of #ifdef/#ifndef/#if");
  end if;
              
directive "endif" [] (dispatcher :: <C-preprocessor-token-dispatcher>, srcloc :: <source-location>)
  let preprocessor-state = dispatcher.token-dispatcher-state;
  let index = preprocessor-state.preprocessor-skipping?-stack.size - 1;
  if (index >= 0)
    preprocessor-state.preprocessor-skipping?-stack.size := index;
    preprocessor-state.preprocessor-true-seen?-stack.size := index;
    preprocessor-state.preprocessor-else-seen?-stack.size := index;
    if (index = 0)
      preprocessor-state.preprocessor-skipping? := #f;
    else
      preprocessor-state.preprocessor-skipping?
        := preprocessor-state.preprocessor-skipping?-stack[index - 1];
    end if;
  else
    source-error(srcloc, "#endif outside of #ifdef/#ifndef/#if");
  end if;
              
directive "include" [HEADER-NAME] (dispatcher :: <C-preprocessor-token-dispatcher>)
  do-include(dispatcher, #"HEADER-NAME", HEADER-NAME);
              
directive "include" [SOURCE-NAME] (dispatcher :: <C-preprocessor-token-dispatcher>)
  do-include(dispatcher, #"SOURCE-NAME", SOURCE-NAME);
              
directive "include" [STRING-LITERAL] (dispatcher :: <C-preprocessor-token-dispatcher>)
  let filename // FIXME
    = copy-sequence(STRING-LITERAL, start: 1, end: STRING-LITERAL.size - 1);
  do-include(dispatcher, #"SOURCE-NAME", as(<file-locator>, filename));
              
directive "line" [PP-NUMBER] (dispatcher :: <C-preprocessor-token-dispatcher>, srcloc :: <source-location>)
  let line = string-to-integer(PP-NUMBER);
  if (line <= 0)
    source-error(srcloc, "#line number must be 1 or greater")
  end if;
  let here = dispatcher.token-dispatcher-scanner.scanner-source-position;
  rangemap-add-line(dispatcher.token-dispatcher-rangemap, here, line);
              
directive "line" [PP-NUMBER STRING-LITERAL] (dispatcher :: <C-preprocessor-token-dispatcher>, srcloc :: <source-location>)
  let line = string-to-integer(PP-NUMBER);
  let filename // FIXME
    = copy-sequence(STRING-LITERAL, start: 1, end: STRING-LITERAL.size - 1);
  if (line <= 0)
    source-error(srcloc, "#line number must be 1 or greater")
  end if;
  let here = dispatcher.token-dispatcher-scanner.scanner-source-position;
  rangemap-add-line-file(dispatcher.token-dispatcher-rangemap, here,
                         line, as(<file-locator>, filename));
              
directive "error" [pp-tokens-opt] (dispatcher, srcloc :: <source-location>)
  let string
    = with-output-to-string (str)
        for (token :: <C-preprocessing-token> in pp-tokens-opt)
           write-element(str, ' ');
           write(str,
                 preprocessor-token-string(token.preprocessing-token-name,
                                           token.preprocessing-token-value));
        end for;
      end;
  source-error(srcloc, "#error%s", string);
              
directive "pragma" [pp-tokens-opt] ()
  #f;
              
                end,
                simple-grammar-productions
                  
production identifier-list-opt :: <sequence> => [/* empty */] () #[];
production identifier-list-opt :: <sequence> => [identifier-list];

production identifier-list => [IDENTIFIER];
production identifier-list => [identifier-list COMMA IDENTIFIER];

production pp-tokens-opt :: <sequence> => [/* empty */] () #[];
production pp-tokens-opt :: <sequence> => [pp-tokens];

production pp-tokens => [preprocessing-token];
production pp-tokens => [pp-tokens preprocessing-token];
              
production primary-expression :: <function>
  => [IDENTIFIER] ()
  method() 0 end;
              
production primary-expression :: <function>
  => [PP-NUMBER] ()
  always(string-to-integer(PP-NUMBER));
production primary-expression :: <function>
  => [CHARACTER-CONSTANT] ()
  error("CHARACTER-CONSTANT");
              
production primary-expression :: <function>
  => [LPAREN constant-expression RPAREN];
              
production unary-expression :: <function> => [primary-expression];
production unary-expression :: <function>
  => [PLUS unary-expression] ()
  unary-expression;
production unary-expression :: <function>
  => [MINUS unary-expression] ()
  method() negative(unary-expression()) end;
production unary-expression :: <function>
  => [TILDE unary-expression] ()
  method () lognot(unary-expression()) end;
production unary-expression :: <function>
  => [BANG unary-expression] ()
  method ()
    if (zero?(unary-expression())) 1 else 0 end
  end;
production unary-expression :: <function>
  => [DEFINED IDENTIFIER] (dispatcher  :: <C-preprocessor-token-dispatcher>)
  let preprocessing-translation-unit
    = dispatcher.token-dispatcher-preprocessing-translation-unit;
  let definition
    = element(preprocessing-translation-unit.preprocessing-macro-definitions,
              IDENTIFIER,
              default: #f);
  always(if (definition) 1 else 0 end);
production unary-expression :: <function>
  => [DEFINED LPAREN IDENTIFIER RPAREN] (dispatcher  :: <C-preprocessor-token-dispatcher>)
  let preprocessing-translation-unit
    = dispatcher.token-dispatcher-preprocessing-translation-unit;
  let definition
    = element(preprocessing-translation-unit.preprocessing-macro-definitions,
              IDENTIFIER,
              default: #f);
  always(if (definition) 1 else 0 end);
              
production multiplicative-expression :: <function> => [unary-expression];
production multiplicative-expression :: <function>
  => [multiplicative-expression STAR unary-expression] ()
  method ()
    multiplicative-expression() * unary-expression()
  end;
production multiplicative-expression :: <function>
  => [multiplicative-expression SLASH unary-expression] (dispatcher, srcloc :: <source-location>)
  method ()
    let divisor = unary-expression();
    if (zero?(divisor)) source-error(srcloc, "division by zero") end if;
    truncate/(multiplicative-expression(), divisor)
  end;
production multiplicative-expression :: <function>
  => [multiplicative-expression PERCENT unary-expression] (dispatcher, srcloc :: <source-location>)
  method ()
    let divisor = unary-expression();
    if (zero?(divisor)) source-error(srcloc, "division by zero") end if;
    remainder(multiplicative-expression(), divisor)
  end;
              
production additive-expression :: <function> => [multiplicative-expression];
production additive-expression :: <function>
  => [additive-expression PLUS multiplicative-expression] ()
  method ()
    additive-expression() + multiplicative-expression()
  end;
production additive-expression :: <function>
  => [additive-expression MINUS multiplicative-expression] ()
  method ()
    additive-expression() - multiplicative-expression()
  end;
              
production shift-expression :: <function> => [additive-expression];
production shift-expression :: <function>
  => [shift-expression SHL additive-expression] ()
  method()
    ash(shift-expression(), additive-expression())
  end;
production shift-expression :: <function>
  => [shift-expression SHR additive-expression] ()
  method ()
    ash(shift-expression(), -additive-expression())
  end;
              
production relational-expression :: <function> => [shift-expression];
production relational-expression :: <function>
  => [relational-expression LT shift-expression] ()
  method ()
    if (relational-expression() < shift-expression()) 1 else 0 end
  end;
production relational-expression :: <function>
  => [relational-expression GT shift-expression] ()
  method ()
    if (relational-expression() > shift-expression()) 1 else 0 end
  end;
production relational-expression :: <function>
  => [relational-expression LE shift-expression] ()
  method ()
    if (relational-expression() <= shift-expression()) 1 else 0 end
  end;
              
production relational-expression :: <function>
  => [relational-expression GE shift-expression] ()
  method ()
    if (relational-expression() <= shift-expression()) 1 else 0 end
  end;
              
production equality-expression :: <function> => [relational-expression];
production equality-expression :: <function>
  => [equality-expression EQ relational-expression] ()
  method ()
    if (equality-expression() = relational-expression()) 1 else 0 end
  end;
production equality-expression :: <function>
  => [equality-expression NE relational-expression] ()
  method ()
    if (equality-expression() ~= relational-expression()) 1 else 0 end
  end;
              
production AND-expression :: <function> => [equality-expression];
production AND-expression :: <function>
  => [AND-expression AMP equality-expression] ()
  method ()
    logand(AND-expression(), equality-expression())
  end;
              
production exclusive-OR-expression :: <function> => [AND-expression];
production exclusive-OR-expression :: <function>
  => [exclusive-OR-expression HAT AND-expression] ()
  method ()
    logxor(exclusive-OR-expression(), AND-expression())
  end;
              
production inclusive-OR-expression :: <function> => [exclusive-OR-expression];
production inclusive-OR-expression :: <function>
  => [inclusive-OR-expression OR exclusive-OR-expression] ()
  method ()
    logior(inclusive-OR-expression(), exclusive-OR-expression())
  end;
              
production logical-AND-expression :: <function> => [inclusive-OR-expression];
production logical-AND-expression :: <function>
  => [logical-AND-expression AMPAMP inclusive-OR-expression] ()
  method ()
    if (~zero?(logical-AND-expression()) & ~zero?(inclusive-OR-expression()))
      1
    else
      0
    end
  end;
              
production logical-OR-expression :: <function> => [logical-AND-expression];
production logical-OR-expression :: <function>
  => [logical-OR-expression OROR logical-AND-expression] ()
  method ()
    if (~zero?(logical-OR-expression()) | ~zero?(logical-AND-expression()))
      1
    else
      0
    end
  end;
              
production conditional-expression :: <function> => [logical-OR-expression];
production conditional-expression :: <function>
  => [logical-OR-expression QUEST constant-expression
                            COLON conditional-expression] ()
  method ()
    if (~zero?(logical-OR-expression()))
      constant-expression()
    else
      conditional-expression()
    end
  end;
              
production constant-expression :: <function> => [conditional-expression];
production constant-expression :: <function>
  => [constant-expression COMMA conditional-expression] ()
  conditional-expression;
              
                end,
                
map(method (token-name :: <symbol>)
      make(<simple-production>,
           nonterminal: #"preprocessing-token",
           nonterminal-type: <C-preprocessing-token>,
           derives: vector(token-name),
           action:
             method
                 (p :: <simple-parser>, data, s, e)
              => (token :: <C-preprocessing-token>);
               make(<C-preprocessing-token>,
                    name: token-name,
                    value: p[0])
             end)
    end,
    #[#"IDENTIFIER", #"PP-NUMBER", #"CHARACTER-CONSTANT", #"STRING-LITERAL",
      #"SHARP", #"SHARPSHARP", #"LBRACK", #"RBRACK", #"LBRACE", #"RBRACE",
      #"LPAREN", #"RPAREN", #"DOT", #"ARROW", #"INC", #"DEC", #"AMP", #"STAR",
      #"PLUS", #"MINUS", #"TILDE", #"BANG", #"SLASH", #"PERCENT",
      #"SHL", #"SHR", #"LT", #"GT", #"LE", #"GE", #"EQ", #"NE", #"HAT", #"OR",
      #"AMPAMP", #"OROR", #"QUEST", #"COLON", #"SEMI", #"ELIPSIS",
      #"EQUALS", #"STAR-EQUALS", #"SLASH-EQUALS", #"PERCENT-EQUALS",
      #"PLUS-EQUALS", #"MINUS-EQUALS", #"SHL-EQUALS", #"SHR-EQUALS",
      #"AMP-EQUALS", #"HAT-EQUALS", #"OR-EQUALS", #"COMMA"])
              );
            
define preprocessor-state-function linestart
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  select (token-name)
    #"NEW-LINE", #"WHITESPACE" =>
      ; // stay in this state
    #"SHARP" =>
      enter-state(token-dispatcher, sharp);

      let parser = preprocessor-state.preprocessor-directive-parser;
      simple-parser-reset(parser, start-symbol: #"directive");
      simple-parser-consume-token(parser, 0, #"SHARP", #f,
                                  start-position, end-position);
    #"EOF" =>
      ; // EOF is okay here
    otherwise =>
      enter-state(token-dispatcher, linemid);

      unless (preprocessor-state.preprocessor-skipping?)
        C-preprocess-token(token-dispatcher, token-number,
                           token-name, token-value,
                           start-position, end-position);
      end unless;
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function linemid
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  select (token-name)
    #"NEW-LINE" =>
      enter-state(token-dispatcher, linestart);
    #"WHITESPACE" =>
      ; // stay in this state
    #"EOF" =>
      signal("File does not end in a newline");
    otherwise =>
      unless (token-dispatcher.token-dispatcher-state.preprocessor-skipping?)
        C-preprocess-token(token-dispatcher, token-number,
                           token-name, token-value,
                           start-position, end-position);
      end unless;
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function sharp
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  select (token-name)
    #"IDENTIFIER" =>
      let dialect = preprocessor-state.preprocessor-dialect;
      let directive
        = element(dialect.preprocessor-dialect-directives, token-value,
                  default: #f);
      if (directive)
        let parser = preprocessor-state.preprocessor-directive-parser;
        simple-parser-consume-token(parser, 0, token-value, #f,
                                    start-position, end-position);

        if (preprocessor-state.preprocessor-skipping?)
          if (~directive.directive-skipping-function(token-dispatcher))
            enter-state(token-dispatcher, sharp-skipping);
          elseif (directive.directive-macro-replaced?)
            enter-state(token-dispatcher, sharp-directive-expanded);
          else
            enter-state(token-dispatcher, sharp-directive);
          end if;
        else
          if (directive.directive-lexical-definition)
            token-dispatcher.token-dispatcher-scanner.scanner-lexical-definition
             := directive.directive-lexical-definition;
          end if;

          if (directive.directive-macro-replaced?)
            enter-state(token-dispatcher, sharp-directive-expanded);
          else
            enter-state(token-dispatcher, sharp-directive);
          end if;
        end if;
      elseif (preprocessor-state.preprocessor-skipping?)
        enter-state(token-dispatcher, sharp-skipping);
      else
        preprocessor-error(token-dispatcher, start-position, end-position,
                           "invalid preprocessing directive #%s", token-value);
      end if;

    #"NEW-LINE" =>
      // Null directive
      enter-state(token-dispatcher, linestart);

    #"WHITESPACE" =>
      ; // remain in this state

    otherwise =>
      if (preprocessor-state.preprocessor-skipping?)
        enter-state(token-dispatcher, sharp-skipping);
      else
        preprocessor-error(token-dispatcher, start-position, end-position,
                           "invalid preprocessing directive");
      end if;
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function sharp-directive
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let parser = preprocessor-state.preprocessor-directive-parser;

  select (token-name)
    #"WHITESPACE" =>
      if (simple-parser-can-consume-token?(parser, token-number, token-name))
        simple-parser-consume-token(parser, token-number, token-name, #f,
                                    start-position, end-position);
      end if;

    #"NEW-LINE" =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);

      enter-state(token-dispatcher, linestart);

    #"EOF" =>
      signal("File does not end in a newline");
      simple-parser-consume-token(parser, 0, #"NEW-LINE", #f,
                                  start-position, end-position);

    otherwise =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);
      
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function sharp-skipping
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  if (token-name == #"NEW-LINE")
    enter-state(token-dispatcher, linestart);
  end if;  
end preprocessor-state-function;
            
define class <C-preprocessing-macro> (<source-location-mixin>)
  constant slot preprocessing-macro-parameters :: false-or(<sequence>),
    init-value: #f, init-keyword: parameters:;
  constant slot preprocessing-macro-varargs-parameter
    :: false-or(<byte-string>),
    init-value: #f, init-keyword: varargs-parameter:;
  constant slot preprocessing-macro-replacement :: <sequence>,
    required-init-keyword: replacement:;
  constant slot preprocessing-macro-expander :: false-or(<function>),
    init-value: #f, init-keyword: expander:;
  slot preprocessing-macro-suppressed? :: <boolean>,
    init-value: #f;
end class;
            
define sealed domain make(singleton(<C-preprocessing-macro>));
            
define sealed domain initialize(<C-preprocessing-macro>);
            
define macro with-macro-suppressed
  { with-macro-suppressed (?definition:expression) ?:body end }
    => { block ()
           ?definition.preprocessing-macro-suppressed? := #t;
           ?body
         cleanup
           ?definition.preprocessing-macro-suppressed? := #f;
          end block }
end macro;           
            
define sealed method \=
    (macro1 :: <C-preprocessing-macro>,
     macro2 :: <C-preprocessing-macro>)
 => (equal? :: <boolean>);
  (macro1.preprocessing-macro-parameters
     = macro2.preprocessing-macro-parameters)
    & (macro1.preprocessing-macro-varargs-parameter
         = macro2.preprocessing-macro-varargs-parameter)
    & (macro1.preprocessing-macro-replacement
         = macro2.preprocessing-macro-replacement)
end method;    
            
define class <C-preprocessing-token> (<object>)
  constant slot preprocessing-token-name :: <symbol>,
    required-init-keyword: name:;
  constant slot preprocessing-token-value
      :: type-union(<byte-string>, <boolean>),
    init-value: #f, init-keyword: value:;
/*
  constant slot preprocessing-token-start-position :: false-or(<integer>),
    init-value: #f, init-keyword: start-position:;
  constant slot preprocessing-token-end-position :: false-or(<integer>),
    init-value: #f, init-keyword: end-position:;
*/
end class;
            
define sealed method \=
    (token1 :: <C-preprocessing-token>,
     token2 :: <C-preprocessing-token>)
 => (result :: <boolean>);
  token1.preprocessing-token-name == token2.preprocessing-token-name
    & token1.preprocessing-token-value = token2.preprocessing-token-value
end method;
            
define function preprocessor-add-macro-definition
    (preprocessing-translation-unit
       :: <C-preprocessing-translation-unit-representation>,
     macro-name :: <byte-string>,
     definition :: <C-preprocessing-macro>)
 => ();
  let previous-definition
    = element(preprocessing-translation-unit.preprocessing-macro-definitions,
              macro-name,
              default: #f);
  if (previous-definition)
    if (definition ~= previous-definition)
      source-warning(definition, "Redefining macro %s", macro-name);
    end if;
  else
    preprocessing-translation-unit.preprocessing-macro-definitions[macro-name]
      := definition;
  end if;
end function;
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "define");
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "undef");
              
define function C-preprocess-token
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
 => ();
  let pp-translation-unit
    = token-dispatcher.token-dispatcher-preprocessing-translation-unit;
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let rangemap = token-dispatcher.token-dispatcher-rangemap;

  if (token-name == #"IDENTIFIER")
    let definition
      = element(pp-translation-unit.preprocessing-macro-definitions,
                token-value, default: #f);
    if (definition & ~definition.preprocessing-macro-suppressed?)
      C-expand-macro(token-dispatcher, definition, token-value,
                     start-position, end-position, #t);
    else
      (preprocessor-state.preprocessor-consumer)
        (preprocessor-state.preprocessor-consumer-data,
         token-name, token-value,
         rangemap, start-position, end-position);
    end if;
  else
    (preprocessor-state.preprocessor-consumer)
      (preprocessor-state.preprocessor-consumer-data,
       token-name, token-value,
       rangemap, start-position, end-position);
  end if;
end function;
            
define function C-expand-macro
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     definition :: <C-preprocessing-macro>,
     token-value, start-position, end-position,
     newline-permitted? :: <boolean>)
 => ();
  if (definition.preprocessing-macro-parameters)
    
token-dispatcher.token-dispatcher-macro-invocation
  := make(<C-preprocessing-macro-invocation>,
          name: token-value,
          start-position: start-position,
          end-position: end-position,
          macro-definition: definition,
          state-function: token-dispatcher.token-dispatcher-state-function,
          newline-permitted?: newline-permitted?);
enter-state(token-dispatcher, macro-name);
            
  elseif (definition.preprocessing-macro-expander)
    definition.preprocessing-macro-expander(token-dispatcher, token-value,
                                            start-position, end-position);
  else
    // Object-like macro: expand directly
    with-macro-suppressed (definition)
      C-expand-replacement-list
        (token-dispatcher, definition, #f, start-position, end-position);
    end;
  end if;
end function;
            
define class <C-preprocessing-macro-invocation> (<object>)
  // Name of the invoked macro
  constant slot invocation-name :: <byte-string>,
    required-init-keyword: name:;
  // Starting position of the macro name in the source
  constant slot invocation-start-position :: <integer>,
    required-init-keyword: start-position:;
  // Ending position of the macro name in the source
  constant slot invocation-end-position :: <integer>,
    required-init-keyword: end-position:;
  // Macro definition
  constant slot invocation-macro-definition :: <C-preprocessing-macro>,
    required-init-keyword: macro-definition:;
  // State from which macro expansion state was entered
  constant slot invocation-state-function :: <function>,
    required-init-keyword: state-function:;
  // True of newline is permitted within the macro invocation
  constant slot invocation-newline-permitted? :: <boolean>,
    required-init-keyword: newline-permitted?:;
  // Tokens saved for the current argument
  constant slot invocation-saved-tokens :: <stretchy-vector>
    = make(<stretchy-vector>);
  // Table (indexed by parameter name) of argument values
  constant slot invocation-arguments :: <object-table>
    = make(<object-table>);
  // Count of nested parentheses (beyond the first opening parenthesis)
  slot invocation-parens :: <integer>,
    init-value: 0;
  // Number of arguments processed so far
  slot invocation-argument-count :: <integer>,
    init-value: 0;
end class;
            
define function abandon-macro-invocation
    (token-dispatcher :: <C-preprocessor-token-dispatcher>)
 => ();
  let invocation = token-dispatcher.token-dispatcher-macro-invocation;
  token-dispatcher.token-dispatcher-macro-invocation := #f;

  token-dispatcher.token-dispatcher-state-function
    := invocation.invocation-state-function;
  
  let definition = invocation.invocation-macro-definition;
  with-macro-suppressed (definition)
    token-dispatcher.token-dispatcher-state-function
      (token-dispatcher, 0, #"IDENTIFIER", invocation.invocation-name,
       invocation.invocation-start-position,
       invocation.invocation-end-position);
  end;
end function;
            
define preprocessor-state-function macro-name
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  select (token-name)
    #"LPAREN" =>
      enter-state(token-dispatcher, macro-arguments);

    #"WHITESPACE" =>
      ; // remain in this state

    #"NEW-LINE" =>
      let invocation = token-dispatcher.token-dispatcher-macro-invocation;
      if (invocation.invocation-newline-permitted?)
        enter-state(token-dispatcher, macro-name-linestart);
      else
        abandon-macro-invocation(token-dispatcher);

        token-dispatcher.token-dispatcher-state-function
          (token-dispatcher, token-number, token-name, token-value,
           start-position, end-position);
      end if;

    otherwise =>
      abandon-macro-invocation(token-dispatcher);

      token-dispatcher.token-dispatcher-state-function
        (token-dispatcher, token-number, token-name, token-value,
         start-position, end-position);
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function macro-name-linestart
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  select (token-name)
    #"LPAREN" =>
      enter-state(token-dispatcher, macro-arguments);

    #"WHITESPACE", #"NEW-LINE" =>
      ; // remain in this state

    otherwise =>
      abandon-macro-invocation(token-dispatcher);

      token-dispatcher.token-dispatcher-state-function
        (token-dispatcher, 0, #"NEW-LINE", #f,
         start-position, end-position);

      token-dispatcher.token-dispatcher-state-function
        (token-dispatcher, token-number, token-name, token-value,
         start-position, end-position);
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function macro-arguments
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let invocation = token-dispatcher.token-dispatcher-macro-invocation;
  local
    method save-token () => ();
      let token
        = make(<C-preprocessing-token>,
               name: token-name, value: token-value);
      add!(invocation.invocation-saved-tokens, token);
    end method;

  select (token-name)
    #"COMMA" =>
      if (zero?(invocation.invocation-parens))
        
let definition = invocation.invocation-macro-definition;
let index = invocation.invocation-argument-count;
if (index < definition.preprocessing-macro-parameters.size)
  let parameter = definition.preprocessing-macro-parameters[index];
  invocation.invocation-arguments[parameter]
    := as(<simple-object-vector>, invocation.invocation-saved-tokens);
  invocation.invocation-saved-tokens.size := 0;
  invocation.invocation-argument-count := index + 1;
elseif (definition.preprocessing-macro-varargs-parameter)
  save-token();
else
  preprocessor-error(token-dispatcher, start-position, end-position,
                     "too many arguments to %s macro",
                     invocation.invocation-name);
end if;
            
      else
        save-token();
      end if;

    #"LPAREN" =>
      invocation.invocation-parens := invocation.invocation-parens + 1;
      save-token();

    #"RPAREN" =>
      if (zero?(invocation.invocation-parens))
        
let definition = invocation.invocation-macro-definition;
let index = invocation.invocation-argument-count;
if (index < definition.preprocessing-macro-parameters.size)
  let parameter = definition.preprocessing-macro-parameters[index];
  invocation.invocation-arguments[parameter]
    := as(<simple-object-vector>, invocation.invocation-saved-tokens);

  if (index + 1 ~= definition.preprocessing-macro-parameters.size)
    preprocessor-error(token-dispatcher, start-position, end-position,
                       "too few arguments to %s macro (expected %d)",
                       invocation.invocation-name,
                       definition.preprocessing-macro-parameters.size);
  end if;
elseif (definition.preprocessing-macro-varargs-parameter)
  let parameter = definition.preprocessing-macro-varargs-parameter;
  invocation.invocation-arguments[parameter]
    := as(<simple-object-vector>, invocation.invocation-saved-tokens);
elseif (~empty?(invocation.invocation-saved-tokens))
  preprocessor-error(token-dispatcher, start-position, end-position,
                     "too many arguments to %s macro",
                     invocation.invocation-name);
end if;
            
token-dispatcher.token-dispatcher-macro-invocation := #f;
token-dispatcher.token-dispatcher-state-function
  := invocation.invocation-state-function;

with-macro-suppressed (definition)
  C-expand-replacement-list
    (token-dispatcher,
     definition,
     invocation.invocation-arguments,
     invocation.invocation-start-position,
     end-position);
end;
            
      else
        invocation.invocation-parens := invocation.invocation-parens - 1;
        save-token();
      end if;
      
    #"WHITESPACE", #"NEW-LINE" =>
      if (token-name == #"NEW-LINE"
            & ~invocation.invocation-newline-permitted?)
        preprocessor-error(token-dispatcher, start-position, end-position,
                           "newline within invocation of %s macro",
                           invocation.invocation-name);
      end if;
      
      let saved-tokens = invocation.invocation-saved-tokens;
      unless (empty?(saved-tokens)
                | saved-tokens[saved-tokens.size - 1].preprocessing-token-name
                    == #"WHITESPACE")
        add!(invocation.invocation-saved-tokens,
             make(<C-preprocessing-token>, name: #"WHITESPACE"));
      end unless;

    #"EOF" =>
      preprocessor-error(token-dispatcher, start-position, end-position,
                         "end of input within invocation of %s macro",
                         invocation.invocation-name);

    otherwise =>
      save-token();
  end select;
end preprocessor-state-function;
            
define function C-expand-replacement-list
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     definition :: <C-preprocessing-macro>,
     parameters :: false-or(<object-table>),
     start-position, end-position)
 => ();
  let replacement-list = definition.preprocessing-macro-replacement;
  let limit = replacement-list.size;
  local
    
    method item
        (i :: <integer>)
     => (token-name, token-value, next :: <integer>);
      let token :: <C-preprocessing-token> = replacement-list[i];
      let token-name = token.preprocessing-token-name;
      let token-value = token.preprocessing-token-value;
      select (token-name)
        #"IDENTIFIER" =>
          let expansion
            = parameters & element(parameters, token-value, default: #f);
          if (expansion)
            values(#"PARAMETER", expansion, i + 1)
          else
            values (token-name, token-value, i + 1);
          end if;
        #"SHARP" =>
          if (parameters)
            if (i + 1 < limit)
              let pname-token :: <C-preprocessing-token>
                = replacement-list[i + 1];
              let pname-token-value = pname-token.preprocessing-token-value;
              if (pname-token.preprocessing-token-name == #"IDENTIFIER"
                    & element(parameters, pname-token-value, default: #f))
                
let stringified
  = with-output-to-string (str)
      write-element(str, '\"');
      for (param-token :: <C-preprocessing-token>
             in parameters[pname-token-value])
        let param-token-name = param-token.preprocessing-token-name;
        let param-token-value = param-token.preprocessing-token-value;    
        if (param-token-name == #"STRING-LITERAL"
              | param-token-name == #"CHARACTER-CONSTANT")
          for (c in param-token-value)
            if (c == '\\' | c == '\"')
              write-element(str, '\\');
            end if;
            write-element(str, c);
          end for;
        else
          write(str, preprocessor-token-string(param-token-name,
                                               param-token-value));
        end if;
      end for;
      write-element(str, '\"');
    end;
values(#"STRING-LITERAL", stringified, i + 2);
            
              else
                source-error(definition,
                             "A macro parameter name must follow the "
                             "# operator");
              end if;
            else
              source-error(definition,
                           "A macro parameter name must follow the # operator");
            end if;
          else
            values (token-name, token-value, i + 1);
          end if;
        otherwise =>
          values (token-name, token-value, i + 1);
      end select;
    end,
            
    method paste
        (token-name, token-value, paste-token-name, paste-token-value)
     => (placemarker);
      let placemarker
        = if (token-name == #"PLACEMARKER")
            token-value
          else
            paste-aux(make(<string-stream>, direction: #"output"),
                      token-name, token-value);
          end if;
      paste-aux(placemarker, paste-token-name, paste-token-value);
    end method,
            
    method paste-aux
        (placemarker, token-name, token-value)
     => (placemarker);
      if (token-name == #"PARAMETER")
        for (param-token :: <C-preprocessing-token> in token-value)
          write(placemarker,
                preprocessor-token-string(param-token.preprocessing-token-name,
                                          param-token.preprocessing-token-value));
        end for;
      else
        write(placemarker, preprocessor-token-string(token-name, token-value));
      end if;
      placemarker
    end method,
            
    method output (token-name, token-value) => ();
      if (token-name == #"PARAMETER")
        
local
  method perform-reexpansion
      (expansion-token-dispatcher :: <C-preprocessor-token-dispatcher>,
       token-number, token-name :: <symbol>, token-value,
       start-position, end-position)
   => ();
    if (token-name == #"IDENTIFIER")
      let pp-translation-unit
        = expansion-token-dispatcher
            .token-dispatcher-preprocessing-translation-unit;
      let definition
        = element(pp-translation-unit.preprocessing-macro-definitions,
                  token-value, default: #f);
      if (definition & ~definition.preprocessing-macro-suppressed?)
        C-expand-macro(expansion-token-dispatcher, definition, token-value,
                       start-position, end-position, #t);
      else
        token-dispatcher.token-dispatcher-state-function
          (token-dispatcher, 0, token-name, token-value,
           start-position, end-position);
      end if;
    elseif (token-name ~== #"EOF")
      token-dispatcher.token-dispatcher-state-function
        (token-dispatcher, 0, token-name, token-value,
         start-position, end-position);
    end if;
  end;

let expansion-token-dispatcher
  = make(<C-preprocessor-token-dispatcher>,
         preprocessing-translation-unit:
           token-dispatcher.token-dispatcher-preprocessing-translation-unit,
         scanner: token-dispatcher.token-dispatcher-scanner,
         directory: token-dispatcher.token-dispatcher-directory,
         rangemap: token-dispatcher.token-dispatcher-rangemap,
         consumer-state-function: perform-reexpansion,
         consumer-state: token-dispatcher.token-dispatcher-state);
            
for (param-token :: <C-preprocessing-token> in token-value)
  let param-token-name = param-token.preprocessing-token-name;
  let param-token-value = param-token.preprocessing-token-value;    
  expansion-token-dispatcher.token-dispatcher-state-function
    (expansion-token-dispatcher, 0, param-token-name, param-token-value,
     start-position, end-position);
end for;

expansion-token-dispatcher.token-dispatcher-state-function
  (expansion-token-dispatcher, 0, #"EOF", #f, end-position, end-position);
            
      elseif (token-name == #"PLACEMARKER")
        
let scanner
  = make(<simple-lexical-scanner>,
         definition: $C-tokens,
         rangemap: token-dispatcher.token-dispatcher-rangemap);
scanner.scanner-source-position := start-position;
scan-tokens(scanner,
            C-preprocessor-dispatch-token,
            token-dispatcher,
            stream-contents(token-value));
            
      elseif (token-name)
        token-dispatcher.token-dispatcher-state-function
          (token-dispatcher, 0, token-name, token-value,
           start-position, end-position);
      end if;
    end method,
            
    method loop (i :: <integer>, prev-token-name, prev-token-value)
      if (i < limit)
        let (token-name, token-value, next) = item(i);
        if (token-name == #"SHARPSHARP" & i + 1 < limit)
          let (paste-token-name, paste-token-value, next) = item(i + 1);
          let placemarker = paste(prev-token-name, prev-token-value,
                                  paste-token-name, paste-token-value);
          loop(next, #"PLACEMARKER", placemarker);
        else
          output(prev-token-name, prev-token-value);
          loop(next, token-name, token-value);
        end if;
      else
        output(prev-token-name, prev-token-value);
      end if;
    end method;
  loop(0, #f, #f);
end function;
            
define constant $month-abbreviations
  = #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Nov", "Dec"];
            
define method initialize
    (instance :: <C-preprocessing-translation-unit-representation>,
     #key, #all-keys)
 => ();
  let now = current-date();
  let date-string
    = format-to-string("\"%s %2d %4d\"",
                       $month-abbreviations[now.date-month - 1],
                       now.date-day,
                       now.date-year);
  preprocessor-define(instance, "__DATE__", date-string);
  let time-string
    = format-to-string("\"%02d:%02d:%02d\"",
                       now.date-hours, now.date-minutes, now.date-seconds);
  preprocessor-define(instance, "__TIME__", time-string);

  preprocessor-add-macro-definition
    (instance, intern-string("__FILE__"),
     make(<C-preprocessing-macro>,
          replacement: #[],
          expander:
            method (dispatcher, name, start-position, end-position)
              let srcloc
                = range-source-location(dispatcher.token-dispatcher-rangemap,
                                        start-position, end-position);
              let literal
                = concatenate("\"", as(<string>, srcloc.source-file), "\"");
              dispatcher.token-dispatcher-state-function
                (dispatcher, 0, #"STRING-LITERAL", literal,
                 start-position, end-position);              
            end));

  preprocessor-add-macro-definition
    (instance, intern-string("__LINE__"),
     make(<C-preprocessing-macro>,
          replacement: #[],
          expander:
            method (dispatcher, name, start-position, end-position)
              let srcloc
                = range-source-location(dispatcher.token-dispatcher-rangemap,
                                        start-position, end-position);
              dispatcher.token-dispatcher-state-function
                (dispatcher,
                 0, #"PP-NUMBER", integer-to-string(srcloc.source-start-line),
                 start-position, end-position);              
            end));

  preprocessor-define(instance, "__STDC__", "1");
  preprocessor-define(instance, "__STDC_HOSTED__", "1");
  preprocessor-define(instance, "__STDC_VERSION__", "199901L");
end method;
            
define constant $defined-operator :: <string> = intern-string("defined");
            
define preprocessor-state-function sharp-directive-expanded
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let parser = preprocessor-state.preprocessor-directive-parser;

  select (token-name)
    #"WHITESPACE" =>
      if (simple-parser-can-consume-token?(parser, token-number, token-name))
        simple-parser-consume-token(parser, token-number, token-name, #f,
                                    start-position, end-position);
      end if;

    #"NEW-LINE" =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);

      token-dispatcher.token-dispatcher-scanner.scanner-lexical-definition
        := $C-tokens;
      enter-state(token-dispatcher, linestart);

    #"IDENTIFIER" =>
      token-dispatcher.token-dispatcher-scanner.scanner-lexical-definition
        := $C-tokens;

      let pp-translation-unit
        = token-dispatcher.token-dispatcher-preprocessing-translation-unit;
      let definition
        = element(pp-translation-unit.preprocessing-macro-definitions,
                  token-value, default: #f);
      if (definition & ~definition.preprocessing-macro-suppressed?)
        C-expand-macro(token-dispatcher, definition, token-value,
                       start-position, end-position, #f);
      elseif (token-value == $defined-operator)
        
simple-parser-consume-token(parser, 0, #"DEFINED", #f,
                            start-position, end-position);
enter-state(token-dispatcher, sharp-directive-defined);
            
      else
        simple-parser-consume-token(parser,
                                    token-number, token-name, token-value,
                                    start-position, end-position);
      end if;

    #"EOF" =>
      signal("File does not end in a newline");
      simple-parser-consume-token(parser, 0, #"NEW-LINE", #f,
                                  start-position, end-position);

    otherwise =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);
      
  end select;
end preprocessor-state-function;
            
define preprocessor-state-function sharp-directive-defined
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     token-number, token-name :: <symbol>, token-value,
     start-position, end-position)
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let parser = preprocessor-state.preprocessor-directive-parser;

  select (token-name)
    #"WHITESPACE" =>
      #f;

    #"NEW-LINE" =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);

      enter-state(token-dispatcher, linestart);

    #"IDENTIFIER" =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);
      enter-state(token-dispatcher, sharp-directive-expanded);

    #"EOF" =>
      signal("File does not end in a newline");
      simple-parser-consume-token(parser, 0, #"NEW-LINE", #f,
                                  start-position, end-position);

    otherwise =>
      simple-parser-consume-token(parser, token-number, token-name, token-value,
                                  start-position, end-position);
  end select;
end preprocessor-state-function;
            
define function preprocessor-begin-conditional
    (token-dispatcher :: <C-preprocessor-token-dispatcher>, flag)
 => ();
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  preprocessor-state.preprocessor-skipping? := ~flag;
  add!(preprocessor-state.preprocessor-skipping?-stack, ~flag);
  add!(preprocessor-state.preprocessor-true-seen?-stack, flag);
  add!(preprocessor-state.preprocessor-else-seen?-stack, #f);
end function;
            
define function if-skipping-function
    (token-dispatcher :: <C-preprocessor-token-dispatcher>)
 => (parse-directive? :: <boolean>);
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  add!(preprocessor-state.preprocessor-skipping?-stack, #t);
  add!(preprocessor-state.preprocessor-true-seen?-stack, #f);
  add!(preprocessor-state.preprocessor-else-seen?-stack, #f);
  #f;
end function;
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "ifdef",
                                skipping-function: if-skipping-function);
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "ifndef",
                                skipping-function: if-skipping-function);
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "if",
                                skipping-function: if-skipping-function,
                                macro-replaced?: #t);
              
define function else-skipping-function
    (token-dispatcher :: <C-preprocessor-token-dispatcher>)
 => (parse-directive? :: <boolean>);
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let index = preprocessor-state.preprocessor-skipping?-stack.size - 1;
  index = 0 | ~preprocessor-state.preprocessor-skipping?-stack[index - 1]
end function;
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "elif",
                                skipping-function: else-skipping-function,
                                macro-replaced?: #t);
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "else",
                                skipping-function: else-skipping-function);
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "endif",
                                skipping-function: method(dispatcher) #t end);
              
define constant $C-include-directive-tokens
  = simple-lexical-definition
      token EOF;

      // Whitespace and comments
      token WHITESPACE = "([ \t\f]|/\\*([^*]|\\*+[^*/])*\\*+/)+";

      // New-line characters and line-end comments
      token NEW-LINE = "(//[^\r\n]*)?(\n|\r|\r\n)";

      // Identifiers
      token IDENTIFIER :: <string> = "[a-zA-Z_][0-9a-zA-Z_]*",
        semantic-value-function:
          method (token-string :: <byte-string>,
                  token-start :: <integer>,
                  token-end :: <integer>);
            intern-string(token-string, start: token-start, end: token-end)
          end;
            
      token HEADER-NAME :: <string> = "<[^\n\r>]+>",
        semantic-value-function:
          method (token-string :: <byte-string>,
                  token-start :: <integer>,
                  token-end :: <integer>);
            as(<file-locator>,
               copy-sequence(token-string,
                             start: token-start + 1, end: token-end - 1))
          end;
        
      token SOURCE-NAME :: <string> = "\"[^\n\r\"]+\"",
        semantic-value-function:
          method (token-string :: <byte-string>,
                  token-start :: <integer>,
                  token-end :: <integer>);
            as(<file-locator>,
               copy-sequence(token-string,
                             start: token-start + 1, end: token-end - 1))
          end;
    end;
              
define-C-preprocessor-directive
  ($C90-C-preprocessor-dialect, "include",
   macro-replaced?: #t, lexical-definition: $C-include-directive-tokens);
              
define function do-include
    (token-dispatcher :: <C-preprocessor-token-dispatcher>,
     kind :: one-of(#"HEADER-NAME", #"SOURCE-NAME"),
     name :: <file-locator>)
 => ();
  token-dispatcher.token-dispatcher-scanner.scanner-lexical-definition
    := $C-tokens;
  
  let preprocessing-translation-unit
    = token-dispatcher.token-dispatcher-preprocessing-translation-unit;
  let preprocessor-state = token-dispatcher.token-dispatcher-state;
  let rangemap = token-dispatcher.token-dispatcher-rangemap;

  
let end-position
  = token-dispatcher.token-dispatcher-scanner.scanner-source-position;
let here
  = range-source-location(rangemap, end-position, end-position);
              
  
let (directory, search-path)
  = select (kind)
      #"HEADER-NAME" =>
        values(#f,
               preprocessing-translation-unit
                 .preprocessing-system-header-search-path);
      #"SOURCE-NAME" =>
        values(token-dispatcher.token-dispatcher-directory,
               preprocessing-translation-unit
                 .preprocessing-header-search-path);
    end select;
let new-source-position
  = do-with-C-header-file
      (name, directory, search-path,
       method
           (found-locator :: <file-locator>, stream :: <stream>)
        => (new-end-position :: <integer>);
         rangemap-add-line-file(rangemap, end-position, 1, found-locator);
         preprocess-C-stream(preprocessing-translation-unit,
                             stream, found-locator.locator-directory,
                             rangemap,
                             preprocessor-state.preprocessor-consumer,
                             preprocessor-state.preprocessor-consumer-data,
                             start-position: end-position + 1);
       end);
              
  
token-dispatcher.token-dispatcher-scanner.scanner-source-position
  := new-source-position;
rangemap-add-line-file(rangemap, new-source-position,
                       here.source-start-line, here.source-file);
              
end function;
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "line",
                                macro-replaced?: #t);
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "error");
              
define-C-preprocessor-directive($C90-C-preprocessor-dialect, "pragma");
              
define function C-screen-token
    (parser :: <simple-parser>, token-name, token-value,
     rangemap, start-position, end-position)
 => ();
  select (token-name)
    #"IDENTIFIER" =>
      
let reserved-word-token-name
  = element($C99-reserved-words, token-value, default: #f);
if (reserved-word-token-name)
  simple-parser-consume-token(parser, 0, reserved-word-token-name, #f,
                              start-position, end-position)
else
  simple-parser-consume-token(parser, 0, token-name, token-value,
                              start-position, end-position)
end if;
            
    #"PP-NUMBER" =>
      
simple-parser-consume-token(parser, 0, #"INTEGER-CONSTANT",
                            string-to-integer(token-value),
                            start-position, end-position);
            
    otherwise =>
      simple-parser-consume-token(parser, 0, token-name, token-value,
                                  start-position, end-position)
  end select;
end;
            
define constant $C99-reserved-words :: <object-table>
  = begin
      let spec =
        #[#("auto" . #"AUTO"),
          #("break" . #"BREAK"),
          #("case" . #"_CASE"),
          #("char" . #"CHAR"),
          #("const" . #"CONST"),
          #("continue" . #"CONTINUE"),
          #("default" . #"DEFAULT"),
          #("do" . #"DO"),
          #("double" . #"DOUBLE"),
          #("else" . #"_ELSE"),
          #("enum" . #"ENUM"),
          #("extern" . #"EXTERN"),
          #("float" . #"FLOAT"),
          #("for" . #"_FOR"),
          #("goto" . #"GOTO"),
          #("if" . #"_IF"),
          #("inline" . #"INLINE"),
          #("int" . #"INT"),
          #("long" . #"LONG"),
          #("register" . #"REGISTER"),
          #("restrict" . #"RESTRICT"),
          #("return" . #"RETURN"),
          #("short" . #"SHORT"),
          #("signed" . #"SIGNED"),
          #("sizeof" . #"SIZEOF"),
          #("static" . #"STATIC"),
          #("struct" . #"STRUCT"),
          #("switch" . #"SWITCH"),
          #("typedef" . #"TYPEDEF"),
          #("union" . #"UNION"),
          #("unsigned" . #"UNSIGNED"),
          #("void" . #"VOID"),
          #("volatile" . #"VOLATILE"),
          #("while" . #"_WHILE"),
          #("_Bool" . #"BOOL"),
          #("_Complex" . #"COMPLEX"),
          #("_Imaginary" . #"IMAGINARY")];
      let reserved-words = make(<object-table>, size: spec.size);
      for (word in spec)
        reserved-words[intern-string(word.head)] := word.tail;
      end for;
      reserved-words
    end;
            
define constant <C-storage-class-specifier>
  = one-of(#"TYPEDEF", #"EXTERN", #"STATIC", #"AUTO", #"REGISTER");
            
define constant <C-simple-type-specifier>
  = one-of(#"VOID", #"CHAR", #"SHORT", #"INT", #"LONG", #"FLOAT", #"DOUBLE",
           #"SIGNED", #"UNSIGNED", #"BOOL", #"COMPLEX", #"IMAGINARY");
            
define function assignment-action
    (operator :: <C-binary-expression-operator>)
 => (function :: <function>);
  method (p :: <simple-parser>, data, s, e)
    let srcloc = simple-parser-source-location(p, s, e);
    make(<C-binary-expression-representation>,
         source-location: srcloc,
         operator: #"ASSIGN",
         left: p[0],
         right: make(<C-binary-expression-representation>,
                     source-location: srcloc,
                     operator: operator,
                     left: p[0],
                     right: p[2]));
  end;
end function;
            
define constant $C99-grammar-productions
 = simple-grammar-productions
     
production translation-unit-opt => [/* empty */];
production translation-unit-opt => [translation-unit-opt external-declaration];
            
production external-declaration => [function-definition];
production external-declaration => [declaration];
            
production declaration => [declaration-specifiers init-declarator-list-opt SEMI];
          
production declaration-specifiers-opt => [/* empty */];
production declaration-specifiers-opt => [declaration-specifiers];

production declaration-specifiers
  => [storage-class-specifier declaration-specifiers-opt];

production declaration-specifiers
  => [type-specifier declaration-specifiers-opt];

production declaration-specifiers
  => [type-qualifier declaration-specifiers-opt];

production declaration-specifiers
  => [function-specifier declaration-specifiers-opt];
          
production init-declarator-list-opt :: <sequence> => [/* empty */] () #[];
production init-declarator-list-opt :: <sequence> => [init-declarator-list];

production init-declarator-list => [init-declarator];
production init-declarator-list => [init-declarator-list COMMA init-declarator];
          
production init-declarator => [declarator];
production init-declarator => [declarator EQUALS initializer];
          
production storage-class-specifier :: <C-storage-class-specifier>
    => [TYPEDEF] (builder)
  #"TYPEDEF";

production storage-class-specifier :: <C-storage-class-specifier>
    => [EXTERN] (builder)
  #"EXTERN";

production storage-class-specifier :: <C-storage-class-specifier>
    => [STATIC] (builder)
  #"STATIC";

production storage-class-specifier :: <C-storage-class-specifier>
    => [AUTO] (builder)
  #"AUTO";

production storage-class-specifier :: <C-storage-class-specifier>
    => [REGISTER] (builder)
  #"REGISTER";
            
production type-specifier :: <C-simple-type-specifier>
    => [VOID] (builder)
  #"VOID";
production type-specifier :: <C-simple-type-specifier>
    => [CHAR] (builder)
  #"CHAR";
production type-specifier :: <C-simple-type-specifier>
    => [SHORT] (builder)
  #"SHORT";
production type-specifier :: <C-simple-type-specifier>
    => [INT] (builder)
  #"INT";
production type-specifier :: <C-simple-type-specifier>
    => [LONG] (builder)
  #"LONG";
production type-specifier :: <C-simple-type-specifier>
    => [FLOAT] (builder)
  #"FLOAT";
production type-specifier :: <C-simple-type-specifier>
    => [DOUBLE] (builder)
  #"DOUBLE";
production type-specifier :: <C-simple-type-specifier>
    => [SIGNED] (builder)
  #"SIGNED";
production type-specifier :: <C-simple-type-specifier>
    => [UNSIGNED] (builder)
  #"UNSIGNED";
production type-specifier :: <C-simple-type-specifier>
    => [BOOL] (builder)
  #"BOOL";
production type-specifier :: <C-simple-type-specifier>
    => [COMPLEX] (builder)
  #"COMPLEX";
production type-specifier :: <C-simple-type-specifier>
    => [IMAGINARY] (builder)
  #"IMAGINARY";
production type-specifier => [struct-or-union-specifier];
production type-specifier => [enum-specifier];
production type-specifier => [TYPEDEF-NAME];
            
production struct-or-union-specifier
  => [struct-or-union LBRACE struct-declaration-list RBRACE];
production struct-or-union-specifier
  => [struct-or-union IDENTIFIER LBRACE struct-declaration-list RBRACE];
              
production struct-or-union-specifier => [struct-or-union IDENTIFIER];

production struct-or-union => [STRUCT];
production struct-or-union => [UNION];
              
production struct-declaration-list => [struct-declaration];
production struct-declaration-list
  => [struct-declaration-list struct-declaration];
              
production struct-declaration
  => [specifier-qualifier-list struct-declarator-list SEMI];
              
production specifier-qualifier-list-opt 
  => [/* empty */];
production specifier-qualifier-list-opt
  => [specifier-qualifier-list];

production specifier-qualifier-list
  => [type-specifier specifier-qualifier-list-opt];
production specifier-qualifier-list
  => [type-qualifier specifier-qualifier-list-opt];
              
production struct-declarator-list
  => [struct-declarator];
production struct-declarator-list
  => [struct-declarator-list COMMA struct-declarator];
              
production struct-declarator => [declarator];
production struct-declarator => [declarator-opt COLON constant-expression];
              
production enum-specifier => [ENUM LBRACE enumerator-list RBRACE];
production enum-specifier => [ENUM LBRACE enumerator-list COMMA RBRACE];
production enum-specifier => [ENUM IDENTIFIER LBRACE enumerator-list RBRACE];
production enum-specifier
  => [ENUM IDENTIFIER LBRACE enumerator-list COMMA RBRACE];
production enum-specifier => [ENUM IDENTIFIER];
              
production enumerator-list => [enumerator];
production enumerator-list => [enumerator-list COMMA enumerator];
              
production enumerator => [IDENTIFIER];
production enumerator => [IDENTIFIER EQUALS constant-expression];
              
production type-qualifier :: singleton(#"CONST") => [CONST] ()
  #"CONST";
production type-qualifier :: singleton(#"RESTRICT") => [RESTRICT] ()
  #"RESTRICT";
production type-qualifier :: singleton(#"VOLATILE") => [VOLATILE] ()
  #"VOLATILE";
            
production function-specifier :: singleton(#"INLINE") => [INLINE] ()
  #"INLINE";
            
production declarator-opt => [/* empty */];
production declarator-opt => [declarator];
            
production declarator => [pointer-opt direct-declarator];
            
production direct-declarator
  => [IDENTIFIER];
production direct-declarator
  => [LPAREN declarator RPAREN];
production direct-declarator
  => [direct-declarator LBRACK type-qualifier-list-opt assignment-expression-opt RBRACK];
production direct-declarator
  => [direct-declarator LBRACK STATIC type-qualifier-list-opt assignment-expression RBRACK];
production direct-declarator
  => [direct-declarator LBRACK type-qualifier-list STATIC assignment-expression RBRACK];
production direct-declarator
  => [direct-declarator LBRACK type-qualifier-list-opt STAR RBRACK];
production direct-declarator
  => [direct-declarator LPAREN parameter-type-list RPAREN];
production direct-declarator
  => [direct-declarator LPAREN identifier-list-opt RPAREN];
            
production pointer-opt => [/* empty */];
production pointer-opt => [pointer];
            
production pointer => [STAR type-qualifier-list-opt];
production pointer => [STAR type-qualifier-list-opt pointer];
            
production type-qualifier-list-opt => [/* empty */];
production type-qualifier-list-opt => [type-qualifier-list];

production type-qualifier-list => [type-qualifier];
production type-qualifier-list => [type-qualifier-list type-qualifier];
            
production parameter-type-list-opt => [/* empty */];
production parameter-type-list-opt => [parameter-type-list];

production parameter-type-list => [parameter-list];
production parameter-type-list => [parameter-list COMMA ELIPSIS];
	    
production parameter-list => [parameter-declaration];
production parameter-list => [parameter-list COMMA parameter-declaration];
            
production parameter-declaration => [declaration-specifiers declarator];

production parameter-declaration => [declaration-specifiers abstract-declarator-opt];
            
production identifier-list-opt => [/* empty */];
production identifier-list-opt => [identifier-list];

production identifier-list => [IDENTIFIER];
production identifier-list => [identifier-list COMMA IDENTIFIER];
            
production type-name => [specifier-qualifier-list abstract-declarator-opt];
            
production abstract-declarator-opt => [/* empty */];
production abstract-declarator-opt => [abstract-declarator];

production abstract-declarator => [pointer];
production abstract-declarator => [pointer-opt direct-abstract-declarator];
            
production direct-abstract-declarator => [LPAREN abstract-declarator RPAREN];
production direct-abstract-declarator => [LBRACK assignment-expression-opt RBRACK];
production direct-abstract-declarator => [LBRACK STAR RBRACK];
production direct-abstract-declarator => [LPAREN parameter-type-list-opt RPAREN];
production direct-abstract-declarator => [direct-abstract-declarator LBRACK assignment-expression-opt RBRACK];
production direct-abstract-declarator => [direct-abstract-declarator LBRACK STAR RBRACK];
production direct-abstract-declarator => [direct-abstract-declarator LPAREN parameter-type-list-opt RPAREN];
	    
production initializer => [assignment-expression];
production initializer => [LBRACE initializer-list RBRACE];
production initializer => [LBRACE initializer-list COMMA RBRACE];
            
production initializer-list => [designation-opt initializer];
production initializer-list => [initializer-list COMMA designation-opt initializer];
            
production designation-opt => [/* empty */];
production designation-opt => [designator-list EQUALS];

production designator-list => [designator];
production designator-list => [designator-list designator];
            
production designator => [LBRACK constant-expression RBRACK];
production designator => [DOT IDENTIFIER];
            
          
production function-definition
  => [declaration-specifiers declarator declaration-list-opt compound-statement];
          
production declaration-list-opt => [/* empty */];
production declaration-list-opt => [declaration-list-opt declaration];
          
make-production constant :: <C-constant-expression-representation>
    => [INTEGER-CONSTANT],
  value: INTEGER-CONSTANT;
make-production constant :: <C-constant-expression-representation>
    => [FLOATING-CONSTANT],
  value: FLOATING-CONSTANT;
make-production constant :: <C-constant-expression-representation>
    => [CHARACTER-CONSTANT],
  value: CHARACTER-CONSTANT;
              
production concatenated-string-literal :: <string>
    => [STRING-LITERAL] (builder)
  STRING-LITERAL; // FIXME
production concatenated-string-literal :: <string>
    => [concatenated-string-literal STRING-LITERAL] (builder)
  concatenate(concatenated-string-literal, STRING-LITERAL); // FIXME
              
make-production primary-expression
  :: <C-variable-reference-expression-representation>
    => [IDENTIFIER],
  variable: IDENTIFIER;
  
production primary-expression => [constant];

make-production primary-expression
  :: <C-string-literal-expression-representation>
    => [concatenated-string-literal],
  value: concatenated-string-literal;

production primary-expression => [LPAREN expression RPAREN];
            
production postfix-expression => [primary-expression];

production postfix-expression :: <C-unary-expression-representation>
    => [postfix-expression LBRACK expression RBRACK] (builder, srcloc)
  make(<C-unary-expression-representation>,
       source-location: srcloc,
       operator: #"DEREF",
       operand: make(<C-binary-expression-representation>,
                     source-location: srcloc,
                     operator: #"ADD",
                     left: postfix-expression,
                     right: expression));

make-production postfix-expression
  :: <C-function-call-expression-representation>
    => [postfix-expression LPAREN argument-expression-list-opt RPAREN],
  function: postfix-expression,
  arguments: argument-expression-list-opt;

make-production postfix-expression :: <C-member-expression-representation>
    => [postfix-expression DOT IDENTIFIER],
  operand: postfix-expression,
  name: IDENTIFIER;

production postfix-expression :: <C-member-expression-representation>
    => [postfix-expression ARROW IDENTIFIER] (builder, srcloc)
  make(<C-member-expression-representation>,
       source-location: srcloc,
       operand: make(<C-unary-expression-representation>,
                     source-location: srcloc,
                     operator: #"DEREF",
                     operand: postfix-expression),
       name: IDENTIFIER);

make-production postfix-expression :: <C-unary-expression-representation>
    => [postfix-expression INC],
  operator: #"POSTINC",
  operand: postfix-expression;

make-production postfix-expression :: <C-unary-expression-representation>
    => [postfix-expression DEC],
  operator: #"POSTDEC",
  operand: postfix-expression;

production postfix-expression
  => [LPAREN type-name RPAREN LBRACE initializer-list RBRACE];
production postfix-expression
  => [LPAREN type-name RPAREN LBRACE initializer-list COMMA RBRACE];
              
production argument-expression-list-opt :: <sequence> => [/* empty */] () #[];
production argument-expression-list-opt :: <sequence>
    => [argument-expression-list];

production argument-expression-list
  => [assignment-expression];
production argument-expression-list
  => [argument-expression-list COMMA assignment-expression];
            
production unary-expression => [postfix-expression];

make-production unary-expression :: <C-unary-expression-representation>
    => [INC unary-expression],
  operator: #"PREINC",
  operand: unary-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [DEC unary-expression],
  operator: #"PREDEC",
  operand: unary-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [AMP cast-expression],
  operator: #"ADDROF",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [STAR cast-expression],
  operator: #"DEREF",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [PLUS cast-expression],
  operator: #"PLUS",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [MINUS cast-expression],
  operator: #"MINUS",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [TILDE cast-expression],
  operator: #"BITWISE-NOT",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [BANG cast-expression],
  operator: #"LOGICAL-NOT",
  operand: cast-expression;

make-production unary-expression :: <C-unary-expression-representation>
    => [SIZEOF unary-expression],
  operator: #"SIZEOF",
  operand: unary-expression;

make-production unary-expression :: <C-sizeof-type-expression-representation>
    => [SIZEOF LPAREN type-name RPAREN],
  sizeof-type: type-name;
            
production cast-expression => [unary-expression];

make-production cast-expression :: <C-cast-expression-representation>
    => [LPAREN type-name RPAREN cast-expression],
  type: type-name,
  operand: cast-expression;
            
production multiplicative-expression => [cast-expression];

make-production multiplicative-expression :: <C-binary-expression-representation>
    => [multiplicative-expression STAR cast-expression],
  operator: #"MUL", left: multiplicative-expression, right: cast-expression;

make-production multiplicative-expression :: <C-binary-expression-representation>
  => [multiplicative-expression SLASH cast-expression],
  operator: #"DIV", left: multiplicative-expression, right: cast-expression;

make-production multiplicative-expression :: <C-binary-expression-representation>
  => [multiplicative-expression PERCENT cast-expression],
  operator: #"MOD", left: multiplicative-expression, right: cast-expression;
            
production additive-expression => [multiplicative-expression];

make-production additive-expression :: <C-binary-expression-representation>
    => [additive-expression PLUS multiplicative-expression],
  operator: #"ADD",
  left: additive-expression,
  right: multiplicative-expression;

make-production additive-expression :: <C-binary-expression-representation>
    => [additive-expression MINUS multiplicative-expression],
  operator: #"SUB",
  left: additive-expression,
  right: multiplicative-expression;
            
production shift-expression => [additive-expression];

make-production shift-expression :: <C-binary-expression-representation>
    => [shift-expression SHL additive-expression],
  operator: #"SHL",
  left: shift-expression,
  right: additive-expression;

make-production shift-expression :: <C-binary-expression-representation>
    => [shift-expression SHR additive-expression],
  operator: #"SHR",
  left: shift-expression,
  right: additive-expression;
            
production relational-expression => [shift-expression];

make-production relational-expression :: <C-binary-expression-representation>
    => [relational-expression LT shift-expression],
  operator: #"LT",
  left: relational-expression,
  right: shift-expression;

make-production relational-expression :: <C-binary-expression-representation>
    => [relational-expression GT shift-expression],
  operator: #"GT",
  left: relational-expression,
  right: shift-expression;

make-production relational-expression :: <C-binary-expression-representation>
    => [relational-expression LE shift-expression],
  operator: #"LE",
  left: relational-expression,
  right: shift-expression;

make-production relational-expression :: <C-binary-expression-representation>
    => [relational-expression GE shift-expression],
  operator: #"GE",
  left: relational-expression,
  right: shift-expression;
            
production equality-expression => [relational-expression];

make-production equality-expression :: <C-binary-expression-representation>
    => [equality-expression EQ relational-expression],
  operator: #"EQ",
  left: equality-expression,
  right: relational-expression;

make-production equality-expression :: <C-binary-expression-representation>
    => [equality-expression NE relational-expression],
  operator: #"NE",
  left: equality-expression,
  right: relational-expression;
            
production AND-expression => [equality-expression];

make-production AND-expression :: <C-binary-expression-representation>
    => [AND-expression AMP equality-expression],
  operator: #"BITWISE-AND",
  left: AND-expression,
  right: equality-expression;
            
production exclusive-OR-expression => [AND-expression];

make-production exclusive-OR-expression :: <C-binary-expression-representation>
    => [exclusive-OR-expression HAT AND-expression],
  operator: #"BITWISE-XOR",
  left: exclusive-OR-expression,
  right: AND-expression;
            
production inclusive-OR-expression => [exclusive-OR-expression];

make-production inclusive-OR-expression :: <C-binary-expression-representation>
    => [inclusive-OR-expression OR exclusive-OR-expression],
  operator: #"BITWISE-XOR",
  left: inclusive-OR-expression,
  right: exclusive-OR-expression;
            
production logical-AND-expression => [inclusive-OR-expression];

make-production logical-AND-expression :: <C-binary-expression-representation>
  => [logical-AND-expression AMPAMP inclusive-OR-expression],
  operator: #"LOGICAL-AND",
  left: logical-AND-expression,
  right: inclusive-OR-expression;
            
production logical-OR-expression => [logical-AND-expression];

make-production logical-OR-expression :: <C-binary-expression-representation>
    => [logical-OR-expression OROR logical-AND-expression],
  operator: #"LOGICAL-OR",
  left: logical-OR-expression,
  right: logical-AND-expression;
            
production conditional-expression => [logical-OR-expression];

make-production conditional-expression :: <C-conditional-expression-representation>
    => [logical-OR-expression QUEST expression COLON conditional-expression],
  condition: logical-OR-expression,
  true: expression,
  false: conditional-expression;
            
production assignment-expression-opt => [/* empty */];
production assignment-expression-opt => [assignment-expression];
            
production assignment-expression => [conditional-expression];

make-production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression EQUALS assignment-expression],
  operator: #"ASSIGN", 
  left: unary-expression,
  right: assignment-expression;

production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression STAR-EQUALS assignment-expression],
  action: assignment-action(#"MUL");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression SLASH-EQUALS assignment-expression],
  action: assignment-action(#"DIV");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression PERCENT-EQUALS assignment-expression],
  action: assignment-action(#"MOD");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression PLUS-EQUALS assignment-expression],
  action: assignment-action(#"ADD");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression MINUS-EQUALS assignment-expression],
  action: assignment-action(#"SUB");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression SHL-EQUALS assignment-expression],
  action: assignment-action(#"SHL");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression SHR-EQUALS assignment-expression],
  action: assignment-action(#"SHR");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression AMP-EQUALS assignment-expression],
  action: assignment-action(#"BITWISE-AND");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression HAT-EQUALS assignment-expression],
  action: assignment-action(#"BITWISE-XOR");
production assignment-expression :: <C-binary-expression-representation>
  => [unary-expression OR-EQUALS assignment-expression],
  action: assignment-action(#"BITWISE-OR");
            
production expression-opt => [/* empty */];
production expression-opt => [expression];
            
production expression => [assignment-expression];

make-production expression :: <C-binary-expression-representation>
  => [expression COMMA assignment-expression],
  operator: #"COMMA",
  left: expression,
  right: assignment-expression;
            
production constant-expression => [conditional-expression];
            
production statement => [open-statement];
production statement => [closed-statement];
          
production open-statement => [open-labeled-statement];
production open-statement => [open-selection-statement];
production open-statement => [open-iteration-statement];
          
production closed-statement => [closed-labeled-statement];
production closed-statement => [compound-statement];
production closed-statement => [expression-statement];
production closed-statement => [closed-selection-statement];
production closed-statement => [closed-iteration-statement];
production closed-statement => [jump-statement];
          
production open-labeled-statement
  => [IDENTIFIER COLON open-statement];
production open-labeled-statement
  => [_CASE constant-expression COLON open-statement];
production open-labeled-statement
  => [DEFAULT COLON open-statement];
            
production closed-labeled-statement
  => [IDENTIFIER COLON closed-statement];
production closed-labeled-statement
  => [_CASE constant-expression COLON closed-statement];
production closed-labeled-statement
  => [DEFAULT COLON closed-statement];
            
production compound-statement => [LBRACE block-item-list-opt RBRACE];
            
production block-item-list-opt => [/* empty */];
production block-item-list-opt => [block-item-list-opt block-item];
            
production block-item => [declaration];
production block-item => [statement];
            
production expression-statement => [expression-opt SEMI];
            
production open-selection-statement
    => [_IF LPAREN expression RPAREN statement];

production open-selection-statement
    => [_IF LPAREN expression RPAREN closed-statement _ELSE open-statement];

production open-selection-statement
    => [SWITCH LPAREN expression RPAREN open-statement];

            
production closed-selection-statement
    => [_IF LPAREN expression RPAREN closed-statement _ELSE closed-statement];

production closed-selection-statement
    => [SWITCH LPAREN expression RPAREN closed-statement];
            
production open-iteration-statement
    => [_WHILE LPAREN expression RPAREN open-statement];

production open-iteration-statement
    => [_FOR LPAREN expression-opt SEMI expression-opt SEMI expression-opt RPAREN open-statement];

production open-iteration-statement
    => [_FOR LPAREN declaration expression-opt SEMI expression-opt RPAREN open-statement];
            
production closed-iteration-statement
    => [_WHILE LPAREN expression RPAREN closed-statement];

production closed-iteration-statement
    => [DO statement _WHILE LPAREN expression RPAREN SEMI];

production closed-iteration-statement
    => [_FOR LPAREN expression-opt SEMI expression-opt SEMI expression-opt RPAREN closed-statement];

production closed-iteration-statement
    => [_FOR LPAREN declaration expression-opt SEMI expression-opt RPAREN closed-statement];
            
production jump-statement => [GOTO IDENTIFIER SEMI];
production jump-statement => [CONTINUE SEMI];
production jump-statement => [BREAK SEMI];
production jump-statement => [RETURN expression-opt SEMI];
            
   end;
            
define constant $C99-parser-automaton
  = simple-parser-automaton($C-tokens, $C99-grammar-productions,
                            #[#"translation-unit-opt"]);
            
define method print-C-expression
    (expression :: <C-constant-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  print(expression.expression-value, stream);
end method;
            
define method print-C-expression
    (expression :: <C-string-literal-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  print(expression.expression-value, stream);
end method;
            
define method print-C-expression
    (expression :: <C-variable-reference-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  write(stream, expression.expression-variable);
end method;
            
define method print-C-expression
    (expression :: <C-function-reference-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  // FIXME
end method;
            
define method print-C-expression
    (expression :: <C-unary-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  local
    method do-operator (name :: <byte-string>,
                        operator-level :: <integer>,
                        operand-level :: <integer>,
                        postfix? :: <boolean>)
      let parens? = operator-level > level;
      printing-logical-block(stream, prefix: parens? & "(",
                                     suffix: parens? & ")")
        unless (postfix?) write(stream, name) end;
        print-C-expression(expression.expression-unary-operand, stream,
                           level: operand-level);
        if (postfix?) write(stream, name) end;
      end;
    end;
  select (expression.expression-operator)
    #"POSTINC" =>
      do-operator("++", 1, 1, #t);
    #"POSTDEC" =>
      do-operator("--", 1, 1, #t);
    #"PREINC" =>
      do-operator("++", 2, 2, #f);
    #"PREDEC" =>
      do-operator("--", 2, 2, #f);
    #"ADDROF" =>
      do-operator("&", 2, 2, #f);
    #"DEREF" =>
      do-operator("*", 2, 2, #f);
    #"PLUS" =>
      do-operator("+", 2, 2, #f);
    #"MINUS" =>
      do-operator("-", 2, 2, #f);
    #"BITWISE-NOT" =>
      do-operator("~", 2, 2, #f);
    #"LOGICAL-NOT" =>
      do-operator("!", 2, 2, #f);
    #"SIZEOF" =>
      do-operator("sizeof ", 2, 2, #f);
    #"ALIGNOF" =>
      do-operator("__alignof__ ", 2, 2, #f);
  end;
end method;
            
define method print-C-expression
    (expression :: <C-binary-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  local
    method do-operator (name :: <byte-string>, operator-level :: <integer>)
      let parens? = operator-level > level;
      printing-logical-block(stream, prefix: parens? & "(",
                                     suffix: parens? & ")")
        print-c-expression(expression.expression-binary-left, stream,
                           level: operator-level);
        unless (name = ",") write-element(stream, ' ') end;
        pprint-newline(#"fill", stream);
        format(stream, "%s ", name);
        print-C-expression(expression.expression-binary-right, stream,
                           level: operator-level - 1);
      end;
    end;
  select (expression.expression-operator)
    #"MUL" =>
      do-operator("*", 4);
    #"DIV" =>
      do-operator("/", 4);
    #"MOD" =>
      do-operator("%", 4);
    #"ADD" =>
      do-operator("+", 5);
    #"SUB" =>
      do-operator("-", 5);
    #"SHL" =>
      do-operator("<<;", 6);
    #"SHR" =>
      do-operator(">>", 6);
    #"LT" =>
      do-operator("<", 7);
    #"LE" =>
      do-operator("<=", 7);
    #"GT" =>
      do-operator(">", 7);
    #"GE" =>
      do-operator(">=", 7);
    #"EQ" =>
      do-operator("==", 8);
    #"NE" =>
      do-operator("!=", 8);
    #"BITWISE-AND" =>
      do-operator("&", 9);
    #"BITWISE-XOR" =>
      do-operator("^", 10);
    #"BITWISE-OR" =>
      do-operator("|", 11);
    #"LOGICAL-AND" =>
      do-operator("&&", 12);
    #"LOGICAL-OR" =>
      do-operator("||", 13);
    #"ASSIGN" =>
      do-operator("=", 15);
    #"COMMA" =>
      do-operator(",", 16);
  end;
end method;
            
define method print-C-expression
    (expression :: <C-conditional-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  let parens? = level < 14;
  printing-logical-block(stream, prefix: parens? & "(",
                                 suffix: parens? & ")")
    print-C-expression(expression.expression-conditional-condition, stream,
                       level: 13);
    write-element(stream, ' ');
    pprint-newline(#"fill", stream);
    write(stream, "? ");
    print-C-expression(expression.expression-conditional-true, stream,
                       level: 16);
    write-element(stream, ' ');
    pprint-newline(#"fill", stream);
    write(stream, ": ");
    print-C-expression(expression.expression-conditional-false, stream,
                       level: 14);
  end;
end method;
            
define method print-C-expression
    (expression :: <C-cast-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  let parens? = level < 3;
  printing-logical-block(stream, prefix: parens? & "(",
                                 suffix: parens? & ")")
    write-element(stream, '(');
    // FIXME
    write(stream, ") ");
    print-C-expression(expression.expression-cast-operand, stream, level: 3);
  end;
end method;
            
define method print-C-expression
    (expression :: <C-sizeof-type-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  write(stream, "sizeof( ");
  // FIXME
  write-element(stream, ')');
end method;
            
define method print-C-expression
    (expression :: <C-function-call-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  print-C-expression(expression.expression-call-function, stream, level: 1);
  printing-logical-block(stream, prefix: "(", suffix: ")")
    for (argument in expression.expression-call-arguments,
         first? = #t then #f)
      unless (first?)
        write(stream, ", ");
        pprint-newline(#"fill", stream);
      end;
      print-C-expression(argument, stream, level: 15);
    end for;
  end;
end method;
            
define method print-C-expression
    (expression :: <C-member-expression-representation>,
     stream :: <stream>,
     #key level :: <integer> = $precedence-level-assignment-expression)
 => ();
  print-C-expression(expression.expression-member-operand, stream, level: 1);
  format(stream, ".%s", expression.expression-member-name);
end method;
            
define method print-message
    (expression :: <C-expression-representation>, stream :: <stream>)
 => ();
  print-C-expression(expression, stream,
                     level: $precedence-level-assignment-expression);
end method;

            
