Module: p2k-lexer


define class <p2k-lexer> (<object>)
  constant slot lexer-stream :: <stream>,
    required-init-keyword: stream:;
  
slot current-token :: false-or(<symbol>) = #f;
slot token-value :: <object> = #f;
          
end class;
          
define method lexer-state-0 (stream :: <stream>)
  if(stream-at-end?(stream))
    #"EOF";
  else
    let char = read-element(stream);
    select(char)
      ' ', '\t', '\n', '\r' =>
        lexer-state-0(stream);
      '[' =>
        #"[";
      '(' =>
        lexer-state-lparen(stream);
      ')' =>
        #")";
      ']' =>
        #"]";
      '*' =>
        #"*";
      '+' =>
        #"+";
      ',' =>
        #",";
      ':' =>
        lexer-state-colon(stream);
      '-' =>
        #"-";
      '.' =>
        lexer-state-dot(stream);
      ';' =>
        #";";
      '<' =>
        lexer-state-lt(stream);
      '=' =>
        #"=";
      '>' =>
        lexer-state-gt(stream);
      otherwise =>
        if(('a' <= char & char <= 'z')
           | ('A' <= char & char <= 'Z'))
  	  lexer-state-alpha(stream, make(<string>, size: 1, fill: char));
        elseif('0' <= char & char <= '9')
          let val = as(<integer>, char) - as(<integer>, '0');
	  lexer-state-digit(stream, val);
        else
          error("illegal input character: '%c'", char);
        end if;
    end select;
  end if;
end method;
          
define method lexer-state-lparen (stream :: <stream>)
  select(peek(stream))
    '*' =>
      read-element(stream);
      lexer-state-comment(stream);
    otherwise =>
      #"(";
  end select;
end method;
          
define method lexer-state-comment (stream :: <stream>)
  select(read-element(stream))
      '*' =>
        lexer-state-comend(stream);
      otherwise =>
        lexer-state-comment(stream);
  end select;
end method;
          
define method lexer-state-comend (stream :: <stream>)
  select(read-element(stream))
    ')' =>
      lexer-state-0(stream);
    '*' =>
      lexer-state-comend(stream);
    otherwise =>
      lexer-state-comment(stream);
  end select;
end method;
          
define method lexer-state-colon (stream :: <stream>)
  select(peek(stream))
    '=' =>
      read-element(stream);
      #":=";
    otherwise =>
      #":";
  end select;
end method;
          
define method lexer-state-dot (stream :: <stream>)
  select(peek(stream))
    '.' =>
      read-element(stream);
      #"..";
    otherwise =>
      #".";
  end select;
end method;
          
define method lexer-state-lt (stream :: <stream>)
  select(peek(stream))
    '=' =>
      read-element(stream);
      #"<=";
    '>' =>
      read-element(stream);
      #"<>";
    otherwise =>
      #"<";
  end select;
end method;
          
define method lexer-state-gt (stream :: <stream>)
  select(peek(stream))
    '=' =>
      read-element(stream);
      #">=";
    otherwise =>
      #">";
  end select;
end method;
          
define method lexer-state-alpha (stream :: <stream>, string :: <string>)
  let char = peek(stream);
  if(('a' <= char & char <= 'z')
     | ('A' <= char & char <= 'Z')
     | ('0' <= char & char <= '9'))
    lexer-state-alpha(stream, add(string, read-element(stream)));
  else
    values(#"IDENT", string);
  end if;
end method;
          
define method lexer-state-digit
    (stream :: <stream>, number :: <integer>)
  let char = peek(stream);
  if('0' <= char & char <= '9')
    let val = as(<integer>, read-element(stream))
              - as(<integer>, '0');
    lexer-state-digit(stream, number * 10 + val);
  else
    values(#"NUMBER", number);
  end if;
end method;
          
define method match
    (lexer :: <p2k-lexer>, match-token :: <symbol>,
     #key value, consume = #t)
 => (result :: <boolean>);
  if(lexer.current-token == #f)
    let (token, #rest vals) = lexer-state-0(lexer.lexer-stream);
    lexer.current-token := token;
    unless(empty?(vals))
      lexer.token-value := first(vals);
    end;
  end if;
  if(lexer.current-token == match-token)
    if(value)
      if(value = lexer.token-value)
        if(consume) lexer.current-token := #f; end;
        #t;
      else
        #f;
      end if;
    else
      if(consume) lexer.current-token := #f; end;
      #t;
    end if;
  else
    #f;
  end if;
end method;
          
define method expect
    (lexer :: <p2k-lexer>, match-token :: <symbol>,
     #key value, consume = #t)
 => (result :: <boolean>);
  unless(match(lexer, match-token, value: value, consume: consume))
    error("Syntax error: %s expected, got %s",
      if(value)
        value;
      else
        as(<string>, match-token);
      end if, as(<string>, lexer.current-token));
  end;
end method;
          
