Module: simple-lexical-scanner


define class <simple-lexical-scanner> (<object>)
  slot scanner-lexical-definition :: <simple-lexical-definition>,
    required-init-keyword: definition:;
  slot scanner-source-position :: <integer>,
    init-value: 0, init-keyword: position:;
  constant slot scanner-rangemap :: <source-location-rangemap>,
    required-init-keyword: rangemap:;
  
slot scanner-state :: <simple-lexical-state>;
slot scanner-saved-cr-seen? :: <boolean>, init-value: #f;
slot scanner-saved-text :: <byte-string>, init-value: "";
slot scanner-saved-accepting-state :: false-or(<simple-lexical-state>),
  init-value: #f;
slot scanner-saved-accepting-index :: <integer>, init-value: 0;
          
end class;
              
define method scan-tokens
    (scanner :: <simple-lexical-scanner>,
     consumer-function :: <function>,
     consumer-data :: <object>,
     text :: <byte-string>,
     #key start: text-start :: <integer> = 0,
          end: text-end :: <integer> = text.size,
          partial?)
 => ();
  
let state = scanner.scanner-state;
let cr-seen? = scanner.scanner-saved-cr-seen?;
let token-start :: <integer>
  = if (empty?(scanner.scanner-saved-text))
      text-start;
    else
      let saved-size = scanner.scanner-saved-text.size;
      scanner.scanner-source-position
        := scanner.scanner-source-position - saved-size;
      -(saved-size);
    end if;
let accepting-state :: false-or(<simple-lexical-state>)
  = scanner.scanner-saved-accepting-state;
let accepting-index :: <integer>
  = scanner.scanner-saved-accepting-index;
          
  iterate loop(index :: <integer> = text-start,
               state :: <simple-lexical-state> = scanner.scanner-state)
    if (index < text-end)
      let symbol = as(<integer>, text[index]);
      
if (state.lexical-state-accept-function)
  accepting-state := state;
  accepting-index := index;
end if;
let new-state = state.lexical-state-transitions[symbol];
if (new-state)
  
if (symbol = as(<integer>, '\n'))
  let source-position
    = scanner.scanner-source-position
    + if (token-start < 0)
        index - text-start - token-start
      else
        index - token-start
      end;
  rangemap-add-line(scanner.scanner-rangemap, source-position + 1, #f);
  cr-seen? := #f;
elseif (symbol = as(<integer>, '\r'))
  if (cr-seen?)
    let source-position
      = scanner.scanner-source-position
      + if (token-start < 0)
          index - text-start - token-start
        else
          index - token-start
        end;
    rangemap-add-line(scanner.scanner-rangemap, source-position, #f);
  end if;
  cr-seen? := #t;
elseif (cr-seen?)
  let source-position
    = scanner.scanner-source-position
    + if (token-start < 0)
        index - text-start - token-start
      else
        index - token-start
      end;
  rangemap-add-line(scanner.scanner-rangemap, source-position, #f);
  cr-seen? := #f;
end if;
          
  loop(index + 1, new-state);
elseif (accepting-state)
  
accepting-state.lexical-state-accept-function
  (scanner, consumer-function, consumer-data, text, text-start,
   token-start, accepting-index);
          
  if (accepting-index < 0)
    // 
    error("backtrack to %d isn't yet implemented", accepting-index);
  else
    token-start := accepting-index;
    accepting-state := #f;
    accepting-index := 0;
    loop(token-start, scanner.scanner-lexical-definition.lexical-automaton);
  end if;
else
  
let source-position
  = scanner.scanner-source-position
  + if (token-start < 0)
      index - text-start - token-start
    else
      index - token-start
    end;
source-error(range-source-location(scanner.scanner-rangemap,
                                   source-position,
                                   source-position),
             "unrecognized character '%c'", text[index]);
          
end if;
          
    else
      if (partial?)
        
scanner.scanner-state := state;
scanner.scanner-saved-cr-seen? := cr-seen?;
          
if (token-start < 0)
  let saved-size = scanner.scanner-saved-text.size;
  let new-saved-size = saved-size + (text-end - text-start);
  let new-saved-text :: <byte-string>
    = make(<byte-string>, size: new-saved-size);
  copy-bytes(scanner.scanner-saved-text, saved-size + token-start,
             new-saved-text, 0, saved-size);
  copy-bytes(text, text-start,
             new-saved-text, saved-size, text-end - text-start);
  scanner.scanner-saved-text := new-saved-text;
  scanner.scanner-source-position
    := scanner.scanner-source-position + new-saved-size;
elseif (token-start ~= text-end)
  let new-saved-size = text-end - token-start;
  let new-saved-text :: <byte-string>
    = make(<byte-string>, size: new-saved-size);
  copy-bytes(text, token-start, new-saved-text, 0, text-end - token-start);
  scanner.scanner-saved-text := new-saved-text;
  scanner.scanner-source-position
    := scanner.scanner-source-position + new-saved-size;
end if;
          
if (accepting-state)
  scanner.scanner-saved-accepting-state := accepting-state;
  if (accepting-index < 0)
    scanner.scanner-saved-accepting-index
      := accepting-index - (text-end - text-start);
  else
    scanner.scanner-saved-accepting-index := accepting-index - text-end;
  end if;
else
  scanner.scanner-saved-accepting-state := #f;
  scanner.scanner-saved-accepting-index := 0;
end if;
          
      else
        
if (state.lexical-state-accept-function)
  state.lexical-state-accept-function
    (scanner, consumer-function, consumer-data, text, text-start,
     token-start, index);
elseif (accepting-state)
  accepting-state.lexical-state-accept-function
    (scanner, consumer-function, consumer-data, text, text-start,
     token-start, accepting-index);
  error("unrecognized character at %d-%d\n", accepting-index, index);
elseif (token-start ~= index)
  error("unrecognized character at %d-%d\n", token-start, index);
end if;
          
      end if;
    end if;
  end iterate;
end method;
              
define method initialize
    (scanner :: <simple-lexical-scanner>,
     #key definition :: <simple-lexical-definition>, #all-keys)
 => ();
  next-method();
  scanner.scanner-state := definition.lexical-automaton;
end method;
          
define method token-accept-function
    (token-number :: false-or(<integer>),
     token-name :: false-or(<symbol>),
     token-semantic-value-function :: false-or(<function>))
 => (accept-function :: <function>);
  if (token-number)
    if (token-semantic-value-function)
      
method
    (scanner :: <simple-lexical-scanner>,
     consumer-function :: <function>,
     consumer-data :: <object>,
     text :: <byte-string>, text-start :: <integer>,
     token-start :: <integer>, accepting-index :: <integer>)
 => ();
let (semantic-value, accept-text-size)
  = if (token-start < 0)
      let saved-size = scanner.scanner-saved-text.size;
      if (accepting-index < 0)
        values(token-semantic-value-function(scanner.scanner-saved-text,
                                             saved-size + token-start,
                                             saved-size + accepting-index),
               accepting-index - token-start);
      else
        let accept-text-size = saved-size + (accepting-index - text-start);
        let accept-text :: <byte-string>
          = make(<byte-string>, size: accept-text-size);
        copy-bytes(scanner.scanner-saved-text, 0, accept-text, 0, saved-size);
        copy-bytes(text, text-start, accept-text, saved-size,
                   accept-text-size - saved-size);
        scanner.scanner-saved-text := "";
        values(token-semantic-value-function(accept-text, 0, accept-text-size),
               accept-text-size);
      end if;
    else
      values(token-semantic-value-function(text, token-start, accepting-index),
             accepting-index - token-start);
    end if;
  let start-position = scanner.scanner-source-position;
  let next-start-position = start-position + accept-text-size;
  scanner.scanner-source-position := next-start-position;
  consumer-function(consumer-data, token-number, token-name, semantic-value,
                    start-position, next-start-position - 1);
end;
          
    else
      
method
    (scanner :: <simple-lexical-scanner>,
     consumer-function :: <function>,
     consumer-data :: <object>,
     text, text-start :: <integer>,
     token-start :: <integer>, accepting-index :: <integer>)
 => ();
  let accept-text-size
    = if (token-start < 0 & accepting-index >= 0)
        scanner.scanner-saved-text := "";
        accepting-index - text-start - token-start;
      else
        accepting-index - token-start;
      end if;
  let start-position = scanner.scanner-source-position;
  let next-start-position = start-position + accept-text-size;
  scanner.scanner-source-position := next-start-position;
  consumer-function(consumer-data, token-number, token-name, #f,
                    start-position, next-start-position - 1);
end;
          
    end;
  else
    
method
    (scanner :: <simple-lexical-scanner>,
     consumer-function :: <function>,
     consumer-data :: <object>,
     text, text-start :: <integer>,
     token-start :: <integer>, accepting-index :: <integer>)
 => ();
  if (token-start < 0 & accepting-index >= 0)
    scanner.scanner-saved-text := "";
    scanner.scanner-source-position
      := scanner.scanner-source-position
      + accepting-index - text-start - token-start;
  else
    scanner.scanner-source-position
      := scanner.scanner-source-position
      + accepting-index - token-start
  end if;
end;
          
  end if;
end method;
          
