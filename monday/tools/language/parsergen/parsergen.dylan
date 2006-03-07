Module: parsergen


define function copy-header
    (input-stream :: <stream>, output-stream :: <stream>)
 => ();
  for(line = read-line(input-stream) then read-line(input-stream),
      until: line = "%%")
    write-line(output-stream, line);
  end for;
end function;
          
define function copy-trailer
    (input-stream :: <stream>, output-stream :: <stream>)
 => ();
  for(line = read-line(input-stream, on-end-of-stream: #f)
        then read-line(input-stream, on-end-of-stream: #f),
      while: line)
    write-line(output-stream, line);
  end for;
end function;
          
define class <parsergen-symbol> (<object>)
  constant slot symbol-type :: false-or(<symbol>),
    init-value: #f, init-keyword: type:;
end class;
          
define class <parsergen-terminal> (<parsergen-symbol>)
  constant slot terminal-number :: <integer>,
    required-init-keyword: number:;
end class;
          
define class <parsergen-union> (<parsergen-symbol>)
  constant slot union-components :: <sequence>,
    required-init-keyword: components:;
end class;
          
define class <parsergen-nonterminal> (<parsergen-symbol>)
  
slot nonterminal-goto-transitions :: <list>,
  init-value: #();
            
end class;
          
define class <parsergen-production> (<production>)
  constant slot production-action :: <sequence>,
    required-init-keyword: action:;
  constant slot production-number :: <integer>,
    required-init-keyword: number:;
end class;
          
define function get-token
    (stream :: <stream>)
 => (token :: <symbol>, #rest val);
  let symbol-chars = make(<stretchy-vector>);
  local
    
method state0() => (token :: <symbol>, #rest val);
  let char = read-element(stream, on-end-of-stream: #f);
  select(char)
    #f =>
      #"EOF";
    ' ', '\t', '\f', '\r', '\n' =>
      state0();
    ';' =>
      comment();
    '(' =>
      #"lparen";
    ')' =>
      #"rparen";
    '%' =>
      percent();
    ':' =>
      keyword();
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z',
    '+', '-', '*', '/', '=', '<', '>',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9' =>
      symbol(char);
    '|' =>
      vbarsymbol();

    otherwise =>
      error("unrecognized input character %c", char);
  end select;
end,
          
method comment() => (token :: <symbol>, #rest val);
  let char = read-element(stream);
  if(char == '\n' | char == '\r')
    state0();
  else
    comment();
  end if;
end,
          
method percent() => (token :: <symbol>, #rest val);
  let char = read-element(stream);
  if(char == '%')
    #"%%";
  else
    error("Unrecognized %%%c", char);
  end;
end,
          
method keyword
    ()
 => (token :: <symbol>, #rest val);
  let new-char = peek(stream);
  select(new-char)
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '_',
    ':', '+', '-', '*', '/', '=', '<', '>',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9' =>
      add!(symbol-chars, read-element(stream));
      keyword();
    otherwise =>
      values(#"keyword",
             as(<symbol>, as(<string>, symbol-chars)));
  end select; 
end,
          
method symbol
    (char :: <character>)
 => (token :: <symbol>, #rest val);
  add!(symbol-chars, char);

  let new-char = peek(stream);
  select(new-char)
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '_',
    ':', '+', '-', '*', '/', '=', '<', '>',
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9' =>
      symbol(read-element(stream));
    otherwise =>
      values(#"symbol",
             as(<symbol>, as(<string>, symbol-chars)));
  end select; 
end,
          
method vbarsymbol
    ()
 => (token :: <symbol>, #rest val);
  let new-char = read-element(stream);
  select(new-char)
    '|' =>
      values(#"symbol",
             as(<symbol>, as(<string>, symbol-chars)));
    otherwise =>
      add!(symbol-chars, new-char);
      vbarsymbol();
  end select; 
end
          ;
  state0();
end function;
          
define function read-grammar
    (stream :: <stream>)
 => (entry-points :: <sequence>,
     terminal-infos :: <object-table>,
     nonterminal-infos :: <object-table>,
     productions :: <sequence>);

  let entry-points = make(<stretchy-vector>);
  let terminal-infos = make(<object-table>);
  let nonterminal-infos = make(<object-table>);
  let productions = make(<stretchy-vector>);

  let terminal-count = 0;

  block(done)
    while(#t)
      let (token, value) = get-token(stream);
      select(token)
        #"%%", #"EOF" =>
          done();
        #"keyword" =>
          select(value)
            
#"entry-point" =>
  let (token, symbol) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :entry-point declaration");
  end if;
  add!(entry-points, symbol);
          
#"token" =>
  let (token, name) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :token declaration");
  end if;
  let (token, type) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :token declaration");
  end if;
  let terminal-info = make(<parsergen-terminal>,
                           number: terminal-count,
                           type: type);
  terminal-infos[name] := terminal-info;
  terminal-count := terminal-count + 1;
          
#"union" =>
  let (token, name) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :union declaration");
  end if;
  let (token, type) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :union declaration");
  end if;
  if(get-token(stream) ~== #"lparen")
    error("bad :union declaration");
  end if;

  let components = make(<stretchy-vector>);
  block(components-done)
    while(#t)
      let(token, symbol) = get-token(stream);
      if(token == #"rparen")
        components-done();
      elseif(token == #"symbol")
        add!(components, symbol);
      else
        error("expected a symbol or ) here");
      end if;
    end while;
  end block;
  
  let terminal-info = make(<parsergen-union>,
                           components: components,
                           type: type);
  terminal-infos[name] := terminal-info;
          
#"type" =>
  let (token, name) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :type declaration");
  end if;
  let (token, type) = get-token(stream);
  if(token ~== #"symbol")
    error("bad :type declaration");
  end if;
  let nonterminal-info = make(<parsergen-nonterminal>,
                              type: type);
  nonterminal-infos[name] := nonterminal-info;
          
            otherwise =>
              error("Unexpected token :%s", value);
          end select;
        #"symbol" =>
          
let nonterminal = value;
if(get-token(stream) ~== #"lparen")
  error("expected ( here");
end if;

let derives = make(<stretchy-vector>);
block(derives-done)
  while(#t)
    let(token, symbol) = get-token(stream);
    if(token == #"rparen")
      derives-done();
    elseif(token == #"symbol")
      add!(derives, symbol);
    else
      error("expected a symbol or ) here");
    end if;
  end while;
end block;
let action = make(<stretchy-vector>);
for(line = read-line(stream) then read-line(stream), until: line = "%")
  add!(action, line);
end for;
let production
  = make(<parsergen-production>,
         nonterminal: nonterminal,
         derives: derives,
         action: action,
         number: productions.size);
add!(productions, production);
          
        otherwise =>
          error("Unexpected token %=", token);
      end select;
    end while;
  end block;

  values(entry-points, terminal-infos, nonterminal-infos, productions);
end function;
          
define method print-message
    (production :: <parsergen-production>, stream :: <stream>)
 => ();
  format(stream, "%s (", as(<string>, production.production-nonterminal));
  for (symbol in production.production-derives, first? = #t then #f)
    unless (first?) write-element(stream, ' ') end;
    write(stream, as(<string>, symbol))
  end for;
  write-element(stream, ')');
end;
          
define function output-parse-tables
    (automaton :: <lr-parser-automaton>,
     entry-points :: <sequence>,
     terminal-infos :: <object-table>,
     nonterminal-infos :: <object-table>,
     productions :: <sequence>,
     output-stream :: <stream>)
 => ();
  
format(output-stream, "define constant $action-bits = 2;\n");
format(output-stream,
       "define constant $action-mask = ash(1, $action-bits) - 1;\n\n");
format(output-stream, "define constant $error-action = 0;\n");
format(output-stream, "define constant $accept-action = 1;\n");
format(output-stream, "define constant $reduce-action = 2;\n");
format(output-stream, "define constant $shift-action = 3;\n\n");
            
  
let row-size
  = for(info in terminal-infos,
        count = 0
          then if(instance?(info, <parsergen-terminal>))
                 count + 1
               else
                 count
               end)
    finally
      count
    end for;
let rows = make(<stretchy-vector>);
let state-infos = make(<object-table>);
local
  method compute-row
      (state :: <object>)
   => (number :: <integer>)
    let info = element(state-infos, state, default: #f);
    if(info)
      info.state-number;
    else
      let new-row = make(<simple-object-vector>,
                         size: row-size,
                         fill: 0);
      let new-info = make(<parsergen-state-info>,
                          action-row: new-row,
                          number: rows.size);

      add!(rows, new-row);
      state-infos[state] := new-info;

      
local
  set-row-entry!(terminal :: <object>, entry :: <integer>) => ();
    let terminal-info = element(terminal-infos, terminal, default: #f);
    unless(terminal-info)
      error("(presumed) terminal %s was not declared", terminal);
    end unless;
    if(instance?(terminal-info, <parsergen-terminal>))
      if(new-row[terminal-info.terminal-number] ~= 0)
        error("token/union conflict on token %s", terminal);
      end if;
      new-row[terminal-info.terminal-number] := entry;
    else
      for(union-terminal in terminal-info.union-components)
        set-row-entry!(union-terminal, entry);
      end for;
    end if;
  end method;
let terminals = lr-parser-automaton-transition-terminals(automaton, state);
if(terminals)
  for(terminal in terminals)
    let (action, data)
      = lr-parser-automaton-terminal-transition(automaton, state, terminal);
    let entry
      = select(action)
          #"shift" =>   logior(ash(compute-row(data), 2), 3);
          #"reduce" =>  logior(ash(data.production-number, 2), 2);
          #"accept" =>  1;
          #"error" =>   0;
        end select;
    set-row-entry!(terminal, entry);
  end for;
end if;
            
      
let transition-nonterminals
  = lr-parser-automaton-transition-nonterminals(automaton, state);
if(transition-nonterminals)
  for(nonterminal in transition-nonterminals)
    let nonterminal-info
      = element(nonterminal-infos, nonterminal, default: #f)
      | (nonterminal-infos[nonterminal] := make(<parsergen-nonterminal>));

    let goto-state = lr-parser-automaton-nonterminal-transition
                       (automaton, state, nonterminal);
    nonterminal-info.nonterminal-goto-transitions
      := pair(pair(new-info.state-number, compute-row(goto-state)),
              nonterminal-info.nonterminal-goto-transitions);
  end for;
end if;
            
      new-info.state-number;
    end if;
  end method;
for(entry-point in entry-points)
  compute-row(lr-parser-automaton-initial-state(automaton, entry-point));
end for;
            
  
format(output-stream, "define constant $action-table\n");
format(output-stream, "  = #[\n      ");
for(row in rows, index from 0)
  if(index > 0)
    format(output-stream, ",\n      ");
  end if;
  format(output-stream, "%=", row);
end for;
format(output-stream, "\n     ];\n\n");

for(entry-point in entry-points)
  let initial-state
    = lr-parser-automaton-initial-state(automaton, entry-point);
  format(output-stream, "define constant $%s-start-state = %d;\n",
         entry-point,
         state-infos[initial-state].state-number);
end for;
            
  
for(production in productions)
  format(output-stream, "\ndefine function production-%d\n",
         production.production-number);
  format(output-stream, "    (prev-state :: <integer>");
  format(output-stream, ", srcloc-@0 :: <source-location>");

  for(symbol in production.production-derives, index from 1)
    format(output-stream, ",\n     rhs-%%%d", index);
    let terminal-info = element(terminal-infos, symbol, default: #f);
    if(terminal-info & terminal-info.symbol-type)
      format(output-stream, " :: %s", terminal-info.symbol-type);
    else
      let nonterminal-info = element(nonterminal-infos, symbol, default: #f);
      if(nonterminal-info & nonterminal-info.symbol-type)
        format(output-stream, " :: %s", nonterminal-info.symbol-type);
      end if;
    end if;
    format(output-stream, ", srcloc-@%d :: <source-location>", index);
  end for;

  format(output-stream, ")\n");
  format(output-stream, " => (goto-state :: <integer>, value");
  let nonterminal-info = element(nonterminal-infos,
                                 production.production-nonterminal,
                                 default: #f);
  if(nonterminal-info & nonterminal-info.symbol-type)
    format(output-stream, " :: %s", nonterminal-info.symbol-type);
  end if;
  format(output-stream, ");\n");

  format(output-stream, "  // %s (", production.production-nonterminal);
  for(symbol in production.production-derives, first? = #t then #f)
    format(output-stream, if(first?) "%s" else " %s" end, symbol);
  end for;
  format(output-stream, ")\n");

  format(output-stream, "  values(");
  
let nonterminal = production.production-nonterminal;
let nonterminal-transitions
  = nonterminal-infos[nonterminal].nonterminal-goto-transitions;
if(begin
    let ft = nonterminal-transitions.first.tail;
    every?(method(trans) trans.tail = ft end, nonterminal-transitions);
   end)
  format(output-stream, "%d", nonterminal-transitions.first.tail)
elseif(nonterminal-transitions.size <= 8)
  format(output-stream, "select(prev-state by \\=)\n");
  for(transition in nonterminal-transitions)
    format(output-stream, "           %d => %d;\n",
           transition.head, transition.tail);
  end for;
  format(output-stream, "         end");
else
  let vec = make(<simple-object-vector>,
                 size: state-infos.size, fill: 0);
  for(transition in nonterminal-transitions)
    vec[transition.head] := transition.tail;
  end for;
  format(output-stream, "%=[prev-state]", vec);
end if;
            
  format(output-stream, ",\n");
  format(output-stream, "         begin\n");
  
for(line in production.production-action)
  write(output-stream, "           ");
  for(c in line)
    if(c == '%')
      write(output-stream, "rhs-%");
    elseif(c == '@')
      write(output-stream, "srcloc-@");
    else
      write-element(output-stream, c);
    end if;
  end for;
  new-line(output-stream);
end for;
            
  format(output-stream, "         end);\n");
  
  format(output-stream, "end function;\n");
end for;
            
  
format(output-stream,
       "define constant $production-table :: <simple-object-vector>\n");
format(output-stream,
       " = vector(");
for(production in productions, index from 0)
  if(index > 0)
    format(output-stream, ",\n          ");
  end if;
  format(output-stream, "production-%d", index);
end for;
format(output-stream, ");\n\n");
            
  
format(output-stream, "define constant $number-of-pops\n = #[");
for(production in productions, index from 0)
  if(index > 0)
    format(output-stream, ", ");
    if(modulo(index, 16) = 0)
      format(output-stream, "\n     ");
    end if;
  end if;
  format(output-stream, "%d", production.production-derives.size);
end for;
format(output-stream, "];\n");
            
end function;
          
define class <parsergen-state-info> (<object>)
  constant slot state-action-row :: <simple-object-vector>,
    required-init-keyword: action-row:;
  constant slot state-number :: <integer>,
    required-init-keyword: number:;
end class;
            
define function main(name :: <string>, arguments :: <sequence>)
  if(arguments.size < 1 | arguments.size > 3)
    format(*standard-error*,
           "usage: parsergen input.input [output.dylan [verbose.log]]\n");
    exit-application(1);
  end if;

  let reduce/reduce-errors = 0;
  let shift/reduce-errors = 0;

  block()
    let input = element(arguments, 0);
    let input-stream = make(<file-stream>, locator: input);

    let output-stream
      = if(arguments.size > 1)
          let output = element(arguments, 1);
          make(<file-stream>, direction: #"output", locator: output);
        else
          *standard-output*;
        end if;

    let verbose-stream
      = if(arguments.size > 2)
          let verbose = element(arguments, 2);
          make(<file-stream>, direction: #"output", locator: verbose);
        else
          #f;
        end if;

    format(*standard-error*, "Copying header\n");
    force-output(*standard-error*);
    copy-header(input-stream, output-stream);

    format(*standard-error*, "Parsing input\n");
    force-output(*standard-error*);
    let (entry-points, terminal-infos, nonterminal-infos, productions)
      = read-grammar(input-stream);
    unless(element(terminal-infos, #"EOF", default: #f))
      error("Token EOF must be defined");
    end unless;

    format(*standard-error*, "Generating automaton\n");
    force-output(*standard-error*);
    let grammar = make(<grammar>, productions: productions);
    begin
      let handler (<parser-automaton-reduce/reduce-error>) =
      method (condition :: <parser-automaton-reduce/reduce-error>,
              next-handler)
        format(*standard-error*,
               "reduce/reduce error on token %s, productions:\n",
               condition.parser-automaton-error-inputs.first);
        for (production in condition.parser-automaton-error-productions)
          format(*standard-error*, "  %s\n", production);
        end;
        reduce/reduce-errors := reduce/reduce-errors + 1;
        signal(make(<parser-automaton-reduce/reduce-restart>,
                    action:
                      condition.parser-automaton-error-productions.second));
      end method;

      let handler (<parser-automaton-shift/reduce-error>) =
      method (condition :: <parser-automaton-shift/reduce-error>,
              next-handler)
        format(*standard-error*,
               "shift/reduce error on token %s (choosing shift), "
                 "productions:\n",
               condition.parser-automaton-error-inputs.first);
        for (production in condition.parser-automaton-error-productions)
          format(*standard-error*, "  %s\n", production);
        end;
        shift/reduce-errors := shift/reduce-errors + 1;

        signal(make(<parser-automaton-shift/reduce-restart>,
                    action: #"shift"));
      end method;

      let automaton = make(<parser-automaton>,
                           grammar: grammar,
                           start-symbols: entry-points, end-symbol: #"EOF",
                           class: #"LALR-1", full-lookahead?: #t);
  
      format(*standard-error*, "Writing output\n");
      force-output(*standard-error*);
      output-parse-tables(automaton, entry-points,
                          terminal-infos, nonterminal-infos,
                          productions, output-stream);
    end;

    format(*standard-error*, "Copying trailer\n");
    force-output(*standard-error*);
    copy-trailer(input-stream, output-stream);

  exception (condition :: <error>)
    format(*standard-error*, "parsergen: %s\n", condition);
    exit-application(1);
  end block;

  if(reduce/reduce-errors > 0)
    exit-application(1);
  end if;

  exit-application(0);
end function;
          
main(application-name(), application-arguments());
          
