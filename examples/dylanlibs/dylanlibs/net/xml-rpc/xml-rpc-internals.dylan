Module:    xml-rpc-internals
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

// Encode/Decode a string so it is safe to pass inside XML
define method encode-string(s :: <string>) => (r :: <string>)
  let v = make(<stretchy-vector>);
  for(ch in s)
    select(ch)
      '<' => begin
               v := add!(v, '&');
               v := add!(v, 'l');
               v := add!(v, 't');
               v := add!(v, ';');
             end;
      '>' => begin
               v := add!(v, '&');
               v := add!(v, 'g');
               v := add!(v, 't');
               v := add!(v, ';');
             end;
      '&' => begin
               v := add!(v, '&');
               v := add!(v, 'a');
               v := add!(v, 'm');
               v := add!(v, 'p');
               v := add!(v, ';');
             end;
      otherwise => v := add!(v, ch);
    end;       
  finally 
    as(<string>, v);
  end for;  
end method encode-string;

define method decode-string(s :: <string>) => (r :: <string>)
  let result = make(<stretchy-vector>);
  let state = #"normal";
  local method process-state(ch)
    select(state)
      #"normal" => 
        if(ch == '&')
          state := #"encoding";
        else
          result := add!(result, ch);
        end if;
      #"encoding" =>
        select(ch)
          'a' => state := #"amp1";
          'g' => state := #"gt1";
          'l' => state := #"lt1";
          otherwise => 
            begin
              state := #"normal";
              result := add!(result, '&');
              process-state(ch);
            end;
        end;
      #"amp1" =>
        if(ch == 'm')
          state := #"amp2";
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'a');
          process-state(ch);
        end if;
      #"amp2" => 
        if(ch == 'p')
          state := #"amp3";
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'a');
          result := add!(result, 'm');
          process-state(ch);
        end if;
      #"amp3" => 
        if(ch == ';')
          state := #"normal";
          result := add!(result, '&');
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'a');
          result := add!(result, 'm');
          result := add!(result, 'p');
          process-state(ch);
        end if;
      #"gt1" =>
        if(ch == 't')
          state := #"gt2";
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'g');
          process-state(ch);
        end if;
      #"gt2" => 
        if(ch == ';')
          state := #"normal";
          result := add!(result, '>');
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'g');
          result := add!(result, 't');
          process-state(ch);
        end if;
      #"lt1" =>
        if(ch == 't')
          state := #"lt2";
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'l');
          process-state(ch);
        end if;
      #"lt2" => 
        if(ch == ';')
          state := #"normal";
          result := add!(result, '<');
        else
          state := #"normal";
          result := add!(result, '&');
          result := add!(result, 'l');
          result := add!(result, 't');
          process-state(ch);
        end if;
      otherwise => error("Unknown state");
    end select;
  end method;

  for(n from 0 below s.size)
    let ch = s[n];
    process-state(ch);
  finally
    as(<string>, result);
  end for;                       
end method decode-string;

define constant <letter> = 
    one-of('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
           'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
           'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
           'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

define constant <digit> = 
    one-of('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

define constant <space> =
  one-of(as(<character>, #x20), as(<character>, #x9), as(<character>, #xd),
         as(<character>, #x0a));

define constant <version-number> =
  type-union(<letter>, <digit>, one-of('_', '.', ':'));

// This is the high level function to call to parse a 
// <methodResponse>...</methodResponse> from an XML-RPC server.
// 
// Technically an XML-RPC response, to be XML compliant, cannot start
// with anything before the xml declaration if the xml declaration
// exists. Unforunately some servers have white space before this
// declaration when they return the response so I accept white space.
define method parse-response(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, xml-decl, response, misc, space);
    [loop(parse-s(space)), {[parse-xml-decl(xml-decl), loop(parse-s(space))], []},
     parse-method-response(response)];
    values(index, response);
  end with-meta-syntax;
end method parse-response;

define method parse-xml-decl(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, version-info, encoding-info, space);
    ["<?xml",
     loop(parse-s(space)),
     parse-version-info(version-info),     
     loop(parse-s(space)),
     { [parse-encoding-info(encoding-info), loop(parse-s(space))], [] },
     "?>"];
    values(index, #t);
  end with-meta-syntax;
end method parse-xml-decl;

define method parse-encoding-info(string, #key start = 0, end: stop)
  local method is-not-single-quote?(char :: <character>)
      char ~= '\'';
  end method is-not-single-quote?;

  local method is-not-double-quote?(char :: <character>)
      char ~= '\"';
  end method is-not-double-quote?;

  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, space, eq, version-num);
    ["encoding",
     parse-eq(eq),
     {['\'',
       [test(is-not-single-quote?, c), loop(test(is-not-single-quote?, c))],
       '\''],
      ['\"',
       [test(is-not-double-quote?, c), loop(test(is-not-double-quote?, c))],
       '\"']}];
    values(index, #t);
  end with-meta-syntax;
end method parse-encoding-info;

define method parse-version-info(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, space, eq, version-num);
    ["version",
     parse-eq(eq),
     {['\'',
       parse-version-num(version-num),
       '\''],
      ['\"',
       parse-version-num(version-num),
       '\"']}];
    values(index, #t);
  end with-meta-syntax;
end method parse-version-info;

define method parse-eq(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, space1, space2);
    [loop(parse-s(space1)), 
     '=',
     loop(parse-s(space2))];
    values(index, #t);
  end with-meta-syntax;
end method parse-eq;

define method parse-version-num(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c);
    [loop(type(<version-number>, c))];
    values(index, #t);
  end with-meta-syntax;
end method parse-version-num;

define method parse-s(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c);
    [type(<space>, c), loop(type(<space>, c))];
    values(index);
  end with-meta-syntax;  
end method parse-s;

define method parse-method-response (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, params, fault);
    ["<methodResponse>", 
     loop(parse-s(space)),
     { parse-response-params(params), parse-response-fault(fault) },
     loop(parse-s(space)),
     "</methodResponse>"];
    values(index, params);
  end with-meta-syntax;  
end method parse-method-response;   

define method parse-response-params (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, param);
    ["<params>", 
     loop(parse-s(space)),
     parse-response-param(param),
     loop(parse-s(space)),
     "</params>"];
    values(index, param);
  end with-meta-syntax;  
end method parse-response-params;   

define method parse-response-param (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<param>", 
     loop(parse-s(space)),
     parse-response-value(value),
     loop(parse-s(space)),
     "</param>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-param;   

define method parse-response-value (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<value>", 
     loop(parse-s(space)),
     { parse-response-int(value),
       parse-response-i4(value),
       parse-response-boolean(value),
       parse-response-string(value),
       parse-response-double(value),
       parse-response-date-time(value),
       parse-response-base64(value),
       parse-response-struct(value),
       parse-response-array(value),
       parse-response-string-internal(value)
     },
     loop(parse-s(space)),
     "</value>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-value;   

define method parse-response-int (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<int>", 
     parse-response-integer-value(value),
     "</int>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-int;   

define method parse-response-i4 (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<i4>", 
     parse-response-integer-value(value),
     "</i4>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-i4;   

define method parse-response-boolean (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<boolean>", 
     { ['0', set!(value, #f)],
       ['1', set!(value, #t)] },
     "</boolean>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-boolean;   


define method parse-response-string-internal (string, #key start = 0, end: stop)
  local method is-not-special?(char :: <character>)
      char ~= '<';
  end method is-not-special?;

  with-collector into-buffer result like string, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, c);
      [loop([test(is-not-special?, c), do(collect(c))])];
      values(index, decode-string(result()));
    end with-meta-syntax;  
  end with-collector;
end method parse-response-string-internal;   


define method parse-response-string (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<string>", 
     parse-response-string-internal(value),
     "</string>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-string;   

define method parse-response-double-value(string, #key start = 0, end: stop)
  with-collector into-buffer result like string, collect: collect;
    with-collector into-buffer result2 like string, collect: collect2;
      with-meta-syntax parse-string (string, start: start, pos: index)
        variables(sign, c, space, value, state);
        [ set!(state, #"lhs"),
          {['-', set!(sign, -1) ],
           [ { '+', [] } , set!(sign, 1)]},
           loop({ ['.', set!(state, #"rhs") ],
                  [type(<digit>, c), do(if(state == #"lhs") collect(c) else collect2(c) end)]}),
         ];

        values(index, 
          begin
            let r1 = copy-sequence(result());
            let r2 = copy-sequence(result2());
            let r1 = if(empty?(r1)) "0" else r1 end;
            let r2 = if(empty?(r2)) "0" else r2 end;
            (string-to-integer(r1) * sign) + 
                       (as(<double-float>, (string-to-integer(r2) * sign)) / 
                         (10 ^ r2.size))
          end);
      end with-meta-syntax;  
    end with-collector;
  end with-collector;
end method parse-response-double-value;

define method parse-response-double (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<double>", 
     parse-response-double-value(value),
     "</double>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-double;   

// Needs to be implemented properly
define method parse-response-date-time (string, #key start = 0, end: stop)
  local method iso8601-to-date(iso8601)
    make(<date>, iso8601-string: remove!(iso8601, ':', count: #f));
  end method;

  local method is-not-special?(char :: <character>)
      char ~= '<';
  end method is-not-special?;

  with-collector into-buffer result like string, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, c);
      ["<dateTime.iso8601>", 
       loop([test(is-not-special?, c), do(collect(c))]),
       "</dateTime.iso8601>"];
      values(index, iso8601-to-date(copy-sequence(result())));
    end with-meta-syntax;  
  end with-collector;
end method parse-response-date-time;   

// Needs to be implemented properly
define method parse-response-base64 (string, #key start = 0, end: stop)
  local method is-not-special?(char :: <character>)
      char ~= '<';
  end method is-not-special?;

  with-collector into-buffer result like string, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, c);
      ["<base64>", 
       loop([test(is-not-special?, c), do(collect(c))]),
       "</base64>"];
      values(index, base64-decode(copy-sequence(result())));
    end with-meta-syntax;  
  end with-collector;
end method parse-response-base64;   

define method parse-response-integer-value(string, #key start = 0, end: stop)
  with-collector into-buffer result like string, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(sign, c, space, value);
      [{['-', set!(sign, -1) ],
        [{ '+', [] }, set!(sign, 1)]},
       type(<digit>, c), do(collect(c)), 
       loop([type(<digit>, c), do(collect(c))])];
      values(index, string-to-integer(result()) * sign);
    end with-meta-syntax;  
  end with-collector;
end method parse-response-integer-value;

define method parse-response-struct (string, #key start = 0, end: stop)
  local method value-to-table(value)
    let result = make(<string-table>);
    for(v in value)
      result[v.first] := v.second;
    end for;
    result
  end method;

  with-collector into-vector struct, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, value);
      ["<struct>", 
       loop(parse-s(space)),
       [parse-response-struct-member(value), do(collect(value)), loop(parse-s(space))],
       loop([parse-response-struct-member(value), do(collect(value)), loop(parse-s(space))]),
       "</struct>"];
      values(index, value-to-table(struct));
    end with-meta-syntax;  
  end with-collector;
end method parse-response-struct;   

define method parse-response-struct-member (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, name, value);
    ["<member>", 
     loop(parse-s(space)),
     parse-response-struct-member-name(name),
     loop(parse-s(space)),
     parse-response-struct-member-value(value),
     loop(parse-s(space)),
     "</member>"];
    values(index, vector(copy-sequence(name), value));
  end with-meta-syntax;  
end method parse-response-struct-member;   

define method parse-response-struct-member-name (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<name>", 
     parse-response-string-internal(value),
     "</name>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-struct-member-name;   

define method parse-response-struct-member-value (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    [parse-response-value(value)];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-struct-member-value;   

define method parse-response-array (string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, value);
    ["<array>", 
     loop(parse-s(space)),
     parse-response-array-data(value),
     loop(parse-s(space)),
     "</array>"];
    values(index, value);
  end with-meta-syntax;  
end method parse-response-array;   

define method parse-response-array-data (string, #key start = 0, end: stop)
  with-collector into-vector array, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, value);
      ["<data>",
       loop(parse-s(space)),
       loop([parse-response-value(value), do(collect(value)), loop(parse-s(space))]),	   
       loop(parse-s(space)),
       "</data>"];
      values(index, array);
    end with-meta-syntax;  
  end with-collector;
end method parse-response-array-data;   

define class <xml-rpc-condition> (<error>)
  slot xml-rpc-fault-code, init-keyword: fault-code:;
  slot xml-rpc-fault-string, init-keyword: fault-string:;  
end class <xml-rpc-condition>;

define method print-object(condition :: <xml-rpc-condition>, stream :: <stream>) => ()
  print(condition.object-class, stream);
  let text = concatenate(" - Fault Code: ",
	integer-to-string(condition.xml-rpc-fault-code),
	" (",
	condition.xml-rpc-fault-string,
	").");
  write(stream, text);
end method print-object;

define method parse-response-fault (string, #key start = 0, end: stop)
  local method make-fault(fc, fs)
    error(make(<xml-rpc-condition>, fault-code: fc, fault-string: fs));
  end method;

  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(space, fault-code, fault-string);
    ["<fault>", 
     loop(parse-s(space)),
     "<value>",
     loop(parse-s(space)),
     "<struct>",
     loop(parse-s(space)),
     "<member>",
     loop(parse-s(space)),
     "<name>faultCode</name>",
     loop(parse-s(space)),
     "<value>",
     loop(parse-s(space)),
	 { parse-response-int(fault-code), parse-response-i4(fault-code) },
     loop(parse-s(space)),
     "</value>",
     loop(parse-s(space)),
     "</member>",
     loop(parse-s(space)),
     "<member>",
     loop(parse-s(space)),
     "<name>faultString</name>",
     loop(parse-s(space)),
     "<value>",
     loop(parse-s(space)),
     { parse-response-string(fault-string), parse-response-string-internal(fault-string) },
     loop(parse-s(space)),
     "</value>",
     loop(parse-s(space)),
     "</member>",
     loop(parse-s(space)),
     "</struct>",
     loop(parse-s(space)),
     "</value>",
     loop(parse-s(space)),
     "</fault>"];
    values(index, make-fault(fault-code, copy-sequence(fault-string)));
  end with-meta-syntax;  
end method parse-response-fault;   
