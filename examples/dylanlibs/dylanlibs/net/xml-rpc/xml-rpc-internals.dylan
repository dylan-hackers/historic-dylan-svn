Module:    xml-rpc-internals
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

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
      values(index, copy-sequence(result()));
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

// Needs to be implemented properly
define method parse-response-double (string, #key start = 0, end: stop)
  local method is-not-special?(char :: <character>)
      char ~= '<';
  end method is-not-special?;

  with-collector into-buffer result like string, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(space, c);
      ["<double>", 
       loop([test(is-not-special?, c), do(collect(c))]),
       "</double>"];
      values(index, as(<string>, result()));
    end with-meta-syntax;  
  end with-collector;
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
       loop([parse-response-value(value), do(collect(value))]),	   
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
