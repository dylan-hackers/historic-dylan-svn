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

define method parse-value-node-by-tag(tag == #"i4", node :: <node>)
  string-to-integer(node-value(node.node-first-child));
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"int", node :: <node>)
  string-to-integer(node-value(node.node-first-child));
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"boolean", node :: <node>)
  node-value(node.node-first-child)[0] == '1';
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"string", node :: <node>)
  node-value(node.node-first-child);
end method parse-value-node-by-tag;

define method string-to-float(s :: <string>) => (f :: <float>)
  local method is-digit?(ch :: <character>) => (b :: <boolean>)
    let v = as(<integer>, ch);
    v >= as(<integer>, '0') & v <= as(<integer>, '9');
  end method;
  let lhs = make(<stretchy-vector>);
  let rhs = make(<stretchy-vector>);
  let state = #"start";
  let sign = 1;

  local method process-char(ch :: <character>)
    select(state)
      #"start" =>
        select(ch)
          '-' => 
            begin
              sign := -1;
              state := #"lhs";
            end;
          '+' =>
            begin
              sign := 1;
              state := #"lhs";
            end;
          '.' =>
            begin
              lhs := add!(lhs, '0');
              state := #"rhs";
            end;
          otherwise =>
            begin
              state := #"lhs";
              process-char(ch);
            end;
        end select;
      #"lhs" => 
        case
          is-digit?(ch) => lhs := add!(lhs, ch);
          ch == '.' => state := #"rhs";
          otherwise => error("Invalid floating point value.");
        end case;
      #"rhs" =>
        case
          is-digit?(ch) => rhs := add!(rhs, ch);
          otherwise => error("Invalid floating point value.");
        end case;
      otherwise => error("Invalid state while parsing floating point.");
    end select;
  end method;

  for(ch in s)
    process-char(ch);
  end for;

  let lhs = as(<string>, lhs);
  let rhs = if(empty?(rhs)) "0" else as(<string>, rhs) end;
  (string-to-integer(lhs) * sign) +
    as(<double-float>, string-to-integer(rhs) * sign) /
    (10 ^ rhs.size); 
end method string-to-float;

define method parse-value-node-by-tag(tag == #"double", node :: <node>)
  string-to-float(node-value(node.node-first-child));
end method parse-value-node-by-tag;

// TODO: Convert to date/time
define method parse-value-node-by-tag(tag == #"dateTime.iso8601", node :: <node>)
  node-value(node.node-first-child);
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"base64", node :: <node>)
  make(<base64>, string: node-value(node.node-first-child));
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"struct", node :: <node>)
  let result = make(<string-table>);
  let members = node-select-nodes(node, "member");
  for(member in members)
    let name = node-select-single-node(member, "name").node-first-child.node-value;
    let value = parse-value-node(node-select-single-node(member, "value").node-child-nodes.first);
    result[name] := value;
  end for;
  result;
end method parse-value-node-by-tag;

define method parse-value-node-by-tag(tag == #"array", node :: <node>)
  let result = make(<stretchy-vector>);
  let values = node-select-nodes(node, "data/value");
  for(v in values)
    let value = parse-value-node(v.node-child-nodes.first);
    result := add!(result, value);
  end;
  result;
end method parse-value-node-by-tag;

define method parse-value-node(node :: <element>)
  values(parse-value-node-by-tag(as(<symbol>, element-tag-name(node)), node), node);
end method parse-value-node;

define method parse-value-node(node :: <text>)
  values(node-value(node), node);
end method parse-value-node;

define method parse-response(response :: <string>) => (r :: <object>)
  let document = make-document();
  if(document-load-xml(document, response))    
    let de = document-element(document);
    let faults = node-select-nodes(de, "fault");
    if(empty?(faults))    
      let value = node-select-single-node(de, "/methodResponse/params/param/value");
      parse-value-node(node-child-nodes(value).first);
    else
      let fault = parse-value-node(faults[0].node-first-child.node-first-child);
      error(make(<xml-rpc-condition>, fault-code: fault["faultCode"], fault-string: fault["faultString"]));
    end if;
  else
    error("Could not load response xml");
  end if;
end method parse-response;

