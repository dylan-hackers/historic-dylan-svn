module: cop
synopsis: 
author: 
copyright: 

define class <world-skeleton> (<object>)
  slot my-name;
  slot robber;
  slot cops;
  slot nodes;
  slot edges;
end;

define class <point> (<object>)
  slot x :: <integer>, init-keyword: x:;
  slot y :: <integer>, init-keyword: y:;
end class;

define class <node> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot tag :: <string>, init-keyword: tag:;
  slot x :: <string>, init-keyword: x:;
  slot y :: <string>, init-keyword: y:;
end class;

define class <edge> (<object>)
  slot start-location :: <string>, init-keyword: start:;
  slot end-location :: <string>, init-keyword: end:;
  slot type :: <string>, init-keyword: type:;
end class;

define method regexp-match(big :: <string>, regex :: <string>) => (#rest results);
  let (#rest marks) = regexp-position(big, regex);
  let result = make(<stretchy-vector>);

  if(marks[0])
    for(i from 0 below marks.size by 2)
      if(marks[i] & marks[i + 1])
        result := add!(result, subsequence(big, start: marks[i], end: marks[i + 1]))
      else
        result := add!(result, #f)
      end
    end
  end;
  apply(values, result)
end;

define class <parse-error> (<error>)
end class;

define method read-world-skeleton(stream :: <stream>)
  local method re(#rest regexen)
          let regex = apply(concatenate, regexen);
          let line = read-line(stream);
          let (match, #rest substrings) = regexp-match(line, regex);
          unless (match) signal(make(<parse-error>)) end;
          apply(values, substrings)
        end;

  let ws-re   = "[ \t]";
  let name-re = "([-a-zA-Z0-9_#()]+)";
  let node-tag = "(hq|bank|robber-start|ordinary)";
  let edge-type = "(car|foot)";
  let coordinate = "([0-9]+)";

  let res = make(<world-skeleton>);

  re("wsk\\\\");
  res.my-name := re("name:", ws-re, name-re);
  res.robber := re("robber:", ws-re, name-re);
  res.cops := make(<stretchy-vector>);
  for(i from 0 below 5)
    add!(res.cops, re("cop:", ws-re, name-re))
  end;
  re("nod\\\\");
  res.nodes := make(<stretchy-vector>);
  block()
    while (#t)
      let (#rest node) = re("nod:", ws-re,
                            name-re, ws-re,
                            node-tag, ws-re,
                            coordinate, ws-re,
                            coordinate);
      add!(res.nodes,
           apply(make, <node>,
                 intermingle(#(name:, tag:, x:, y:),
                             node)));
    end while
  exception (condition :: <parse-error>)
    //format-out("Parse error while parsing node!\n");
    //we assume we got "nod/"
  end;
  re("edg\\\\");
  res.edges := make(<stretchy-vector>);
  block()
    while(#t)
      let (#rest edge) = re("edg:", ws-re,
                            name-re, ws-re,
                            name-re, ws-re,
                            edge-type);
      add!(res.edges,
           apply(make, <edge>,
                 intermingle(#(start:, end:, type:),
                             edge)));
    end while;
  exception (condition :: <parse-error>)
    //format-out("Parse error while parsing edges!\n");
    //we assume we got "edg/"
  end;
  re("wsk/");
end;

define function intermingle (#rest sequences)
  apply(concatenate, apply(map,
                           method(#rest elements) elements end,
                           sequences));
end;

define function main(name, arguments)
  with-open-file(fs = "world-skeleton",
                 direction: #"input",
                 element-type: <character>)
    read-world-skeleton(fs);
  end;
 // exit-application(0);
end function main;

main(application-name(), application-arguments());
