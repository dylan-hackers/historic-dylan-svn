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

define function re (stream, #rest regexen)
  let regex = reduce1(method(x, y) concatenate(x, ws-re, y) end,
                      regexen);
  let line = read-line(stream);
  let (match, #rest substrings) = regexp-match(line, regex);
  //format-out("RE: %= %= %=\n", regex, line, match);
  unless (match) signal(make(<parse-error>)) end;
  apply(values, substrings)
end;

define constant ws-re   = "[ \t]";
define constant name-re = "([-a-zA-Z0-9_#()]+)";
define constant node-tag = "(hq|bank|robber-start|ordinary)";
define constant edge-type = "(car|foot)";
define constant coordinate = "([0-9]+)";

define method read-world-skeleton(stream :: <stream>)
  let re = curry(re, stream);
  let res = make(<world-skeleton>);

  re("wsk\\\\");
  res.my-name := re("name:", name-re);
  res.robber := re("robber:", name-re);
  res.cops := make(<stretchy-vector>);
  for(i from 0 below 5)
    add!(res.cops, re("cop:", name-re))
  end;
  re("nod\\\\");
  res.nodes := collect(stream,
                       <node>,
                       #(name:, tag:, x:, y:),
                       list("nod:", name-re, node-tag, coordinate, coordinate));
  re("edg\\\\");
  res.edges := collect(stream,
                       <edge>,
                       #(start:, end:, type:),
                       list("edg:", name-re, name-re, edge-type));
  re("wsk/");
end;

define function collect (stream, type, keywords, regexps)
  let res = make(<stretchy-vector>);
  block()
    while(#t)
      let (#rest substrings) = apply(re, stream, regexps);
      add!(res, apply(make, type,
                 intermingle(keywords, substrings)));
    end while;
  exception (condition :: <parse-error>)
  end;
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
