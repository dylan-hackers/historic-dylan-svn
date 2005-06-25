module: world

define class <world-skeleton> (<object>)
  slot my-name;
  slot robber;
  slot cops;
  slot nodes;
  slot edges;
end;

define class <world> (<object>)
  slot world;
  slot loot;
  slot banks;
  slot evidences;
  slot distance;
  slot players :: <collection>;
  slot world-skeleton :: <world-skeleton>;
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

define class <bank> (<object>)
  slot location :: <string>, init-keyword: location:;
  slot money :: <string>, init-keyword: money:;
end;

define class <evidence> (<object>)
  slot location :: <string>, init-keyword: location:;
  slot world :: <string>, init-keyword: world:;
end;

define class <player> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot location :: <string>, init-keyword: location:;
  slot type :: <string>, init-keyword: type:;
end;

define class <plan> (<object>)
  slot bot, init-keyword: bot:;
  slot location, init-keyword: location:;
  slot type, init-keyword: type:;
  slot world, init-keyword: world:;
end class;

define class <inform> (<plan>)
  slot certainty, init-keyword: certainty:;
end;

define method regexp-match(big :: <string>, regex :: <string>) => (#rest results);
  let (#rest marks) = regexp-position(big, regex);
  let result = make(<stretchy-vector>);

  if(marks[0])
    for(i from 0 below marks.size by 2)
      if(marks[i] & marks[i + 1])
        result := add!(result, copy-sequence(big, start: marks[i], end: marks[i + 1]))
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
  dbg("line: %s\n", line);
  let (match, #rest substrings) = regexp-match(line, regex);
  //dbg("RE: %= %= %=\n", regex, line, match);
  unless (match) signal(make(<parse-error>)) end;
  apply(values, substrings)
end;

define constant ws-re   = "[ \t]";
define constant name-re = "([-a-zA-Z0-9_#()]+)";
define constant node-tag = "(hq|bank|robber-start|ordinary)";
define constant edge-type-re = "(car|foot)";
define constant number-re = "([0-9]+)";
define constant negnumber-re = "(-?[0-9]+)";
define constant ptype-re = "(cop-foot|cop-car|robber)";

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
                       list("nod:", name-re, node-tag, number-re, number-re));
  re("edg\\\\");
  res.edges := collect(stream,
                       <edge>,
                       #(start:, end:, type:),
                       list("edg:", name-re, name-re, edge-type-re));
  re("wsk/");
  res;
end;

define method read-world (stream, skeleton)
  let res = make(<world>);
  let re = curry(re, stream);
  re("wor\\\\");
  res.world := re("wor:", number-re);
  res.loot := re("rbd:", number-re);
  re("bv\\\\");
  res.banks := collect(stream,
                       <bank>,
                       #(location:, money:),
                       list("bv:", name-re, number-re));
  re("ev\\\\");
  res.evidences := collect(stream,
                           <evidence>,
                           #(location:, world:),
                           list("ev:", name-re, number-re));
  res.distance := re("smell:", number-re);
  re("pl\\\\");
  res.players := collect(stream,
                         <player>,
                         #(name:, location:, type:),
                         list("pl:", name-re, name-re, ptype-re));
  re("wor/");
  res.world-skeleton := skeleton;
  res;
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
  res;
end;

define function intermingle (#rest sequences)
  apply(concatenate, apply(map,
                           method(#rest elements) elements end,
                           sequences));
end;

