module: world

lock-down
   <world-skeleton>, <world>, <node>, <edge>, <bank>, <evidence>,
   <player>, <plan>, <inform>, <parse-error>
end lock-down;

define class <world-skeleton> (<object>)
  slot my-name     :: <string>, required-init-keyword: my-name:;
  slot robber-name :: <string>, required-init-keyword: robber-name:;
  slot cop-names   :: <vec>, required-init-keyword: cop-names:;
  slot world-nodes :: <vec>, required-init-keyword: nodes:;
  slot world-edges :: <vec>, required-init-keyword: edges:;
end;

define class <world> (<object>)
  slot world-number         :: <integer>, required-init-keyword: number:;
  slot world-loot           :: <integer>, required-init-keyword: loot:;
  slot world-banks          :: <vec>, required-init-keyword: banks:;
  slot world-evidences      :: <vec>, required-init-keyword: evidences:;
  slot world-smell-distance :: <integer>, required-init-keyword: smell:;
  slot world-players        :: <vec>, required-init-keyword: players:;
  slot world-skeleton       :: <world-skeleton>, required-init-keyword: skeleton:;
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

define class <parse-error> (<error>)
end class;


define constant ws-re   = "[ \t]";
define constant name-re = "([-a-zA-Z0-9_#()]+)";
define constant node-tag = "(hq|bank|robber-start|ordinary)";
define constant edge-type-re = "(car|foot)";
define constant number-re = "([0-9]+)";
define constant negnumber-re = "(-?[0-9]+)";
define constant ptype-re = "(cop-foot|cop-car|robber)";


define method read-world-skeleton(stream :: <stream>)
  let re = curry(re, stream);

  re("wsk\\\\");
  let my-name = re("name:", name-re);
  let robber-name = re("robber:", name-re);
  let cop-names = make(<vector>, size: 5);
  for(i from 0 below 5)
    cop-names[i] := re("cop:", name-re);
  end;
  re("nod\\\\");
  let nodes =
    collect(stream,
            <node>,
            #(name:, tag:, x:, y:),
            list("nod:", name-re, node-tag, number-re, number-re));
  re("edg\\\\");
  let edges =
    collect(stream,
            <edge>,
            #(start:, end:, type:),
            list("edg:", name-re, name-re, edge-type-re));
  re("wsk/");

  make(<world-skeleton>,
       my-name: my-name,
       robber-name: robber-name,
       cop-names: cop-names,
       nodes: nodes,
       edges: edges);
end;


define method read-world (stream, skeleton)
  let re = curry(re, stream);
  re("wor\\\\");
  let world = re("wor:", number-re);
  let loot = re("rbd:", number-re);
  re("bv\\\\");
  let banks =
    collect(stream,
            <bank>,
            #(location:, money:),
            list("bv:", name-re, number-re));
  re("ev\\\\");
  let evidences =
    collect(stream,
            <evidence>,
            #(location:, world:),
            list("ev:", name-re, number-re));
  let smell = re("smell:", number-re);
  re("pl\\\\");
  let players =
    collect(stream,
            <player>,
            #(name:, location:, type:),
            list("pl:", name-re, name-re, ptype-re));
  re("wor/");

  make(<world>,
       number: string-to-integer(world),
       loot: string-to-integer(loot),
       banks: banks,
       evidences: evidences,
       smell: string-to-integer(smell),
       players: players,
       skeleton: skeleton);
end;
