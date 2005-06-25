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
  slot node-name :: <string>, required-init-keyword: name:;
  slot node-tag  :: <string>, required-init-keyword: tag:;
  slot node-x    :: <string>, required-init-keyword: x:;
  slot node-y    :: <string>, required-init-keyword: y:;
end class;

define class <edge> (<object>)
  slot edge-start :: <string>, required-init-keyword: start:;
  slot edge-end   :: <string>, required-init-keyword: end:;
  slot edge-type  :: <string>, required-init-keyword: type:;
end class;

define class <bank> (<object>)
  slot bank-location :: <string>, required-init-keyword: location:;
  slot bank-money    :: <string>, required-init-keyword: money:;
end;

define class <evidence> (<object>)
  slot evidence-location :: <string>, required-init-keyword: location:;
  slot evidence-world    :: <string>, required-init-keyword: world:;
end;

define class <player> (<object>)
  slot player-name     :: <string>, required-init-keyword: name:;
  slot player-location :: <string>, required-init-keyword: location:;
  slot player-type     :: <string>, required-init-keyword: type:;
end;

define class <plan> (<object>)
  slot plan-bot      :: <string>, required-init-keyword: bot:;
  slot plan-location :: <string>, required-init-keyword: location:;
  slot plan-type     :: <string>, required-init-keyword: type:;
  slot plan-world    :: <integer>, required-init-keyword: world:;
end class;

define class <inform> (<plan>)
  slot inform-certainty :: <integer>, required-init-keyword: certainty:;
end;

define method make (plan == <plan>,
                          #next next-method,
                          #rest rest,
                          #key world,
                          #all-keys) => (res :: <plan>)
  let args = rest;
  if (instance?(world-number, <string>))
    args := exclude(args, #"world");
    world := string-to-integer(world);
  end if;
  apply(next-method, plan, world: world, args);
end method;

define method make (inform == <inform>,
                    #next next-method,
                    #rest rest,
                    #key certainty,
                    world,
                    #all-keys) => (res :: <inform>)
  let args = rest;
  if (instance?(certainty, <string>))
    args := exclude(args, #"certainty");
    certainty := string-to-integer(certainty);
  end if;
  if (instance?(world, <string>))
    args := exclude(args, #"world");
    world := string-to-integer(world);
  end if;
  apply(next-method, inform, certainty: certainty,
        world: world, args);
end method;

define method exclude (list, symbol) => (sequence)
  let res = make(<stretchy-vector>);
  for (i from 0 below list.size by 2)
    if (list[i] ~= symbol)
      add!(res, list[i]);
      add!(res, list[i + 1]);
    end if;
  end for;
  res;
end method;

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
