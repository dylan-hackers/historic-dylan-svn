module: world

lock-down
   <world-skeleton>, <world>, <node>, <edge>, <bank>, <evidence>,
   <player>, <plan>, <inform>, <parse-error>
end lock-down;

define class <world-skeleton> (<object>)
  constant slot my-name     :: <string>, required-init-keyword: my-name:;
  constant slot robber-name :: <string>, required-init-keyword: robber-name:;
  constant slot cop-names   :: <vec>, required-init-keyword: cop-names:;
  constant slot world-nodes :: <table>, required-init-keyword: nodes:;
  constant slot world-edges :: <vec>, required-init-keyword: edges:;
  constant slot world-nodes-by-id :: <collection>, required-init-keyword: nodes-by-id:;
  constant slot worlds      :: <collection> = make(<stretchy-vector>);
end;

define class <world> (<object>)
  constant slot world-number            :: <integer>, required-init-keyword: number:;
  constant slot world-loot              :: <integer>, required-init-keyword: loot:;
  constant slot world-dirty-cops        :: <vec>, required-init-keyword: dirty-cops:;
  constant slot world-bot-takeover      :: <vec>, required-init-keyword: bot-takeover:;
  constant slot world-false-accusations :: <vec>, required-init-keyword: false-accusations:;
  constant slot world-banks             :: <vec>, required-init-keyword: banks:;
  constant slot world-evidences         :: <vec>, required-init-keyword: evidences:;
  constant slot world-smell-distance    :: <integer>, required-init-keyword: smell:;
  constant slot world-players           :: <vec>, required-init-keyword: players:;
  constant slot world-cops              :: <vec>, required-init-keyword: cops:;
  constant slot world-other-cops        :: <vec>, required-init-keyword: other-cops:;
  constant slot world-my-player         :: <player>, required-init-keyword: my-player:;
  constant slot world-my-players        :: <collection>, required-init-keyword: my-players:;
  constant slot world-robber            :: false-or(<player>), required-init-keyword: robber:;
  constant slot world-skeleton          :: <world-skeleton>, required-init-keyword: skeleton:;
  constant slot world-informs           :: <collection> = make(<stretchy-vector>);
end class;

define class <node> (<object>)
  constant slot node-name      :: <symbol>, required-init-keyword: name:;
  constant slot node-tag       :: <string>, required-init-keyword: tag:;
  constant slot node-x         :: <string>, required-init-keyword: x:;
  constant slot node-y         :: <string>, required-init-keyword: y:;
  constant slot moves-by-foot  :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot moves-by-car   :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot node-id        :: <integer>, required-init-keyword: node-id:;
end class;

define class <edge> (<object>)
  constant slot edge-start :: <string>, required-init-keyword: start:;
  constant slot edge-end   :: <string>, required-init-keyword: end:;
  constant slot edge-type  :: <string>, required-init-keyword: type:;
end class;

define class <bank> (<object>)
  constant slot bank-location :: <node>, required-init-keyword: location:;
  constant slot bank-money    :: <integer>, required-init-keyword: money:;
end;

define class <evidence> (<object>)
  constant slot evidence-location :: <node>, required-init-keyword: location:;
  constant slot evidence-world    :: <integer>, required-init-keyword: world:;
end;

define class <player> (<object>)
  constant slot player-name     :: <string>, required-init-keyword: name:;
  constant slot player-location :: <node>, required-init-keyword: location:;
  constant slot player-type     :: <string>, required-init-keyword: type:;
end;

define class <plan> (<object>)
  constant slot plan-bot      :: <string>, required-init-keyword: bot:;
  constant slot plan-location :: <node>, required-init-keyword: location:;
  constant slot plan-type     :: <string>, required-init-keyword: type:;
  constant slot plan-world    :: <integer>, required-init-keyword: world:;
end class;

define class <inform> (<plan>)
  constant slot inform-certainty :: <integer>, required-init-keyword: certainty:;
end;

define class <from-message-inform> (<object>)
  constant slot sender, required-init-keyword: sender:;
  constant slot informs, required-init-keyword: informs:;
end;

define class <from-message-plan> (<object>)
  constant slot sender, required-init-keyword: sender:;
  constant slot plans, required-init-keyword: plans:;
end;

define variable *world-skeleton* = #f;

define method make (evidence == <evidence>,
                    #next next-method,
                    #rest rest,
                    #key location, world,
                    #all-keys) => (res :: <evidence>)
  let args = rest;
  if (instance?(location, <string>))
    args := exclude(args, #"location");
    location := *world-skeleton*.world-nodes[as(<symbol>, location)];
  end if;
  if (instance?(world, <string>))
    args := exclude(args, #"world");
    world := string-to-integer(world);
  end if;
  apply(next-method, evidence, location: location, world: world, args);
end;

define method make (bank == <bank>,
                    #next next-method,
                    #rest rest,
                    #key location,
                    money,
                    #all-keys) => (res :: <bank>)
  let args = rest;
  if (instance?(location, <string>))
    args := exclude(args, #"location");
    location := *world-skeleton*.world-nodes[as(<symbol>, location)];
  end if;
  if (instance?(money, <string>))
    args := exclude(args, #"money");
    money := string-to-integer(money);
  end if;
  apply(next-method, bank, location: location, money: money, args);
end;

define method make (node == <node>,
                    #next next-method,
                    #rest rest,
                    #key name,
                    #all-keys) => (res :: <node>)
  let args = rest;
  if (instance?(name, <string>))
    args := exclude(args, #"name");
    name := as(<symbol>, name);
  end if;
  let res = apply(next-method, node, name: name,
                  node-id: next-node-id, args);
  next-node-id := next-node-id + 1;
  res;
end;

define method make (plan == <plan>,
                    #next next-method,
                    #rest rest,
                    #key world,
                    location,
                    #all-keys) => (res :: <plan>)
  let args = rest;
  if (instance?(world, <string>))
    args := exclude(args, #"world");
    world := string-to-integer(world);
  end if;
  if (instance?(location, <string>))
    args := exclude(args, #"location");
    location := *world-skeleton*.world-nodes[as(<symbol>, location)];
  end if;
  apply(next-method, plan, world: world, location: location, args);
end method;

define method make (inform == <inform>,
                    #next next-method,
                    #rest rest,
                    #key certainty,
                    world,
                    location,
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
  if (instance?(location, <string>))
    args := exclude(args, #"location");
    location := *world-skeleton*.world-nodes[as(<symbol>, location)];
  end if;
  apply(next-method, inform, certainty: certainty,
        world: world, location: location, args);
end method;

define method make (player == <player>,
                    #next next-method,
                    #rest rest,
                    #key location,
                    #all-keys) => (res :: <player>)
  let args = rest;
  if (instance?(location, <string>))
    args := exclude(args, #"location");
    location := *world-skeleton*.world-nodes[as(<symbol>, location)];
  end if;
  apply(next-method, player, location: location, args);
end method;

define method make (world == <world>,
                    #next next-method,
                    #rest rest,
                    #key players,
                    dirty-cops,
                    bot-takeover,
                    false-accusations,
                    number,
                    #all-keys) => (res :: <world>)
  let args = rest;
  args := exclude(args, #"players");
  let players = players;
  let all-players = copy-sequence(players);
  local method find-pl (name)
          block(return)
            for (element in all-players)
              if (element.player-name = name)
                return(element);
              end if;
            end for;
            #f;
          end block;
        end method;


  let my-player = find-pl(*world-skeleton*.my-name);
  players := remove!(players, my-player);

  let robber = find-pl(*world-skeleton*.robber-name);
  players := remove!(players, robber);

  players := sort!(players, test: method(x, y)
                                      x.player-name < y.player-name;
                                  end method);


  let cops = players;
  if (my-player.player-name = *world-skeleton*.robber-name)
    //we are the robber
    robber := my-player;
  else
    //add myself to the cops
    cops := add(players, my-player);
  end if;

  let number = number;
  if (instance?(number, <string>))
    args := exclude(args, #"number");
    number := string-to-integer(number);
  end if;

  if (number = 1) //first world
    robber := make(<player>,
                   name: *world-skeleton*.robber-name,
                   location: *world-skeleton*.world-nodes[#"54-and-ridgewood"],
                   type: "robber");
  end;

  let dirty-cops* = map(compose(find-pl, bot), dirty-cops); 
  args := exclude(args, #"dirty-cops");

  for (x in bot-takeover)
    x.taken-bot := find-pl(x.taken-bot);
    x.controller := find-pl(x.controller);
  end;

  let my-players = concatenate(list(my-player),
                               map(taken-bot,
                                   choose(method(x)
                                              x.controller = my-player
                                          end, bot-takeover)));
  
  for (x in false-accusations)
    x.accusing-bot := find-pl(x.accusing-bot);
    x.accused-bot := find-pl(x.accused-bot);
  end;

  apply(next-method,
        world,
        number: number,
        dirty-cops: dirty-cops*,
        cops: cops,
        players: all-players,
        robber: robber,
        my-player: my-player,
        my-players: my-players,
        other-cops: players,
        args);
end method;

define method find-player (world :: <world>, name :: <string>)
  block(return)
    for (element in world.world-players)
      if (element.player-name = name)
        return(element);
      end if;
    end for;
    #f;
  end block;
end method;

define method make (world-skeleton == <world-skeleton>,
                    #next next-method,
                    #rest rest,
                    #key nodes,
                    edges,
                    #all-keys) => (res :: <world-skeleton>)

  let args = rest;
  let nodes-table = make(<table>);
  let nodes-by-id = make(<stretchy-vector>);
  for (node in nodes)
    nodes-table[node.node-name] := node;
    add!(node.moves-by-foot, node);
    add!(node.moves-by-car, node);
    nodes-by-id[node.node-id] := node;
  end for;

  local method add-node (list, target-node)
          block(return)
            for (element in list)
              if (element.node-name = as(<symbol>, target-node))
                return();
              end if;
            end for;
            add!(list, nodes-table[as(<symbol>, target-node)]);
          end block;
        end method;

  for (edge in edges)
    if (edge-type(edge) = "foot")
      //foot-edges are possible to go by foot or by car in the right direction
      add-node(moves-by-foot(nodes-table[as(<symbol>, edge.edge-start)]),
               edge.edge-end);
      add-node(moves-by-car(nodes-table[as(<symbol>, edge.edge-start)]),
               edge.edge-end);
      //reverse direction, only by foot
      add-node(moves-by-foot(nodes-table[as(<symbol>, edge.edge-end)]),
               edge.edge-start);
    elseif (edge-type(edge) = "car")
      //shortcuts for cars
      add-node(moves-by-car(nodes-table[as(<symbol>, edge.edge-start)]),
               edge.edge-end);
    end;
  end for;

  args := exclude(args, #"nodes");
  apply(next-method, world-skeleton, 
        nodes: nodes-table,
        nodes-by-id: nodes-by-id,
        args);
end method;


define class <dirty-cops> (<object>)
  slot bot, required-init-keyword: bot:;
end class;

define class <bot-taken> (<object>)
  slot taken-bot, required-init-keyword: taken-bot:;
  slot controller, required-init-keyword: controller:;
end;

define class <false-accusation> (<object>)
  slot accusing-bot, required-init-keyword: accusing-bot:;
  slot accused-bot, required-init-keyword: accused-bot:;
  slot false-accusation-world :: <integer>, required-init-keyword: false-accusation-world:;
end;

define method make (false-accusation == <false-accusation>,
                    #next next-method,
                    #rest rest,
                    #key false-accusation-world,
                    #all-keys) => (res :: <false-accusation>)
  let args = rest;
  args := exclude(args, #"false-accusation-world");
  apply(next-method,
        false-accusation,
        false-accusation-world: string-to-integer(false-accusation-world),
        args);
end;

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

define method maximum-node-id () => (res :: <integer>)
  next-node-id;
end method;

define method find-node-by-id (id :: <integer>)
 => (node :: <node>);
  *world-skeleton*.world-nodes-by-id[id]
end method find-node-by-id;

define class <parse-error> (<error>)
end class;

define variable next-node-id :: <integer> = 0;

define constant ws-re   = "[ \t]";
define constant name-re = "([-a-zA-Z0-9_#()]+)";
define constant node-tag-re = "(hq|bank|robber-start|ordinary)";
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
            list("nod:", name-re, node-tag-re, number-re, number-re));
  re("edg\\\\");
  let edges =
    collect(stream,
            <edge>,
            #(start:, end:, type:),
            list("edg:", name-re, name-re, edge-type-re));
  re("wsk/");

  *world-skeleton* := make(<world-skeleton>,
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
  re("dc\\\\");
  let dirty-cops =
    collect(stream,
            <dirty-cops>,
            #(bot:),
            list("dc:", name-re));
  re("sc\\\\");
  let bot-takeover =
    collect(stream,
            <bot-taken>,
            #(controller:, taken-bot:),
            list("sc:", name-re, name-re));
  re("fac\\\\");
  let false-accusations =
    collect(stream,
            <false-accusation>,
            #(accusing-bot:, accused-bot:, false-accusation-world:),
            list("fac:", name-re, name-re, number-re));
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


  let res =
    make(<world>,
         number: world,
         loot: string-to-integer(loot),
         dirty-cops: dirty-cops,
         bot-takeover: bot-takeover,
         false-accusations: false-accusations,
         banks: banks,
         evidences: evidences,
         smell: string-to-integer(smell),
         players: players,
         skeleton: skeleton);
  skeleton.worlds[string-to-integer(world)] := res;
  res
end;

define method read-from-message-inform (stream)
  let res = make(<stretchy-vector>);
  let re = curry(re, stream);
  re("from\\\\");

  block()
    while(#t)
      let from-who = re("from:", name-re);
      re("inf\\\\");
      let infos = collect(stream,
                          <inform>,
                          #(bot:, location:, type:, world:, certainty:),
                          list("inf:", name-re, name-re, ptype-re, number-re, negnumber-re));
      add!(res, make(<from-message-inform>,
                     sender: from-who,
                     informs: infos));
    end while;
  exception (condition :: <parse-error>)
  end block;
  res;
end;

define method read-from-message-plan (stream)
  let res = make(<stretchy-vector>);
  let re = curry(re, stream);
  re("from\\\\");

  block()
    while(#t)
      let from-who = re("from:", name-re);
      re("plan\\\\");
      let infos = collect(stream,
                          <plan>,
                          #(bot:, location:, type:, world:),
                          list("plan:", name-re, name-re, ptype-re, number-re));
      add!(res, make(<from-message-plan>,
                     sender: from-who,
                     plans: infos));
    end while;
  exception (condition :: <parse-error>)
  end block;
  res;
end;

define method read-vote-tally (stream)
  let re = curry(re, stream);
  block()
    re("winner:", name-re);
  exception (cond :: <parse-error>)
    //no winner
  end;
end method;

define method read-accused (stream)
  let re = curry(re, stream);
  re("accused:");
end;

define method read-offered-cops (stream)
  let re = curry(re, stream);
  let res = #();
  block()
    re("nottelling:");
  exception(cond :: <parse-error>)
    res := collect(stream,
                   <dirty-cops>,
                   #(bot:),
                   list("ofc:", name-re));
    dbg("RES %=\n", res);
  end;
  res;
end;

