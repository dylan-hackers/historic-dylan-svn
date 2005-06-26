module: dylan-user

define library world
  use common-dylan;
  use Dylan, import: {Extensions};
  use io;
  use system;
  use regular-expressions;
  use collection-extensions;
  //use streams;
  export world;
end library;


define module world
  use common-dylan, exclude: {<string>};
  use format-out;
  use standard-io;
  use regular-expressions, import: {regexp-position};
  //use subseq, import: {subsequence};
  use extensions; //, import: {limited-vector-class};
  use streams;
  use simple-random, import: { random };
  use format, import: { format };

  export dbg,
    lock-down,
    maximum-node-id,
    find-node-by-id;

  export <world-skeleton>,
    my-name,
    robber-name,
    cop-names,
    world-nodes,
    world-nodes-by-id,
    world-edges;

  export <world>,
    world-number,
    world-loot,
    world-banks,
    world-evidences,
    world-smell-distance,
    world-other-cops,
    world-cops,
    world-my-player,
    world-robber,
    world-skeleton;

  export <node>,
    node-name,
    node-tag,
    node-x,
    node-y,
    moves-by-foot,
    moves-by-car,
    node-id;

  export <edge>,
    edge-start,
    edge-end,
    edge-type;

  export <bank>,
    bank-location,
    bank-money;

  export <evidence>,
    evidence-location,
    evidence-world;

  export <player>,
    player-name,
    player-location,
    player-type;

  export <plan>,
    plan-bot,
    plan-location,
    plan-type,
    plan-world;

  export <inform>,
    inform-certainty;

  export <from-message-inform>,
    sender,
    informs;

  export <from-message-plan>,
    sender,
    plans;

  export <parse-error>,
    read-world-skeleton, read-world;

  export generate-moves,
    generate-plan,
    generate-inform,
    generate-informs,
    normalize!,
    smelled-nodes,
    smelled-nodes-aux,
    choose-move,
    distance;    

  export next-move,
    random-player-move,
    advance-world;

  export drive-agent;

  export <agent>,
    agent-player;

  export <cop>,
    initial-transport,
    <robber>;

  export  <move>,
    target,
    transport,
    transport-setter;

  export make-informs, perceive-informs,
    make-plan, perceive-plans,
    make-vote, perceive-vote;

end module;
