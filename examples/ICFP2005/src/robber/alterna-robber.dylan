module: robber

define class <alterna-robber> (<robber>)
end class <alterna-robber>;

define method choose-move(robber :: <alterna-robber>, world :: <world>)
  let possible-nodes = map(target, generate-moves(robber.agent-player));
  
  local method evasive-score(target-node)
          reduce1(min, map(rcurry(distance, target-node), world.world-cops))
        end;

  let sorted-nodes = sort(possible-nodes, 
                          test: method(x, y)
                                    x.evasive-score > y.evasive-score
                                end);

  make(<move>, target: sorted-nodes[0], transport: "robber");
end method choose-move;

  

  
           
