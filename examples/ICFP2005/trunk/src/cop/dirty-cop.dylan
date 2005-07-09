module: dirty-cop

define class <dirty-cop> (<stupid-predicting-cop>)
end class;

register-bot(<dirty-cop>);

define method choose-move(agent :: <dirty-cop>, world :: <world>)
  let move = next-method();
  if (dirty-cop?(agent, world))
    make(<dirty-cop-move>,
         moves: move.moves);
  else
    move.offer := "turncoat:";
    move;
  end;
end method;