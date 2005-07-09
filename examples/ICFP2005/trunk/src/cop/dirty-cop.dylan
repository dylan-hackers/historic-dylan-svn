module: dirty-cop

define class <dirty-cop> (<stupid-predicting-cop>)
end class;

register-bot(<dirty-cop>);

define method choose-move(agent :: <dirty-cop>, world :: <world>)
  let move = next-method();
  move.offer := "turncoat:";
  move;
end method;