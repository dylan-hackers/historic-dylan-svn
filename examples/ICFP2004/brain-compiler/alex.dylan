module: assembler


define brain alex-gatherer

  [start:]
    Sense Here Home, (move-forward, stop);

  [move-forward:]
    Move start => stop;

  [stop:]
    Turn Left, (stop);
  
end;


alex-gatherer().dump-brain;
