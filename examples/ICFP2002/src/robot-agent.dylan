module: client

define abstract class <robot-agent>(<object>)
  slot id :: <integer>, required-init-keyword: id:;
end class <robot-agent>;

define method generate-next-move(me :: <robot-agent>, state :: <state>) => (c :: <command>)
end method generate-next-move;
