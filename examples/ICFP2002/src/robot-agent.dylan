module: client

define abstract class <robot-agent>(<object>)
  slot robot :: <robot>, required-init-keyword: robot:;
end class <robot-agent>;

define method generate-next-move(me :: <robot-agent>, state :: <state>) => (c :: <command>)
end method generate-next-move;
