module: client

define abstract class <robot-agent>(<object>)
  slot agent-id :: <integer>, required-init-keyword: agent-id:;
end class <robot-agent>;

define method generate-next-move(me :: <robot-agent>, state :: <state>) => (c :: <command>)
end method generate-next-move;
