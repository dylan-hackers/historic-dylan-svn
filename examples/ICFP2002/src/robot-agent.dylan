module: client

define abstract class <robot-agent> (<object>)
  slot capacity :: <integer>;
  slot board :: <board>;
  slot robot :: <robot>;
end class <robot-agent>;

define method do-something(me :: <robot-agent>) => (c :: <command>)
end method do-something;
