module: client

define abstract class <robot-agent>(<object>)
  slot capacity :: <integer>;
  slot board :: <board>;

  slot x :: <integer>;
  slot y :: <integer>;
  slot money :: <integer>;
  slot inventory :: limited(<vector>, of: <package>);
end class <robot-agent>;

define method move(me :: <robot-agent>, bid :: <integer>,
			      direction :: type-union(#"north", #"east",
						      #"south",
						      #"west"))
end method move;

define method run(me :: <robot-agent>)
end method run;
