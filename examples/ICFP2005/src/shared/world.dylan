module: world

define class <world> (<object>)
end <world>;

define sealed domain make(singleton(<world>));
define sealed domain initialize(<world>);

define method initialize(world :: <world>, #key)
  format-out("initializing world\n");
end initialize;
