module: board

define constant <coordinate> = limited(<integer>, min: 0);

define functional sealed class <point> (<object>)
  slot val :: <integer>,
    required-init-keyword: value:;
end class <point>;

define sealed domain make(singleton(<point>));
define sealed domain initialize(<point>);


define inline function x(p :: <point>) => (x :: <coordinate>);
  logand(p.val, #xffff);
end;

define inline function y(p :: <point>) => (y :: <coordinate>);
  logand(ash(p.val, -16), #xffff);
end;

define sealed method point(#key x :: <coordinate> = 0, y :: <coordinate> = 0)
 => (res :: <point>);
  make(<point>, value: logior(ash(y, 16), logand(x, #xffff)));
end;

define inline sealed method \=(a :: <point>, b :: <point>)
 => b :: <boolean>;
  a.val == b.val;
end method;
