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
                            
define sealed method functional-== (c == <point>, a :: <point>, b :: <point>)
 => (eq? :: <boolean>)
  a.val == b.val;
end method;

define sealed method \=(a :: <point>, b :: <point>)
 => (eq? :: <boolean>)
  a.val == b.val;
end method;

define method equal-hash (p :: <point>, s :: <hash-state>)
 => (i :: <integer>, s :: <hash-state>)
  equal-hash(p.val, s)
end method equal-hash;

define method print-object(p :: <point>, stream :: <stream>)
 => ();
  format(stream, "<point (%d,%d)>", p.x, p.y);
end;

