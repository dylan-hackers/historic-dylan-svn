module: board

define constant <coordinate> = limited(<integer>, min: 0);

define class <point> (<object>)
  slot x :: <coordinate>, required-init-keyword: x:; 
  slot y :: <coordinate>, required-init-keyword: y:;
end class <point>;
