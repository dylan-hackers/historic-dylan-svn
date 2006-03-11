module: graph-viewer

// General vector math

define constant <v2> = <simple-double-float-vector>;

// what's the general definition of the cross product in an
// n-dimensional room?
define inline method cross-product
    (u :: <simple-double-float-vector>, v :: <simple-double-float-vector>)
 => (cross-product :: <simple-double-float-vector>)
  let result = make(type-for-copy(u), size: u.size);
  local method cp(i, j) u[i] * v[j] - v[i] * u[j] end;
  result[0] := cp(1, 2);
  result[1] := cp(2, 0);
  result[2] := cp(0, 1);
  result;
end method cross-product;

define sealed inline method \+(u :: <simple-double-float-vector>, v :: <simple-double-float-vector>)
 => (sum :: <simple-double-float-vector>)
  map(\+, u, v)
end method;

define sealed inline method \-(u :: <simple-double-float-vector>, v :: <simple-double-float-vector>)
 => (difference :: <simple-double-float-vector>)
  map(\-, u, v)
end method;

define sealed inline method negate(u :: <simple-double-float-vector>)
 => (negation :: <simple-double-float-vector>)
  u * -1.0d0
end method;

define sealed inline method \*(u :: <simple-double-float-vector>, v :: <double-float>)
 => (product :: <simple-double-float-vector>)
  map(rcurry(\*, v), u)
end method;

define sealed inline method \*(u :: <double-float>, v :: <simple-double-float-vector>)
 => (product :: <simple-double-float-vector>)
  map(curry(\*, u), v)
end method;

define sealed inline method \*(u :: <simple-double-float-vector>, v :: <simple-double-float-vector>)
 => (dot-product :: <double-float>)
  u[0] * v[0] + u[1] * v[1];
  //reduce1(\+, map(\*, u, v))
end method;

define sealed inline method \/(u :: <simple-double-float-vector>, v :: <double-float>)
 => (product :: <simple-double-float-vector>)
  map(rcurry(\/, v), u)
end method;

define sealed inline method \/(u :: <double-float>, v :: <simple-double-float-vector>)
 => (product :: <simple-double-float-vector>)
  map(curry(\/, u), v)
end method;

define sealed inline method magnitude(v :: <simple-double-float-vector>)
 => (length :: <double-float>)
  sqrt(v * v)
end method magnitude;

define sealed inline method normalize(v :: <simple-double-float-vector>)
 => (normalized-vector :: <simple-double-float-vector>)
 v / magnitude(v)
end method normalize;

define sealed inline method distance (u :: <simple-double-float-vector>, v :: <simple-double-float-vector>)
 => (distance :: <double-float>)
  magnitude(u - v)
end;


