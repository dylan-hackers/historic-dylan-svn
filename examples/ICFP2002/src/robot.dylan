module: board

define abstract class <robot>(<object>)
  slot capacity :: <integer>;

  slot x :: <integer>;
  slot y :: <integer>;
  slot inventory :: limited(<vector>, of: <package>);
end;