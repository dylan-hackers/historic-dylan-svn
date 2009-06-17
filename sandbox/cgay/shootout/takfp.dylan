module: takfp

define constant <fp> = <single-float>;

begin
  local method tak(x :: <fp>, y :: <fp>, z :: <fp>) => (res :: <fp>)
          case
            x > y  => tak(tak(x - 1.0s0, y, z),
                          tak(y - 1.0s0, z, x),
                          tak(z - 1.0s0, x, y));
            otherwise => z;
          end;
        end method;

  let n = application-arguments()[0].string-to-integer;
  format-out("%.1f\n", tak(3.0s0 * n, 2.0s0 * n, 1.0s0 * n));
end;

