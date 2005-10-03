module: harmonic

begin
  let partial-sum :: <double-float> = 0.0d0;
  for (n from application-arguments()[0].string-to-integer above 0 by -1,
       i from 1.0 by 1.0)
    partial-sum := partial-sum + 1.0d0 / i;
  end for;
  format-out("%.9f\n",partial-sum);
end;
