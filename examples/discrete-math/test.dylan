module: foo

begin
  let arg = string-to-integer(element(application-arguments(), 0));
  let arg2 = string-to-integer(element(application-arguments(), 1));
  format-out("arguments: %d %d\n", arg, arg2);
//  if (arg > 0 & arg2 > 1)
    let w = base_n_representation(0, 2);
    format-out("base_n_rep(0,2): %=\n", w);
    format-out("k[0]: %d\n", w[0]);
    let v = base_n_representation(254, 2);
    format-out("base_n_rep(254,2): %=\n", v);
//  end if;
  format-out("mygcd: %=\n", mygcd(arg, arg2));
  format-out("gcd: %=\n", gcd(arg, arg2));
  let (a, b, c) = extended-euclid(arg, arg2);
  format-out("ext-euclid: %d %d %d %d\n", a, b, c, b * arg + c * arg2);
  let x :: <finite-field> = make(<finite-field>, value: arg, size: arg2);
  format-out("<finite-field>: %d %d\n", x.value, x.size);
  let y = x + 23;
  format-out("<finite-field> + int(23): %d\n", y.value);
  let z = x * x;
  format-out("<finite-field> * <finite-field>: %d\n", z.value);
  let a = x ^ 2;
  format-out("<finite-field> ^ 2: %d\n", a.value);
  let ab = x ^ 0;
  format-out("<finite-field> ^ 0: %d\n", ab.value);
  let ba = make(<finite-field>, value: 5, size: 1234);
  format-out("<finite-field> %d %d\n", ba.value, ba.size);
  let aa = ba ^ 596;
  if (aa.value ~= 1013)
    error("pow broken! %d ~= %d", aa.value, 1013);
  else
    format-out("pow: %d\n", aa.value);
  end if;
  format-out("<finite-field> %d %d\n", ba.value, ba.size);
  let b :: <finite-field> = make(<finite-field>, size: 9);
  b.value := 4;
  let c = inverse(b);
  format-out("foo: %d mod %d inverse: %=\n", b.value, b.size, c.value);
  let bb :: <finite-field> = make(<finite-field>, size: 23, value: -2);
  format-out("bb: %d mod %d\n", bb.value, bb.size);
  bb.value := 42;
  format-out("bb: %d mod %d\n", bb.value, bb.size);
  bb.value := -1;
  format-out("bb: %d mod %d\n", bb.value, bb.size);
  format-out("jacobi(158/235): %d\n", jacobi(158, 235));
  let d = make(<polynomial>,
               finite-field: make(<finite-field>, size: 23),
               coefficients: #(1, 1, 2, 8));
  let e = make(<polynomial>,
               finite-field: make(<finite-field>, size: 23),
               coefficients: #(2, 1, 1, 2, 2));
  format-out("poly d: %= degree: %d\n", d.coefficients, degree(d));
  format-out("poly e: %= degree: %d\n", e.coefficients, degree(e));
  let f = d + e;
  format-out("poly d + e: %= degree %d\n", f.coefficients, degree(f));
  format-out("same?(d=e) %= (d=f) %= (e=f) %= (f=f) %=\n", d = e, d = f, e = f, f = f);
  d.coefficients := #();
  format-out("zero?(d) %= d: %=\n", zero?(d), d.coefficients);
  d.coefficients := #(0,0,0,0,0);
  format-out("poly d: %= degree: %d zero? %=\n", d.coefficients, degree(d), zero?(d));
  let f = d + e;
  format-out("poly d + e: %= degree %d\n", f.coefficients, degree(f));
  let fa = make(<polynomial>,
               finite-field: make(<finite-field>, size: 23),
               coefficients: #(0, 0, 0, 0, 0));
  format-out("poly fa: %= degree: %d zero? %=\n", fa.coefficients, degree(fa), zero?(fa));
  fa.coefficients := #(0,0,0,1,0,0,0);
  format-out("poly fa: %= degree: %d zero? %=\n", fa.coefficients, degree(fa), zero?(fa));
  let fba = make(<polynomial>,
                 finite-field: make(<finite-field>, size: 2),
                 coefficients: #(1, 0, 1, 1));
  let fbb = make(<polynomial>,
                 finite-field: make(<finite-field>, size: 2),
                 coefficients: #(1, 1, 0));
  let fbc = fba * fbb;
  let fbd = fba + fbb;
  format-out("fba: %= fbb: %= fba * fbb: %= fba + fbb: %=\n",
             fba.coefficients, fbb.coefficients,
             fbc.coefficients, fbd.coefficients);
  let fc = make(<polynomial>,
                finite-field: make(<finite-field>, size: 23),
                coefficients: #(1, 2, 1, 0));
  let fd = evaluate(fc, 2);
  let fe = evaluate(fc, 3);
  let ff = evaluate(fc, 4);
  format-out("fc: %= fd: %d mod %d fe: %d mod %d ff: %d mod %d\n",
             fc.coefficients, 
             fd.value, fd.size,
             fe.value, fe.size,
             ff.value, ff.size);
  format-out("%d %=\n", 42, trial-division(42, 7));
  format-out("%d %d\n", 455459, pollard-rho(455459));
//  let ga :: <double-integer> = #xffffffff;
//  let gb :: <double-integer> = #x7fffffff;
  format-out("2^32 | 2 ^ 31: %d\n", logior(#xffffffff, #x0000F000));
  format-out("%d %d %d\n", 
             as(<integer>, 'a'), 
             as(<integer>, 'b'), 
             as(<integer>, 'c'));
  let foo = as(<byte-vector>, 2);
  format-out("%=\n", foo)
end;
