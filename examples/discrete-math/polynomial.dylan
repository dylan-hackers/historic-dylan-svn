module: numbers
author: Hannes Mehnert <hannes@mehnert.org>

define class <polynomial> (<object>)
  slot finite-field :: <finite-field>, required-init-keyword: finite-field:;
  slot %coefficients :: <list>, required-init-keyword: coefficients:;
end class <polynomial>;

define method make (class == <polynomial>,
                    #next next-method,
                    #key finite-field :: <finite-field>,
                    coefficients :: <list>)
 => (res :: <polynomial>)
  next-method(class,
              finite-field: finite-field,
              coefficients: reduce-list(coefficients, finite-field.size));
end method make;

define method coefficients-setter(l :: <list>, poly :: <polynomial>)
  poly.%coefficients := reduce-list(l, poly.finite-field.size);
end method coefficients-setter;

define method coefficients (p :: <polynomial>)
 => (coefficients :: <list>)
  p.%coefficients;
end method coefficients;

define method reduce-list (l :: <list>, x :: <integer>)
 => (res :: <list>)
  local method m (y :: <integer>)
          modulo(y, x);
        end method m;
  let res :: <list> = map(m, l);
  while ((res.size > 0) & (res[0] = 0))
    res := tail(res);
  end while;
  res;
end method reduce-list;

define method poly (ff :: <finite-field>, coeff :: <list>)
 => (polynomial :: <polynomial>)
  make(<polynomial>,
       finite-field: ff,
       coefficients: coeff);
end method poly;

define method degree (p :: <polynomial>)
 => (order :: <integer>)
  p.coefficients.size;
end method degree;


define method zero?(p :: <polynomial>)
 => (zero :: <boolean>)
  if (degree(p) = 0)
    #t;
  else
    #f;
  end if;
end method zero?;

define method \= (f :: <polynomial>, g :: <polynomial>)
 => (same :: <boolean>)
  block (return)
    unless (f.finite-field = g.finite-field)
      return(#f);
    end unless;
    unless (degree(f) = degree(g))
      return(#f);
    end unless;
    every?(\=, f.coefficients, g.coefficients);
  end block;
end method \=;

define method \+ (f :: <polynomial>, g :: <polynomial>)
 => (sum :: <polynomial>)
  block (return)
    unless (f.finite-field = g.finite-field)
      error("not same finite fields!");
    end unless;
    let degf = degree(f);
    let degg = degree(g);
    if (degg > degf)
      return(g + f);
    elseif (degg = 0)
      return(f);
    else
      let coeff :: <list> = #();
      let f-coeff :: <list> = f.coefficients;
      for (i from degg below degf)
        coeff := pair(f-coeff.head, coeff);
        f-coeff := f-coeff.tail;
      end for;
      coeff := concatenate(coeff, map(\+, f-coeff, g.coefficients));
      poly(f.finite-field, coeff);
    end if;
  end block;
end method \+;


define method \* (f :: <polynomial>, g :: <polynomial>)
 => (product :: <polynomial>)
  block (return)
    unless (f.finite-field = g.finite-field)
      error("not same finite field!");
    end unless;
    if (degree(g) > degree(f))
      return(g * f);
    else
      //adds 2 lists (modulo f.finite-field.size)
      //prepends shorter list with zeros
      //and appends a zero
      //list-addition(#(1,2,3), #(2,3)) -> #(1,4,6,0)
      local method list-addition (x :: <list>, y :: <list>)
              if (x.size < y.size)
                list-addition(y, x);
              else
                let res :: <list> = make(<list>, size: x.size);
                x := reverse(x);
                y := reverse(y);
                for (j from 0 below x.size)
                  res[j] := if (j < y.size)
                              modulo(x[j] + y[j], f.finite-field.size);
                            else
                              x[j];
                            end if;
                end for;
                res := pair(0, res);
                reverse(res);
              end if;
            end method;
      let res :: <list> = make(<list>);
      for (i from 0 below degree(g))
        local method multiply-with-gi (x :: <integer>)
                modulo(x * g.coefficients[i], f.finite-field.size);
              end method;
        res := list-addition(res, map(multiply-with-gi, f.coefficients));
      end for;
      // remove last element from res
      res := reverse(tail(reverse(res)));
      return(poly(f.finite-field, res));
    end if;
  end block;
end method \*;

define method \/ (f :: <polynomial>, g :: <polynomial>)
 => (quotient :: <polynomial>, remainder :: <polynomial>)
  
end method \/;

//handbook of applied cryptography: 2.227, p.84
define method \^ (f :: <polynomial>, int :: <integer>)
 => (pow :: <polynomial>)
  if (int < 0)
    error("pow only defined for positive integers!");
  end if;

  //1
  let s = poly(f.finite-field, #(1));
  block (return)
    if (int = 0)
      return(s);
    end if;

    //2
    let G = f;
    
    //3
    let k = base-n-representation(int, 2);
    if (k[0] = 1)
      s := f;
    end if;
    
    //4
    for (i from 1 below k.size)
      G := G * G;
      if (k[i] = 1)
        s := G * s;
      end if;
    end for;

    //5
    return(s);
  end block;
end method \^;

//handbook of applied cryptography: 2.226, p.84
define method inverse (f :: <polynomial>)
 => (inverse :: <polynomial>)

end method inverse;

define method evaluate (f :: <polynomial>, int :: <integer>)
 => (res :: <finite-field>)
  let res :: <finite-field> = make(<finite-field>, size: f.finite-field.size);
  //optimize int ^ (degree(f) - 1 - i) (int is a finite field)
  let int-ff :: <finite-field> = make(<finite-field>,
                                      size: f.finite-field.size,
                                      value: int);
  for (i from 0 below degree(f))
    res := res + int-ff ^ (degree(f) - 1 - i) * f.coefficients[i];
  end for;
  res;
end method evaluate;

//handbook of applied cryptography: 2.218, p.82
define method gcd (g :: <polynomial>, h :: <polynomial>)
 => (res :: <polynomial>)
  let res :: <polynomial> = make(<polynomial>);
  //1
  while (~ zero?(h))
    //1.1
//    res := g mod h;
    g := h;
    h := res;
  end while;
  //2
  res;
end method gcd;

//handbook of applied cryptography: 2.221, p.82
define method extended-euclid (g :: <polynomial>, h :: <polynomial>)
 => (gcd :: <polynomial>, s :: <polynomial>, t :: <polynomial>)

end method extended-euclid;

