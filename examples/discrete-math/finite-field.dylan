module: numbers
author: Hannes Mehnert <hannes@mehnert.org>

define class <finite-field> (<number>)
  slot %value :: limited(<integer>, min: 0),
    init-keyword: value:, init-value: 0;
  slot size :: limited(<integer>, min: 2), required-init-keyword: size:;
end class <finite-field>;

//value should always be between 0 and size - 1!
define method make (class == <finite-field>,
                    #next next-method,
                    #key size :: limited(<integer>, min: 2),
                    value :: false-or(<integer>))
 => (res :: <finite-field>)
  if (value)
    if ((value >= size) | (negative?(value)))
      next-method(class, size: size, value: modulo(value, size));
    else
      next-method(class, size: size, value: value);
    end if;
  else
    next-method(class, size: size);
  end if;
end method make;

define method value-setter (value :: <integer>,
                            ff :: <finite-field>)
  if ((value >= ff.size) | (negative?(value)))
    ff.%value := modulo(value, ff.size);
  else
    ff.%value := value;
  end if;
end method value-setter;

define method value (ff :: <finite-field>) => (value :: <integer>)
  ff.%value;
end method value;

define inline method ff (value :: <integer>, size :: <integer>)
 => (ff :: <finite-field>)
  make(<finite-field>, value: value, size: size);
end method ff;


//<,>,= compares size of ff
define method \< (x :: <finite-field>, y :: <finite-field>)
 => (res :: <boolean>)
  x.size < y.size;
end method \<;

//define method \> (x :: <finite-field>, y :: <finite-field>)
// => (res :: <boolean>)
//  x.size > y.size;
//end method \>;

define method \= (x :: <finite-field>, y :: <finite-field>)
 => (equal? :: <boolean>)
  x.size = y.size;
end method \=;


define method zero?(x :: <finite-field>)
 => (zero? :: <boolean>)
  zero?(x.value);
end method zero?;

define method \+ (x :: <finite-field>, y :: <finite-field>)
 => (sum :: <finite-field>)
  if (x = y)
    ff(modulo(x.value + y.value, x.size), x.size);
  else
    error("not same finite-field!");
  end if;
end method \+;

define method \+ (x :: <integer>, y :: <finite-field>)
 => (sum :: <finite-field>)
  ff(modulo(x + y.value, y.size), y.size);
end method \+;

define method \+ (x :: <finite-field>, y :: <integer>)
 => (sum :: <finite-field>)
  ff(modulo(y + x.value, x.size), x.size);
end method \+;

define method \- (x :: <finite-field>, y :: <finite-field>)
 => (difference :: <finite-field>)
  if (x = y)
    ff(modulo(x.value - y.value, x.size), x.size);
  else
    error("not same finite-field!");
  end if;
end method \-;

define method \- (x :: <integer>, y ::<finite-field>)
 => (difference :: <finite-field>)
  ff(modulo(x - y.value, y.size), y.size);
end method \-;

define method \- (x :: <finite-field>, y :: <integer>)
 => (difference :: <finite-field>)
  ff(modulo(y - x.value, x.size), x.size);
end method \-;

define method \* (x :: <finite-field>, y :: <finite-field>)
 => (product :: <finite-field>)
  if (x = y)
    ff(modulo(x.value * y.value, x.size), x.size);
  else
    error("not same finite-field!");
  end if;
end method \*;

define method \* (x :: <integer>, y :: <finite-field>)
 => (product :: <finite-field>)
  ff(modulo(x * y.value, y.size), y.size);
end method \*;

define method \* (x :: <finite-field>, y :: <integer>)
 => (product :: <finite-field>)
  ff(modulo(x.value * y, x.size), x.size);
end method \*;

/*
define method \^ (x :: <finite-field>, y :: <integer>)
 => (result :: <finite-field>)
  make(<finite-field>,
       value: modulo(x.value ^ y, x.size),
       size: x.size );
end method \^;
*/

/*
//buchmann: einfuehrung in die kryptographie: p.39
define method \^ (x :: <finite-field>, y :: <integer>)
 => (result :: <finite-field>)
  let result :: <finite-field> = make(<finite-field>, value: 1, size: x.size);
//  let x :: <finite-field> = x;
//  let y :: <integer> = y;
  while (y > 0)
    if (odd?(y))
      result := result * x;
    end if;
    x := x * x;
    y := ash(y, -1);
  end while;
  result;
end method \^;
*/

//handbook of applied cryptography: 2.143, p.71
define method \^ (x :: <finite-field>, k :: <integer>)
 => (result :: <finite-field>)
  if (negative?(k))
    error("k >= 0!");
  end if;
  //1
  let b :: <finite-field> = ff(1, x.size);
  if (k = 0)
    b;
  else
    let k :: <list> = base-n-representation(k, 2);
    k := reverse!(k);
    //2
    let A :: <finite-field> = ff(x.value, x.size);
    //3
    if (k[0] = 1)
      b := x;
    end if;
    //4
    for (i from 1 below k.size)
      //4.1
      A := A * A;
      //4.2
      if (k[i] = 1)
        b := A * b;
      end if;
    end for;
    //5
    b;
  end if;
end method \^;


// handbook of applied cryptography: 2.142, p.71
define method inverse (finite-field :: <finite-field>)
 => (inverse :: false-or(<finite-field>))
  let (gcd, x, y) = extended-euclid(finite-field.size, finite-field.value);
  if (gcd = 1)
    let size = finite-field.size;
    ff(modulo(y, size), size);
  else
    #f;
  end if;
end method inverse;

/*
//chinese remainder theorem
//buchmann: p.44
define method chinese-remainder
 (ffs :: limited(<simple-vector>, of: <finite-field>)
 => (result :: <finite-field>)
  let result = make(<finite-field>,
                    size: product(ffs.size)
end method chinese-remainder;
*/

