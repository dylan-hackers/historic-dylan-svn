module: vrml-viewer

define method string-to-number (string :: <sequence>, #key base: base = 10)
 => num :: <number>;
  let number = 0;
  let negate = 1;
  let seen-decimal = #f;
  let decimal-divisor = 1;
  for (c in string)
    select (c)
      '-' =>      negate := -1;
      '+' =>      negate := 1;
      '.' =>      seen-decimal := #t;
      otherwise =>
        let digit = digit-to-integer(c);
        if (digit >= base) 
          error("\"%s\" isn't in base %d", string, base);
        elseif (seen-decimal)  
          decimal-divisor := decimal-divisor * base;
          number := number + as(<float>, digit) / as(<float>, decimal-divisor);
        else 
          number := number * base  + digit;
        end if;
    end select;
  end for;

  number * negate;
end method string-to-number;

define method digit-to-integer (c :: <character>) => digit :: <integer>;
  if (~alphanumeric?(c))
    error("Invalid digit %=", c);
  end if;
  select (c)
    '0' => 0;
    '1' => 1;
    '2' => 2;
    '3' => 3;
    '4' => 4;
    '5' => 5;
    '6' => 6;
    '7' => 7;
    '8' => 8;
    '9' => 9;
    otherwise =>
      as(<integer>, as-lowercase(c)) - as(<integer>, 'a') + 10;
  end select;
end method digit-to-integer;

define inline function alphanumeric? (c :: <character>) => answer :: <boolean>;
  (c >= 'a' & c <= 'z')  |  (c >= 'A' & c <= 'Z')  |  (c >= '0' & c <= '9');
end function alphanumeric?;
