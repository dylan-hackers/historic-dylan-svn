module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <ip-address> (<mutable-wrapper-sequence>)
end;

define method make (ip-address == <ip-address>,
                    #next next-method,
                    #rest rest,
                    #key data,
                    #all-keys) => (res :: <ip-address>)
  if (instance?(data, <string>))
    as(<ip-address>, data);
  else
    apply(next-method, ip-address, rest);
  end if;
end;

//print-object
define method print-object (ip :: <ip-address>,
                            stream :: <stream>)
 => ()
  format(stream, "%s", as(<string>, ip));
end;

//arithmetic operations: +(ip, int) +(int, ip) -(ip, int)
define method \+ (a :: <ip-address>, b :: <integer>)
 => (res :: <ip-address>)
  let rem :: <integer> = b;
  let res = make(<byte-vector>, size: 4, fill: 0);
  for (ele in reverse(a),
       i from 3 by -1)
    let (quotient, remainder) = truncate/(ele + rem, 256);
    res[i] := remainder;
    rem := quotient;
    //format-out("rem %= res[i] %=\n", rem, res[i]);
  end;
  res := make(<ip-address>, data: res);
  //format-out("%= + %= = %=\n", a, b, res);
  res;
end;

define method \+ (a :: <integer>, b :: <ip-address>)
 => (res :: <ip-address>)
  b + a;
end;

define method \- (a :: <ip-address>, b :: <integer>)
 => (res :: <ip-address>)
  let rem :: <integer> = b;
  let res = make(<byte-vector>, size: 4, fill: 0);
  for (ele in reverse(a),
       i from 3 by -1)
    if (ele - rem < 0)
      //format-out("ele - rem < 0 (%= - %= < %=)\n", ele, rem, ele - rem);
      res[i] := modulo(ele - rem, 256);
      rem := abs(truncate/(rem, 256));
    else
      res[i] := ele - rem;
      rem := 0;
    end;
    //format-out("rem %= res[i] %=\n", rem, res[i]); 
  end;
  res := make(<ip-address>, data: res);
  //format-out("%= - %= = %=\n", a, b, res);
  res;
end;

define method \< (a :: <ip-address>, b :: <ip-address>)
 => (res :: <boolean>)
  block(done)
    for (ele1 in a,
         ele2 in b)
      if (ele1 < ele2)
        done(#t);
      elseif (ele1 > ele2)
        done(#f);
      end;
    end for;
    #f;
  end block;
end;

define method \= (a :: <ip-address>, b :: <ip-address>)
  => (res :: <boolean>)
  block(done)
    for (ele1 in a,
         ele2 in b)
      unless (ele1 = ele2)
        done(#f);
      end;
    end;
    done(#t);
  end;
end;


// conversions (string, ip, integer)
define method as (class == <string>, ip-address :: <ip-address>)
 => (res :: <string>)
  let strings = make(<list>);
  for (ele in ip-address)
    strings := add(strings, integer-to-string(ele));
  end;
  reduce1(method(x, y)
              concatenate(x, ".", y)
          end, reverse(strings));
end;

define method as (class == <ip-address>, netmask :: <integer>)
 => (res :: <ip-address>)
  let res = make(<byte-vector>, size: 4, fill: 255);
  for (i from 0 below 4,
       mask from netmask by -8)
    if (mask < 0)
      res[i] := 0;
    elseif (mask < 8)
      res[i] := logand(255, ash(255, 8 - mask));
    end if
  end for;
  make(<ip-address>, data: res);
end;

define method as (class == <ip-address>, string :: <string>)
 => (res :: <ip-address>)
  let numbers = split(string, '.');
  let ints = map(string-to-integer, numbers);
  let res = make(<byte-vector>, size: 4, fill: 0);
  for (i from 0 below res.size)
    res[i] := as(<byte>, ints[i]);
  end;
  make(<ip-address>, data: res);
end;


define method string-to-netmask (string :: <string>)
 => (netmask :: <integer>)
  //"255.255.255.0"
  let vec = reverse(as(<ip-address>, string));
  //0, 255, 255, 255
  let mask = 32;
  block (not-zero)
    for (ele in vec)
      if (ele = 0)
        mask := mask - 8;
      else
        for (i from 7 to 0 by -1)
          unless (logbit?(i, ele))
            mask := mask - i - 1;
            not-zero();
          end unless;
        end for;
      end;
    end for;
  end block;
  mask;
end;

define method parse-ip (ip) => (ip-address :: false-or(<ip-address>))
  format-out("PARSE %= %=\n", ip, object-class(ip));
  #f;
end;
