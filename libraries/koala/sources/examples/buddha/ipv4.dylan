module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <ip-address> (<object>)
  slot ip :: <byte-vector>, init-keyword: ip:;
end;

define method make (ip-address == <ip-address>,
                    #next next-method,
                    #rest rest,
                    #key ip,
                    #all-keys) => (res :: <ip-address>)
  let args = rest;
  if (instance?(ip, <string>))
    args := exclude(args, #"ip");
    ip := string-to-ip-address(ip);
  end;
  apply(next-method, ip-address, ip: ip, args);
end;

define method \+ (a :: <ip-address>, b :: <integer>)
 => (res :: <ip-address>)
  let rem :: <integer> = b;
  let res :: <ip-address>
    = make(<ip-address>, ip: make(<byte-vector>, size: 4, fill: 0));
  for (ele in reverse(a.ip),
       i from 3 by -1)
    let (quotient, remainder) = truncate/(ele + rem, 256);
    res.ip[i] := remainder;
    rem := quotient;
    //format-out("rem %= res.ip[i] %=\n", rem, res.ip[i]);
  end;
  //format-out("%= + %= = %=\n", a, b, res);
  res;
end;

define method \+ (a :: <integer>, b :: <ip-address>)
 => (res :: <ip-address>)
  b + a;
end;

define method print-object (ip :: <ip-address>,
                            stream :: <stream>)
 => ()
  format(stream, "%s", ip-address-to-string(ip));
end;
  

define method string-to-netmask (string :: <string>)
 => (netmask :: <integer>)
  //"255.255.255.0"
  let vec = reverse(string-to-ip-address(string));
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

define method netmask-to-string (netmask :: <integer>)
 => (string :: <string>)
  integer-to-string(netmask);
end;

define method netmask-to-vector (netmask :: <integer>)
 => (vec :: <byte-vector>)
  let res = make(<byte-vector>, size: 4, fill: 255);
  for (i from 0 below 4,
       mask from netmask by -8)
    if (mask < 0)
      res[i] := 0;
    elseif (mask < 8)
      res[i] := logand(255, ash(255, 8 - mask));
    end if
  end for;
  res;
end;

define method string-to-ip-address (string :: <string>)
 => (ip :: <byte-vector>)
  let numbers = split(string, '.');
  let ints = map(string-to-integer, numbers);
  let res = make(<byte-vector>, size: 4, fill: 0);
  for (i from 0 below res.size)
    res[i] := as(<byte>, ints[i]);
  end;
  res;
end method;

define method ip-address-to-string (ip-address :: <byte-vector>)
 => (string :: <string>)
  let strings = make(<list>);
  for (ele in ip-address)
    strings := add(strings, integer-to-string(ele));
    //let strings = map(integer-to-string, ip-address);
    //doesn't work. not sure why... '"10" is not of type limited(<integer>)'
  end;
//  let res = 
  reduce1(method(x, y)
              concatenate(x, ".", y)
          end, reverse(strings));
  //format-out("res %=\n", res);
  //res;
end;

define method ip-address-to-string (ip-address :: <ip-address>)
 => (string :: <string>)
  ip-address-to-string(ip-address.ip);
end;

define method parse-ip (ip) => (ip-address :: false-or(<ip-address>))
  format-out("PARSE %= %=\n", ip, object-class(ip));
  #f;
end;

define method \< (a :: <ip-address>, b :: <ip-address>)
 => (res :: <boolean>)
  block(done)
    for (ele1 in a.ip,
         ele2 in b.ip)
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
  every?(method(x) x = #t end,
         map(\=,
             a.ip,
             b.ip));
end;
