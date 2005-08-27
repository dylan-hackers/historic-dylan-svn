module: buddha

define constant <mac-address> = <list>;

define method parse-mac (mac :: <string>)
 => (res :: false-or(<mac-address>))
  block(parse-error)
    mac := as-lowercase(mac);
    if (any?(method(x) x = ':' end, mac))
      //try to parse xx:xx:xx:xx:xx:xx
      let fields = split(mac, ':');
      //check that we really have a valid mac address
      unless (size(fields) = 6)
        //6 fields
        parse-error(#f);
      end unless;
      unless (every?(method(x) x.size = 2 end, fields))
        //each containing 2 characters
        parse-error();
      end unless;
      for (field in fields)
        for (ele in field)
          unless (hex-digit?(ele))
            parse-error(#f);
          end unless;
        end for;
      end for;
      let res = make(<mac-address>, size: 6);
      for (i from 0 below res.size)
        res[i] := fields[i];
      end;
      res;
    elseif (size(mac) = 12)
      //assume xxxxxxxxxxxx
      for(ele in mac)
        unless (hex-digit?(ele))
          parse-error(#f);
        end unless;
      end for;
      string-to-mac(mac);
    else
      //something completely different
      parse-error(#f);
    end if;
  end block;
end;

define method string-to-mac (string :: <string>) => (mac :: <mac-address>)
  //we are sure string is a correct mac-address (12 hex digits)
  let mac = make(<mac-address>, size: 6);
  for (i from 0 below string.size by 2,
       j from 0)
    mac[j] := copy-sequence(string, start: i, end: i + 2);
  end;
  mac;
end;

define method mac-to-string (mac :: <mac-address>) => (string :: <string>)
  reduce1(method(a,b) concatenate(a, ":", b) end, mac);
end;
