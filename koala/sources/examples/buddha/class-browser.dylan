module: buddha

/*
define method browse (s, o, os)
  map(method(x)
          with-xml()
            li{ a("fpp", href => "/") }
          end;
      end, range(to: 10));
//  make(<element>, name: "foo");
end;*/

define method get-reference (object :: <object>) => (res :: <string>)
  let address = copy-sequence(format-to-string("%=", address-of(object)),
                              start: 1);
  $obj-table[address] := object;
  address;
end;

define method browse (object :: <object>) => (res :: <list>)
  let class = object.object-class;
  let res = make(<list>);
  for (slot in class.slot-descriptors)
    if (slot-initialized?(object, slot))
      let name = slot.slot-getter.debug-name;
      let type = slot.slot-type;
      if (type = <string>)
        res := add!(res,
                    with-xml()
                      li(concatenate(name,
                                     ": ",
                                     slot.slot-getter(object)))
                    end);
      elseif (subtype?(type, <sequence>))
        for (ele in slot.slot-getter(object),
             i from 0)
          res := add!(res,
                      with-xml ()
                        li{ a(concatenate(name, " ",
                                          integer-to-string(i)),
                              href => concatenate("/browse?obj=",
                                                  get-reference(ele))) }
                      end);
        end;
      elseif (type = <table>)
        for (ele in slot.slot-getter(object),
             i in slot.slot-getter(object).key-sequence)
          res := add!(res,
                      with-xml()
                        li{ a(format-to-string("%s %=", name, i),
                              href => concatenate("/browse?obj=",
                                                  get-reference(ele))) }
                      end);
        end;
      elseif (type = <integer>)
        res := add!(res,
                    with-xml()
                      li(concatenate
                           (name,
                            ": ",
                            integer-to-string(slot.slot-getter(object))))
                    end);
      elseif (type = <boolean>)  
        res := add!(res,
                    with-xml()
                      li(concatenate(name,
                                     ": ",
                                     if (slot.slot-getter(object))
                                       "true"
                                     else
                                       "false"
                                     end))
                    end);
      else 
        let string = format-to-string("%= %=", name, slot.slot-getter(object));
        res := add!(res, with-xml()
                           li {
                               a(string,
                                 href => concatenate
                                   ("/browse?obj=",
                                    get-reference(slot.slot-getter(object)))) }
                         end);
      end;
    end;
  end;
  res;
end;

define method find-slot (slot-name :: <string>, object :: <object>)
  => (slot)
  let class = object.object-class;
  let number = #f;
  if (any?(method(x) x = '[' end, slot-name))
    let (all, real-slot-name, element-number)
      = regexp-match(slot-name, "(.*)\\[(.*)\\]");
    slot-name := real-slot-name;
    number := string-to-integer(element-number);
  end;
  block (found)
    for (slot in class.slot-descriptors)
      if (slot.slot-getter.debug-name = slot-name)
        if (number)
          found(slot.slot-getter(object)[number]);
        else
          found(slot.slot-getter(object));
        end;
      end;
    end;
  end;
end;