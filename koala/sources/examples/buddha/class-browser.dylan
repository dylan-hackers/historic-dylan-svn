module: buddha
author: Hannes Mehnert <hannes@mehnert.org>


define method browse (object :: <object>) => (res :: <list>)
  let res = make(<list>);
  for (slot in data-slots(object))
    //format-out("DATA SLOT %=\n", slot.slot-name);
    res := add!(res, with-xml()
                       li(concatenate(slot.slot-name, ": ",
                                      show(slot.slot-getter-method(object))))
                     end);
  end;
  for (slot in reference-slots(object))
    //format-out("REF SLOT NAME %= TYPE %= GETTER %=\n",
    //           slot.slot-name, slot.slot-type, slot.slot-getter-method(object));
    res := add!(res,
                with-xml()
                  li { a(concatenate(slot.slot-name, ": ",
                                     show(slot.slot-getter-method(object))),
                         href => concatenate
                           ("/browse?obj=",
                            get-reference(slot.slot-getter-method(object))))
                      }
                end);
  end;
  for (slot in list-reference-slots(object))
    //format-out("LIST SLOT %=\n", slot.slot-name);
    res := add!(res, with-xml() text(slot.slot-name) end);
    for (ele in slot.slot-getter-method(object))
      res := add!(res, with-xml()
                         li{ a(show(ele),
                               href => concatenate("/browse?obj=",
                                                   get-reference(ele)))
                            }
                       end);
    end;
  end;
  reverse(res);
end;

define method show (object :: <string>)
  object
end;

define method show (object :: <integer>)
  integer-to-string(object)
end;

define method show (object :: <boolean>)
  if (object)
    "true"
  else
    "false"
  end
end;

define method show (object :: <object>)
  as(<string>, object)
end;
