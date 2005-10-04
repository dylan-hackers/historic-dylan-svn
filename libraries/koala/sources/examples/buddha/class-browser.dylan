module: buddha
author: Hannes Mehnert <hannes@mehnert.org>


define method browse (object :: <object>) => (res :: <list>)
  let class = object.object-class;
  let res = make(<list>);
  for (slot in class.slot-descriptors)
    let name = slot.slot-getter.debug-name;
    if (slot-initialized?(object, slot))
      let slot-object = slot.slot-getter(object);
      res := concatenate(res, show(slot-object, name));
    else
      res := add!(res, with-xml()
                         li(concatenate(name, " not initialized"))
                       end);
    end;
  end;
  res;
end;

define method show (object :: <ip-address>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ":", as(<string>, object)))
       end)
end;

define method show (object :: <mac-address>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ":", as(<string>, object)))
       end)
end;

define method show (object :: <cidr>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ":", as(<string>, object)))
       end)
end;

define method show (object :: <string>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ": ", object))
       end)
end;

define method show (object :: <sequence>, slot-name :: <string>)
  let res = make(<list>);
  for (ele in object,
       i from 0)
    res := add!(res,
                with-xml ()
                  li{ a(concatenate(slot-name, " ",
                                    integer-to-string(i)),
                        href => concatenate("/browse?obj=",
                                            get-reference(ele))) }
                end);
  end;
  res;
end;

define method show (object :: <table>, slot-name :: <string>)
  let res = make(<list>);
  for (ele in object,
       i in object.key-sequence)
    res := add!(res,
                with-xml()
                  li{ a(format-to-string("%s %=", slot-name, i),
                        href => concatenate("/browse?obj=",
                                            get-reference(ele))) }
                end);
  end;
  res;
end;

define method show (object :: <integer>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ": ",
                        integer-to-string(object)))
       end)
end;

define method show (object :: <boolean>, slot-name :: <string>)
  list(with-xml()
         li(concatenate(slot-name, ": ",
                        if (object)
                          "true"
                        else
                          "false"
                        end))
       end)
end;

define method show (object :: <object>, slot-name :: <string>)
  let string = format-to-string("%= %=", slot-name, object);
  list(with-xml()
         li { a(string,
                href => concatenate("/browse?obj=",
                                    get-reference(object)))
               }
       end)
end;
