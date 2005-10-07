module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define method edit-form (object :: <object>) => (res)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object))
             format-out("SLOTSS %=\n", slot);
             let object = slot.slot-getter-method(object);
             format-out("SLOT %= %= %=\n",
                        slot.slot-name,
                        slot.slot-type,
                        object);
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //XXX check if slot is initialized?
             collect(edit-slot(object, slot.slot-name));
             collect(with-xml() br end);
           end;
           for (slot in reference-slots(object))
             
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //get slot, generate select, option field for each element
             //of global list of elements...
             collect(with-xml()
                       //probably better address of object?
                       \select(name => slot.slot-name)
                       { do(for (ele in slot.slot-getter-method(*config*))
                              collect(with-xml()
                                        option(as(<string>, ele),
                                               value => as(<string>, ele))
                                      end)
                            end)
                       }
                      end);
           end),
        input(type => "hidden",
              name => "obj-id",
              value => get-reference(object)),
        input(type => "hidden",
              name => "action",
              value => "save-object"),
        input(type => "submit",
              name => "save-button",
              value => "Save")
      }
    }
  end;
end;


define method add-form (object :: <object>,
                        name :: <string>,
                        list :: <object>) => (foo) // :: <list> ?
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object))
             format-out("DATA SLOT %= %=\n", slot.slot-name, slot.slot-type);
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //here we should have at least a seperation between integer,
             //strings and lists... or should we implement all lists with
             //has-many?
             if (slot.slot-type = <boolean>)
               collect(with-xml() input(type => "checkbox",
                                        name => slot.slot-name,
                                        value => slot.slot-name)
                       end);
             else
               collect(with-xml() input(type => "text",
                                        name => slot.slot-name)
                       end);
             end;
             collect(with-xml() br end);
           end;
           for (slot in reference-slots(object))
             format-out("REF SLOT %= %= %=\n",
                        slot.slot-name,
                        slot.slot-type,
                        slot.slot-getter-method);
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //get slot, generate select, option field for each element
             //of global list of elements...
             collect(with-xml()
                       //probably better address of object?
                       \select(name => slot.slot-name)
                       { do(for (ele in slot.slot-getter-method(*config*))
                              //this will not work for hosts...
                              //because *config*.subnets is not defined
                              //anyway, when we come to the reference to the
                              //displayed object, it should be the displeyed
                              //object???.... not sure yet
                              collect(with-xml()
                                        option(as(<string>, ele),
                                               value => as(<string>, ele))
                                      end)
                            end)
                       }
                      end);
           end),
        input(type => "hidden",
              name => "obj-id",
              value => get-reference(list)),
        input(type => "hidden",
              name => "action",
              value => "add-object"),
        input(type => "submit",
              name => "add-button",
              value => concatenate("Add to ", name))
      }
    }
  end;
end;


define method list-forms (obj :: <object>) => (res)
  let res = make(<stretchy-vector>);
  for (slot in list-reference-slots(obj))
    let object = slot.slot-getter-method(obj);
    res := add!(res, with-xml()
                       text(concatenate(slot.slot-name, ": "))
                     end);
    res := add!(res, with-xml() br end);
    for (ele in object)
      res := add!(res, with-xml()
                         a(as(<string>, ele),
                           href => concatenate("/edit?obj=",
                                               get-reference(ele)))
                       end);
      res := add!(res, with-xml()
                         form(action => "/edit", \method => "post")
                         { div(class => "edit")
                           { input(type => "hidden",
                                   name => "obj-id",
                                   value => get-reference(ele)),
                             input(type => "hidden",
                                   name => "action",
                                   value => "remove-object"),
                             input(type => "submit",
                                   name => "remove-button",
                                   value => "Remove")
                           }
                         }
                       end);
      res := add!(res, with-xml() br end);
    end;
    format-out("Adding form for type %= slot-name %= to %=\n",
               slot.slot-type,
               slot.slot-name,
               object);
    res := add!(res, add-form(make(slot.slot-type),
                              slot.slot-name,
                              object));
  end;
  res;
end;


define generic edit-slot (object :: <object>, slot-name :: <string>);

define method edit-slot (object :: <object>, slot-name :: <string>)
  with-xml()
    input(type => "text",
          name => slot-name,
          value => as(<string>, object))
  end;
end;

define method edit-slot (object :: <string>, slot-name :: <string>)
  with-xml()
    input(type => "text",
          name => slot-name,
          value => object)
  end;
end;

define method edit-slot (object :: type-union(<list>, <table>),
                         slot-name :: <string>)
  with-xml()
    ul
    { do(if (object.size > 0)
           for (ele in object)
             collect(with-xml()
                       li { do(let reference = get-reference(ele);
                               collect(with-xml()
                                         a(as(<string>, ele),
                                           href => concatenate("/edit?obj=",
                                                               reference))
                                       end);
                               collect(with-xml()
                                         input(type => "submit",
                                               name => concatenate("remove-", reference),
                                               value => "Remove")
                                       end))
                             }
                     end);
           end;
         else
           collect(with-xml() li("empty list") end);
         end if)
    }
  end;
end;

define method edit-slot (object :: <integer>, slot-name :: <string>)
  with-xml()
    input(type => "text",
          name => slot-name,
          value => integer-to-string(object))
  end;
end;

define method edit-slot (object :: <boolean>, slot-name :: <string>)
  if (object)
    with-xml()
      input(type => "checkbox",
            name => slot-name,
            value => slot-name,
            checked => "checked")
    end;
  else
    with-xml()
      input(type => "checkbox",
            name => slot-name,
            value => slot-name)
    end;
  end;
end;

