module: buddha


define method edit (object :: <object>) => (res)
  let class = object.object-class;
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in class.slot-descriptors)
             let name = slot.slot-getter.debug-name;
             collect(with-xml() text(concatenate(name, ": ")) end);
             if (slot-initialized?(object, slot))
               let slot-object = slot.slot-getter(object);
               collect(edit-slot(slot-object, name));
               if (instance?(slot-object, <list>))
                 let type = list-type(object, name);
                 if (type)
                   collect(add-form(type));
                 end;
               end if;
             else
               //slot not initialized...
               collect(with-xml() input(type => "text", name => name) end)
             end if;
             collect(with-xml() br end);
           end),
        input(type => "submit", name => "save-button", value => "Save")
      }
    }
  end;
end;

define generic add-form (object :: <object>);

define macro add-form-helper
  { add-form-helper(?type:name) end }
    => { define method add-form (type == ?#"type")
           with-xml()
             form (action => "/edit", \method => "post")
             {
               div (class => "edit")
               {
                 do(let res = make(<list>);
                    for(slot in ?type.slot-descriptors)
                      let name = slot.slot-getter.debug-name;
                      res := add!(res, with-xml()
                                         text(name)
                                       end);
                      res := add!(res, with-xml()
                                         input(type => "text",
                                               name => name)
                                       end);
                      res := add!(res, with-xml() br end);
                    end;
                    reverse(res);),
                 input(type => "submit",
                       name => "add-button",
                       value => "Add")
               }
             }
           end;
         end; }
end;

add-form-helper(<zone>) end;
add-form-helper(<network>) end;
add-form-helper(<subnet>) end;
add-form-helper(<host>) end;

define method add-form (type == #"<string>")
  with-xml()
    form (action => "/edit", \method => "post")
    {
      div (class => "edit")
      {
        input(type => "text", name => "string"),
        input(type => "submit", name => "add-button", value => "Add")
      }
    }
  end;
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

               