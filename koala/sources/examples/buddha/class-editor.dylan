module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define method edit-form (object :: <object>) => (res)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object))
             let object = slot.slot-getter-method(object);
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
                       \select(name => slot.slot-name)
                       { do(for (ele in slot.slot-global-list(*config*))
                              if (ele = slot.slot-getter-method(object))
                                collect(with-xml()
                                          option(as(<string>, ele),
                                                 value => get-reference(ele),
                                                 selected => "selected")
                                        end);
                              else
                                collect(with-xml()
                                          option(as(<string>, ele),
                                                 value => get-reference(ele))
                                        end)
                              end if;
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

//simple case for lists of strings....
define method add-form (string :: <string>,
                        name :: <string>,
                        parent :: <object>) => (foo)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      {
        input(type => "text",
              name => "string"),
        input(type => "hidden",
              name => "obj-id",
              value => get-reference(parent)),
        input(type => "hidden",
              name => "obj-type",
              value => "<string>"),
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


define method add-form (object :: <object>,
                        name :: <string>,
                        parent :: <object>) => (foo) // :: <list> ?
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object))
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
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //get slot, generate select, option field for each element
             //of global list of elements...
             collect(with-xml()
                       \select(name => slot.slot-name)
                       { do(for (ele in slot.slot-global-list(*config*))
                              collect(with-xml()
                                        option(as(<string>, ele),
                                               value => get-reference(ele))
                                      end)
                            end)
                       }
                      end);
           end),
        input(type => "hidden",
              name => "obj-id",
              value => get-reference(parent)),
        input(type => "hidden",
              name => "action",
              value => "add-object"),
        input(type => "hidden",
              name => "obj-type",
              value => object-class(object).debug-name),
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
                                   value => get-reference(object)),
                             input(type => "hidden",
                                   name => "remove-this",
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
    res := add!(res,
                add-form(make(slot.slot-type), slot.slot-name, object));
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

define method respond-to-post
    (page == #"edit", request :: <request>, response :: <response>)
  let errors = #();
  let action = as(<symbol>, get-query-value("action"));
  let object-string = get-query-value("obj-id");
  let handler <buddha-form-warning>
    = method(e :: <buddha-form-warning>, next-handler :: <function>)
          errors := add!(errors, e)
      end;
  block(return)
    //add, save, remove... we may not need this here...
    let object = element($obj-table, object-string, default: #f);
    unless (object)
      signal(make(<buddha-form-error>,
                  error: concatenate("Unknown object: ", object-string)));
    end;
    select (action)
      #"add-object" => add-object(object, request);
      #"remove-object" => remove-object(object, request);
      #"save-object" => save-object(object, request);
      otherwise => make(<buddha-form-error>,
                        error: concatenate("Unknown action: ",
                                           as(<string>, action)));
    end select;
  exception (e :: <buddha-form-error>)
    errors := add!(errors, e);
    return();
  end;
  respond-to-get(#"edit", request, response, errors: errors);
end;

define method add-object (parent-object :: <object>, request :: <request>)
  //look what type of object needs to be generated
  let type = get-query-value("obj-type");
  //if <string>, that's easy
  if (type = "<string>")
    let value = get-query-value("string");
    parent-object := add!(parent-object, value);
  else
    //more complex objects:
    let object = make(type); //XXX won't work... type is a string...
    //data-slots ref-slots needs to be read and sanity checked
    for (slot in data-slots(object))
      let value = as(slot.slot-type, get-query-value(slot.slot-name));
      //then set slots of object and add to parent list..
      slot.slot-setter-method(object, value);
    end;
    for (slot in reference-slots(object))
      let value = element($obj-table,
                          get-query-value(slot.slot-name),
                          default: #f);
      slot.slot-setter-method(object, value);
    end;
    parent-object := add!(parent-object, object);
  end;
  //also may need to be added to other (global) lists
end;

define method remove-object (parent-object :: <object>, request :: <request>)
  //read object value, get it from $obj-table
  let object = element($obj-table,
                       get-query-value("remove-this"),
                       default: #f);
  //sanity type-check
  //remove from parent list and other has-a references
  parent-object := remove!(parent-object, object);
  //it may need to be removed from several (global) lists...
end;

define method save-object (object :: <object>, request :: <request>)
  //data-slots and ref-slots may have changed...
  for (slot in data-slots(object))
    //convert value to type of slot
    let value = get-query-value(slot.slot-name);
    if (slot.slot-type = <boolean>)
      if (value = slot.slot-name)
        value := #t;
      else
        value := #f;
      end;
    else
      value := as(slot.slot-type, value);
    end;
    //do more error checking //maybe use another generic function, not as?
    //slot-setter! (only if not same object)
    if (value & (value ~= slot.slot-getter-method(object)))
      slot.slot-setter-method(value, object);
    end;
  end;
  //now something completely different
  for (slot in reference-slots(object))
    //get new value via reference
    let value = element($obj-table,
                        get-query-value(slot.slot-name),
                        default: #f);
    //error check it!
    //slot-setter!
    let current-object = slot.slot-getter-method(object);
    if (value & (value ~= current-object))
      //remove old object from list of objects of referenced object
      let class-name = debug-name(object-class(object));
      let class-getter-name = concatenate(copy-sequence(class-name,
                                                        start: 1,
                                                        end: class-name.size - 1),
                                          "s");
      let list-slot = choose(method(x)
                                 x.slot-name = class-getter-name
                             end,
                             list-reference-slots(current-object))[0];

      let old-list = list-slot.slot-getter-method(current-object);
      old-list := remove!(old-list, object);

      //set slot in object
      slot.slot-setter-method(value, object);

      //add new object to list of objects of referenced object
      let new-list = list-slot.slot-getter-method(value);
      new-list := add!(new-list, object);
    end;
  end;
end;
