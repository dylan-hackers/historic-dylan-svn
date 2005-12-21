module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define method check (object :: <object>) => (res :: <boolean>)
  #t;
end;

define method edit-form (object :: <object>) => (res)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object.object-class))
             let object = slot.slot-getter-method(object);
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //XXX check if slot is initialized?
             collect(edit-slot(object, slot.slot-name));
             collect(with-xml() br end);
           end;
           for (slot in reference-slots(object.object-class))
             
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
define method add-form (type == <string>,
                        name :: <string>,
                        parent :: <object>,
                        #key fill-from-request) => (foo)
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
              name => "object-type",
              value => get-reference(type)),
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


define method add-form (object-type :: subclass(<object>),
                        name :: <string>,
                        parent :: <object>,
                        #key fill-from-request,
                        refer) => (foo) // :: <list> ?
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object-type))
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //here we should have at least a seperation between integer,
             //strings and lists... or should we implement all lists with
             //has-many?
             let value = get-query-value(slot.slot-name);
             if (slot.slot-type = <boolean>)
               if (fill-from-request & value)
                 collect(with-xml() input(type => "checkbox",
                                          name => slot.slot-name,
                                          value => slot.slot-name,
                                          checked => "checked")
                         end);
               else
                 collect(with-xml() input(type => "checkbox",
                                          name => slot.slot-name,
                                          value => slot.slot-name)
                         end);
               end;
             else
               if (fill-from-request & value)
                 collect(with-xml() input(type => "text",
                                          name => slot.slot-name,
                                          value => value)
                         end);
               else
                 collect(with-xml() input(type => "text",
                                          name => slot.slot-name)
                         end);
               end if;
             end;
             collect(with-xml() br end);
           end;
           for (slot in reference-slots(object-type))
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //get slot, generate select, option field for each element
             //of global list of elements...
             let value = get-object(get-query-value(slot.slot-name));
             collect(with-xml()
                       \select(name => slot.slot-name)
                       { do(for (ele in slot.slot-global-list(*config*))
                              if (visible?(ele))
                                if (fill-from-request & (ele = value))
                                  collect(with-xml()
                                            option(as(<string>, ele),
                                                   value => get-reference(ele),
                                                   selected => "selected")
                                          end);
                                else
                                  collect(with-xml()
                                            option(as(<string>, ele),
                                                   value => get-reference(ele))
                                          end);
                                end;
                              end;
                            end)
                       }
                      end);
           end),
        input(type => "hidden",
              name => "obj-id",
              value => get-reference(parent)),
        input(type => "hidden",
              name => "object-type",
              value => get-reference(object-type)),
        input(type => "hidden",
              name => "refer-to",
              value => if (refer) refer else get-url-from-type(object-type) end),
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
  for (slot in list-reference-slots(obj.object-class))
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
      res := add!(res, remove-form(ele, object));
      res := add!(res, with-xml() br end);
    end;
    res := add!(res, add-form(slot.slot-type, slot.slot-name, object));
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
    (page == #"edit",
     request :: <request>,
     response :: <response>)
  let errors = #();
  let action = as(<symbol>, get-query-value("action"));
  let object-string = get-query-value("obj-id");
  let object = get-object(object-string);
  let handler <buddha-form-warning>
    = method(e :: <buddha-form-warning>, next-handler :: <function>)
          errors := add!(errors, e)
      end;
  block(return)
    //add, save, remove... we may not need this here...
    unless (object)
      signal(make(<buddha-form-error>,
                  error: concatenate("Unknown object: ", object-string)));
    end;
    select (action)
      #"add-object" => add-object(object, request);
      #"remove-object" => remove-object(object, request);
      #"save-object" => save-object(object, request);
      otherwise => signal(make(<buddha-form-error>,
                               error: concatenate("Unknown action: ",
                                                  as(<string>, action))));
      end select;
  exception (e :: <buddha-form-error>)
    errors := add!(errors, e);
    return();
  exception (e :: <error>)
    errors := add!(errors, make(<buddha-form-error>,
                                error: format-to-string("%=", e)));
    return();
  end;
  let referer = if (get-query-value("refer-to"))
                  as(<symbol>, get-query-value("refer-to"));
                else
                  #"edit";
                end;
  respond-to-get(referer, request, response, errors: if (errors.size > 0) errors else #f end);
end;

define method add-object (parent-object :: <object>, request :: <request>)
  //look what type of object needs to be generated
  let object-type = get-object(get-query-value("object-type"));
  //XXX: hmm, make should probably only be done when all slots
  //are successfully parsed and then use init-keywords...
  let object = make(object-type);
  if (instance?(object, <string>))
    let value = get-query-value("string");
    parent-object := add!(parent-object, value);
  else
    //more complex objects:
    //data-slots ref-slots needs to be read and sanity checked
    for (slot in data-slots(object-type))
      let value = parse(slot.slot-name, slot.slot-type);
      //then set slots of object
      slot.slot-setter-method(value, object);
    end;
    for (slot in reference-slots(object-type))
      let value = get-object(get-query-value(slot.slot-name));
      slot.slot-setter-method(value, object);
    end;
    let command = make(<add-command>,
                       arguments: list(object, parent-object));
    let change = make(<change>,
                      command: command);
    *changes* := add!(*changes*, change);
    redo(command);
  end;
end;

define method remove-object (parent-object :: <object>, request :: <request>)
  //read object value, get it from $obj-table
  let object = get-object(get-query-value("remove-this"));
  //sanity type-check
  let command = make(<remove-command>,
                     arguments: list(object, parent-object));
  let change = make(<change>,
                    command: command);
  *changes* := add!(*changes*, change);
  redo(command);
end;

define method parse (name, type)
  let value = get-query-value(name);
  if (type = <boolean>)
    if (value = name)
      #t;
    else
      #f;
    end;
  elseif (type = <string>)
    value;
  elseif (type = <integer>)
    string-to-integer(value)
  else
    as(type, value);
  end;
end;

define class <triple> (<object>)
  constant slot old-value, init-keyword: old-value:;
  constant slot new-value, init-keyword: new-value:;
  constant slot slot-name, init-keyword: slot-name:;
end;

define method save-object (object :: <object>, request :: <request>)
  //data-slots and ref-slots may have changed...
  let slots = #();
  for (slot in data-slots(object.object-class))
    //maybe use another generic function, not as in parse?
    let value = parse(slot.slot-name, slot.slot-type);
    //can't check for if(value) here, because value can be #f and valid...
    if (value ~= slot.slot-getter-method(object))
      slots := add!(slots,
                    make(<triple>,
                         slot-name: slot.slot-name,
                         new-value: value,
                         old-value: slot.slot-getter-method(object)));
    end;
  end;
  //now something completely different
  for (slot in reference-slots(object.object-class))
    //get new value via reference
    let value = get-object(get-query-value(slot.slot-name));
    //error check it!
    let current-object = slot.slot-getter-method(object);
    if (value & (value ~= current-object))
      slots := add!(slots,
                    make(<triple>,
                         slot-name: slot.slot-name,
                         new-value: value,
                         old-value: slot.slot-getter-method(object)));
    end;
  end;
  let command = make(<edit-command>,
                     arguments: list(object, slots));
  let change = make(<change>,
                    command: command);
  *changes* := add!(*changes*, change);
  redo(command);
end;
