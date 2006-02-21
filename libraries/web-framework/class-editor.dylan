module: web-framework
author: Hannes Mehnert <hannes@mehnert.org>

define class <web-form-warning> (<condition>)
  constant slot error-string :: <string>, required-init-keyword: warning:;
end;

define class <web-success> (<web-form-warning>)
end;

define class <web-error> (<error>)
  constant slot error-string :: <string>, required-init-keyword: error:;
end;

define open generic respond-to-get
    (page, request :: <request>, response :: <response>, #key errors);

define open generic respond-to-post
    (page, request :: <request>, response :: <response>);

define open generic check (object :: <object>, #key test-result)
 => (res :: <boolean>);


define method check (object :: <object>, #key test-result = 0)
 => (res :: <boolean>)
  #t;
end;

define method edit-form (object :: <object>, #key refer, xml) => (res)
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
                              if (visible?(ele))
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
                                end;
                              end if;
                            end)
                       }
                      end);
           end),
        input(type => "hidden",
              name => "parent-object",
              value => get-reference(object)),
        input(type => "hidden",
              name => "action",
              value => "save-object"),
        input(type => "hidden",
              name => "refer-to",
              value => if (refer)
                         refer
                       else
                         get-url-from-type(object.object-class)
                       end),
        do(if(xml) xml end),
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
                        #key fill-from-request,
                        refer,
                        xml) => (foo)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      {
        input(type => "text",
              name => "string"),
        input(type => "hidden",
              name => "parent-object",
              value => get-reference(parent)),
        input(type => "hidden",
              name => "object-type",
              value => get-reference(type)),
        input(type => "hidden",
              name => "action",
              value => "add-object"),
        do(if(xml) xml end),
        do(if(refer)
             with-xml()
               input(type => "hidden",
                     name => "refer-to",
                     value => refer)
             end
           end),
        input(type => "submit",
              name => "add-button",
              value => concatenate("Add to ", name))
      }
    }
  end;
end;


define method add-form (object-type :: subclass(<object>),
                        name :: false-or(<string>),
                        parent :: <object>,
                        #key fill-from-request,
                        refer,
                        xml) => (foo) // :: <list> ?
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      { do(for (slot in data-slots(object-type))
             collect(with-xml() text(concatenate(slot.slot-name, ": ")) end);
             //here we should have at least a seperation between integer,
             //strings and lists... or should we implement all lists with
             //has-many?
             let value = default(slot);
             let query-value = get-query-value(slot.slot-name);
             if (fill-from-request & (query-value & query-value ~= ""))
               value := query-value;
             end;
             if (slot.slot-type = <boolean>)
               collect(edit-slot(value, slot.slot-name));
             else
               if (value)
                 collect(edit-slot(value, slot.slot-name));
               else
                 collect(with-xml() input(type => "text",
                                          name => slot.slot-name)
                         end);
               end if;
             end;
             if (slot.default-help-text)
               collect(with-xml()
                         text(concatenate(" defaults to: ", slot.default-help-text))
                       end);
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
              name => "parent-object",
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
        do(if(xml) xml end),
        input(type => "submit",
              name => "add-button",
              value => if (name) concatenate("Add to ", name) else "Add" end)
      }
    }
  end;
end;

define method remove-form (object :: <object>, parent :: <object>,
                           #key url :: <string> = "edit",
                           xml)
  with-xml()
    form(action => "/edit", \method => "post")
    { div(class => "edit")
      {
         input(type => "hidden",
               name => "refer-to",
               value => url),
         input(type => "hidden",
               name => "parent-object",
               value => get-reference(parent)),
         input(type => "hidden",
               name => "remove-this",
               value => get-reference(object)),
         input(type => "hidden",
               name => "action",
               value => "remove-object"),
         do(if(xml) xml end),
         input(type => "submit",
               name => "remove-button",
               value => "Remove")
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
  let object-string = get-query-value("parent-object");
  let object = get-object(object-string);
  let handler <web-form-warning>
    = method(e :: <web-form-warning>, next-handler :: <function>)
          errors := add!(errors, e)
      end;
  block(return)
    //add, save, remove... we may not need this here...
    unless (object)
      signal(make(<web-error>,
                  error: concatenate("Unknown object: ", object-string)));
    end;
    select (action)
      #"add-object" => add-object(object, request);
      #"remove-object" => remove-object(object, request);
      #"save-object" => save-object(object, request);
      otherwise => signal(make(<web-error>,
                               error: concatenate("Unknown action: ",
                                                  as(<string>, action))));
      end select;
  exception (e :: <web-error>)
    errors := add!(errors, e);
    return();
  exception (e :: <error>)
    errors := add!(errors, make(<web-error>,
                                error: format-to-string("%=", e)));
    return();
  end; 
  let referer = get-query-value("refer-to");
  let elements = split(referer, '-');
  block(return)
    unless ((elements.size = 2) & elements[1] = "detail")
      if ((action = #"add-object")
        & (any?(rcurry(instance?, <web-error>), errors)))
        respond-to-get(#"add", request, response, errors: errors);
        return();
      end;
    end;
    referer := if (referer)
                 as(<symbol>, referer);
               else
                 #"edit";
               end;
    respond-to-get(referer, request, response, errors: if (errors.size > 0) errors else #f end);
  end;
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
      unless ((slot.slot-type = <boolean>) | value)
        value := slot.default-function(object);
        unless (value)
          signal(make(<web-error>,
                      error: concatenate("Please specify ",
                                         slot.slot-name,
                                         " correctly!")));
        end unless;
      end;
      slot.slot-setter-method(value, object);
    end;
    for (slot in reference-slots(object-type))
      let value = get-object(get-query-value(slot.slot-name));
      slot.slot-setter-method(value, object);
    end;
    //sanity check it
    let command = make(<add-command>,
                       arguments: list(object, parent-object));
    redo(command);
    let change = make(<change>,
                      command: command);
    *changes* := add!(*changes*, change);
    signal(make(<web-success>,
                warning: concatenate("Added ",
                                     get-url-from-type(object-type),
                                     ": ", show(object))));
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
  signal(make(<web-success>,
              warning: concatenate("Removed ",
                                   get-url-from-type(object.object-class),
                                   ": ", show(object))));
end;

define method parse (name, type)
  let value = get-query-value(name);
  if (type = <boolean>)
    if (value = name)
      #t;
    else
      #f;
    end;
  else
    if (value & (value ~= ""))
      if (type = <string>)
        value;
      elseif (type = <integer>)
        string-to-integer(value)
      else
        as(type, value);
      end;
    else
      #f;
    end;
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
  if (slots.size > 0)
    let command = make(<edit-command>,
                       arguments: list(object, slots));
    redo(command);
    //check world, if broken, do a rollback!
    let change = make(<change>,
                      command: command);
    *changes* := add!(*changes*, change);
    let slot-names = apply(concatenate,
                           map(method(x)
                                   concatenate(x.slot-name, " to ",
                                               show(x.new-value), "  ")
                               end, slots));
    signal(make(<web-success>,
                warning: concatenate("Saved \"",
                                     get-url-from-type(object.object-class),
                                     "\", ",
                                     show(object),
                                     " changed slots: ",
                                     slot-names)));
  end;
end;

define method get-url-from-type (type) => (string :: <string>)
  copy-sequence(type.debug-name,
                start: 1,
                end: type.debug-name.size - 1)
end;

