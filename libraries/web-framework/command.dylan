module: web-framework
author: Hannes Mehnert <hannes@mehnert.org>

define abstract class <command> (<object>)
  constant slot arguments :: <list>, init-keyword: arguments:;
end;

define class <add-command> (<command>)
end;

define class <remove-command> (<command>)
end;

define class <edit-command> (<command>)
end;

define method reverse-command (c :: <add-command>)
  make(<remove-command>, arguments: c.arguments);
end;

define method reverse-command (c :: <remove-command>)
  make(<add-command>, arguments: c.arguments);
end;

define method reverse-command (c :: <edit-command>)
  make(<edit-command>,
       arguments: list(c.arguments[0],
                       map(reverse-triple, c.arguments[1])));
end;

define method reverse-triple (t :: <triple>)
  make(<triple>,
       slot-name: t.slot-name,
       old-value: t.new-value,
       new-value: t.old-value);
end;

define method execute (command :: <add-command>)
  add-to-list;
end;

define method unexecute (command :: <add-command>)
  remove-from-list;
end;

define method execute (command :: <remove-command>)
  remove-from-list;
end;

define method unexecute (command :: <remove-command>)
  add-to-list;
end;

define method execute (command :: <edit-command>)
  set-slots;
end;

define method unexecute (command :: <edit-command>)
  unset-slots;
end;

define method print-xml (command :: <command>)
  let object = command.arguments[0];
  let type = get-url-from-type(object.object-class);
  with-xml()
    a(concatenate(type, " ", show(object)),
      href => concatenate("/", type, "-detail?", type, "=", get-reference(object)))
  end;
end;

define method print-xml (command :: <add-command>)
  list(with-xml()
         text("Added ")
       end,
       next-method());
end;

define method print-xml (command :: <remove-command>)
  list(with-xml()
         text("Removed ")
       end,
       next-method())
end;

define method print-xml (command :: <edit-command>)
  list(with-xml()
         text("Edited ")
       end,
       next-method(),
       with-xml()
         text(", changed following slots: ")
       end,
       with-xml()
         ul {
             do(map(print-xml, command.arguments[1]))
         }
       end)
end;

define method print-xml (triple :: <triple>)
  with-xml()
    li { text(concatenate(triple.slot-name,
                          " from \"", show(triple.old-value),
                          "\" to \"", show(triple.new-value), "\""))
    }
  end;
end;

define method add-to-list (object :: <object>, list :: <collection>)
  //only add if not in list
  unless (any?(method(x) x = object end, list))
    if (check(object))
      list := sort!(add!(list, object))
    end;
  end
end;

define method remove-from-list (object :: <object>, list :: <collection>)
  list := remove!(list, object);
end;

define method add-to-list (object :: <object>, table :: <table>)
  if (check(object))
    table[key(object)] := object;
  end;
end;

define method remove-from-list (object :: <object>, list :: <table>)
  remove-key!(list, key(object));
end;

define method set-slots (object :: <object>, slots :: <list>)
  map(method(x)
          set-slot(x.slot-name, object, x.new-value)
      end, slots);
  let handler <web-error>
    = method(e :: <web-error>, next-handler :: <function>)
          unset-slots(object, slots);
          next-handler();
      end;
  check(object, test-result: 1);
  //check for consistency, on error, do a rollback
end;

define method unset-slots (object :: <object>, slots :: <list>)
  map(method(x)
          set-slot(x.slot-name, object, x.old-value)
      end, slots);
  let handler <web-error>
    = method(e :: <web-error>, next-handler :: <function>)
          set-slots(object, slots);
          next-handler();
      end;
  check(object, test-result: 1);
  //check for consistency, on error, do a rollback
end;

define method set-slot (name :: <string>,
                        object :: <object>,
                        value :: <object>)
    local method find-slot (slots)
            block(return)
              for (slot in slots)
                if (slot.slot-name = name)
                  return(slot);
                end;
              end;
              #f;
            end;
          end;
  let slot = find-slot(data-slots(object.object-class));
  unless (slot)
    slot := find-slot(reference-slots(object.object-class));
  end;
  if (slot)
    slot.slot-setter-method(value, object)
  end;
end;

define method undo (command)
  apply(unexecute(command), command.arguments);
end;

define method redo (command)
  apply(execute(command), command.arguments);
end;

