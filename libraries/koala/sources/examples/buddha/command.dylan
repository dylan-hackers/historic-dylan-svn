module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define abstract class <command> (<object>)
  slot execute :: <function>, init-keyword: execute:;
  slot unexecute :: <function>, init-keyword: unexecute:;
  slot arguments :: <list>, init-keyword: arguments:;
end;

define class <add-command> (<command>)
  inherited slot execute = add-to-list;
  inherited slot unexecute = remove-from-list;
end;

define class <remove-command> (<command>)
  inherited slot execute = remove-from-list;
  inherited slot unexecute = add-to-list;
end;

define class <edit-command> (<command>)
  inherited slot execute = set-slots;
  inherited slot unexecute = unset-slots;
end;

define method print-xml (command :: <command>)
  let object = command.arguments[0];
  with-xml()
    a(concatenate(get-url-from-type(object.object-class), " ",
                  as(<string>, object)),
      href => concatenate("/browse?obj-id=", get-reference(object)))
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
                          " from ", show(triple.old-value),
                          " to ", show(triple.new-value)))
    }
  end;
end;

define method add-to-list (object :: <object>, list :: <collection>)
  //only add if not in list
  unless (any?(method(x) x = object end, list))
    list := sort!(add!(list, object))
  end
end;

define method remove-from-list (object :: <object>, list :: <collection>)
  list := remove!(list, object);
end;

define method set-slots (object :: <object>, slots :: <list>)
  map(method(x)
          x.setter(x.new-value, object)
      end, slots);
end;

define method unset-slots (object :: <object>, slots :: <list>)
  map(method(x)
          x.setter(x.old-value, object)
      end, slots);
end;

define method undo (#key command = head(*commands*))
  apply(command.unexecute, command.arguments);
end;

define method redo (#key command = head(*commands*))
  apply(command.execute, command.arguments);
end;

