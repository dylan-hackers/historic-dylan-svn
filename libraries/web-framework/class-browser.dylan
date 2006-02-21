module: web-framework
author: Hannes Mehnert <hannes@mehnert.org>

define method browse (object-type :: subclass(<object>),
                      markup :: <function>)
  let res = make(<stretchy-vector>);
  for (slot in data-slots(object-type))
    res := add!(res, markup(#"data", slot));
  end;
  for (slot in reference-slots(object-type))
    res := add!(res, markup(#"reference", slot));
  end;
  for (slot in list-reference-slots(object-type))
    res := add!(res, markup(#"list", slot));
  end;
  res;
end;

define method to-list (key == #"data", slot, object)
  with-xml()
    li(concatenate(slot.slot-name,
                   ":",
                   show(value(object, slot))))
  end;
end;

define method to-list (key == #"reference", slot, object)
  with-xml()
    li {
       text(concatenate(slot.slot-name, ":")),
       do(show-with-link(value(object, slot), "/browse"))
       }
  end;
end;

define method to-list (key == #"list", slot, object)
  with-xml()
    li {
       text(concatenate(slot.slot-name, ":")),
       do(if (value(object, slot).size > 0)
            collect(with-xml()
                      ul {
                         do(for(ele in value(object, slot))
                              collect(with-xml()
                                        li { do(show-with-link(ele, "/browse")) }
                                      end)
                            end)
                         }
                    end)
          end)
        }
  end;
end;

define method show-with-link (object :: <object>, url :: <string>)
  with-xml()
    a(show(object),
      href => concatenate(url,
                          "?obj-id=",
                          get-reference(object)))
  end;
end;

define method browse-list (object :: <object>) => (res)
  with-xml()
    ul { do(browse(object.object-class, rcurry(to-list, object))) }
  end;
end;

define method to-table-header (key, slot)
  with-xml()
    th(slot.slot-name)
  end;
end;

define method to-table (key == #"data", slot, object)
  with-xml()
    td(show(value(object, slot)))
  end;
end;

define method to-table (key == #"reference", slot, object)
  with-xml()
    td{ do(show-with-link(value(object, slot), "/browse")) }
  end;
end;

define method to-table (key == #"list", slot, object)
  with-xml()
    td {
       a(show(size(value(object, slot))),
         href => concatenate("/", get-url-from-type(slot.slot-type),
                             "?obj-id=", get-reference(value(object, slot)),
                             "&obj-parent=", get-reference(object)))
       }
  end;
end;

define method browse-table (headline :: subclass(<object>),
                            object :: <object>) => (res)
  with-xml()
    table {
          tr { do(browse(headline, to-table-header)), th("Remove"), th("Edit") },
          do(for (ele in object)
               collect(with-xml()
                         tr {
                            do(browse(headline, rcurry(to-table, ele))),
                            td {
                               do(remove-form(ele,
                                              object,
                                              url: get-url-from-type(headline)))
                            },
                            td { a("Edit", href => concatenate("/edit?obj=",
                                                               get-reference(ele))) }
                         }
                        end)
              end)
           }
  end;
end;

define method value (object :: <object>, slot :: <slot>) => (value :: <object>)
  slot.slot-getter-method(object)
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


