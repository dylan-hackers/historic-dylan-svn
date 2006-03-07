Module: hierarchical-table


define class <hierarchical-table> (<mutable-explicit-key-collection>, <stretchy-collection>)
  slot parent-table :: false-or(<hierarchical-table>),
    init-value: #f, init-keyword: parent:;
  constant slot object-table :: <object-table> = make(<object-table>);
end class;
          
define sealed domain make(singleton(<hierarchical-table>));
          
define sealed domain initialize(<hierarchical-table>);
          
define method element-setter
    (value, table :: <hierarchical-table>, key)
 => (value);
  element-setter(value, table.object-table, key)
end method;
          
define method element
    (table :: <hierarchical-table>, key, #key default = $unsupplied)
 => (value);
  block (return)
    for (search-table = table then search-table.parent-table,
         while: search-table)
      let value = element(search-table.object-table, key, default: $unfound);
      if (found?(value))
        if (search-table == table)
          return(value)
        else
          return(element(table.object-table, key) := value)
        end if;
      end;
    end for;
    if (supplied?(default))
      default
    else
      error("element %= not found");
    end if
  end block
end method;
            
define method remove-key!
    (table :: <hierarchical-table>, key)
 => (present? :: <boolean>);
  let present? = #f;
  for (search-table = table then search-table.parent-table,
       while: search-table)
    present? := remove-key!(search-table, key) | present?;
  end for;
  present?
end method;
          
define method forward-iteration-protocol
    (table :: <hierarchical-table>)
 => (initial-state, limit,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>, current-element :: <function>,
     current-element-setter :: <function>, copy-state :: <function>);
  for (other-table = table.parent-table then other-table.parent-table,
       while: other-table)
    for (item keyed-by key in other-table.object-table)
      unless (found?(element(table.object-table, key, default: $unfound)))
        table.object-table[key] := item;
      end unless;
    end for;
  end for;
  table.parent-table := #f;
  forward-iteration-protocol(table.object-table)
end method;
          
