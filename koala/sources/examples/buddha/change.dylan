module: buddha
author: Hannes Mehnert <hannes@mehnert.org>

define class <change> (<object>)
  slot author :: <string>;
  slot date :: <date>;
  slot command :: <command>, init-keyword: command:;
end;

define method initialize (change :: <change>, #rest rest, #key, #all-keys)
  next-method();
  change.date := current-date();
  change.author := *user*.username;
end;

define method undo (change :: <change>)
  //rollback to change
  /* let redo-start =
    block(return)
      for (ele in *changes*,
           i from 0)
        if (change = ele)
          return(i - 1);
        else
          undo(ele.command)
        end;
      end;
    end; */
  //undo the change
  undo(change.command);
  //redo all other changes
 /*  for (i from redo-start to 0 by -1)
    let ele = *changes*[i];
    redo(ele.command);
  end; */
  //on success, make a new <change> object, add it to *changes*
  let change = make(<change>,
                    command: reverse-command(change.command));
  *changes* := add!(*changes*, change);
end;

/*
define method redo (change :: <change>)
  //rollback to change
  let redo-start =
    block(return)
      for (ele in *changes*,
           i from 0)
        if (change = ele)
          return(i - 1);
        else
          undo(ele.command)
        end;
      end;
    end;
  //redo the change
  redo(change.command);
  //redo all other changes
  for (i from redo-start to 0 by -1)
    let ele = *changes*[i];
    redo(ele.command);
  end;
  //on success, make a new <change> object, add it to *changes*
  let change = make(<change>,
                    command: change.command);
  *changes* := add!(*changes*, change);
end;
*/

define method print-xml (date :: <date>)
  let $month-names
    = #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  let (iyear, imonth, iday, ihours, iminutes, iseconds, day-of-week, time-zone-offset)
    = decode-date(date);
  local method wrap0 (i :: <integer>) => (string :: <string>)
          if (i < 10)
            concatenate("0", integer-to-string(i));
          else
            integer-to-string(i)
          end;
        end;
  let printed-date
    = concatenate(integer-to-string(iday), " ",
                  $month-names[imonth - 1], "  ",
                  //integer-to-string(iyear), "  ",
                  wrap0(ihours), ":",
                  wrap0(iminutes), ":",
                  wrap0(iseconds));
  list(with-xml()
         text(printed-date)
       end);
end;

define method print-xml (change :: <change>)
  concatenate(print-xml(change.date),
              list(with-xml()
                     text(concatenate(" by ", change.author, ": "))
                   end),
              print-xml(change.command));
end;
