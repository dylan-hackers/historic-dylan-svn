module: changes
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <change> (<object>)
  data author :: <string>;
  data date :: <date>;
  data command :: <command>;
end;

define method initialize (change :: <change>, #rest rest, #key, #all-keys)
  next-method();
  change.date := current-date();
  change.author := current-user().username;
end;

define method undo (change :: <change>)
  //undo the change
  undo(change.command);
  //on success, make a new <change> object, add it to *changes*
  let change = make(<change>,
                    command: reverse-command(change.command));
  save(change);
end;

define method print-xml (date :: <date>)
  let $month-names
    = #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  let (iyear, imonth, iday, ihours, iminutes,
       iseconds, day-of-week, time-zone-offset)
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
