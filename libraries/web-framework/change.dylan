module: web-framework
author: Hannes Mehnert <hannes@mehnert.org>

define variable *changes* = #();

define method get-all-changes () => (changes :: <list>)
  *changes*;
end;

define method set-changes (changes :: <list>) => ()
  *changes* := changes;
end;

define method add-change (change :: <change>) => ()
  *changes* := add!(*changes*, change);
end;

define class <change> (<object>)
  slot author :: <string>;
  slot date :: <date>;
  slot command :: <command>, init-keyword: command:;
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
  *changes* := add!(*changes*, change);
end;

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
