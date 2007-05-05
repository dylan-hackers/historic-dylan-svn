module: change
author: Hannes Mehnert <hannes@mehnert.org>

define web-class <change> (<object>)
  data author :: <string>;
  data date :: <date>;
  data command :: <command>;
end;

define method initialize (change :: <change>, #rest rest, #key, #all-keys)
  next-method();
  change.date := current-date();
  change.author := if (authenticated-user())
                     authenticated-user().username;
                   else
                     "foo"
                   end;
end;

define method setup (change :: <change>) => ()
  redo(change.command);
end;
define method undo (change :: <change>)
  //undo the change
  undo(change.command);
  //on success, make a new <change> object, add it to *changes*
  let change = make(<change>,
                    command: reverse-command(change.command));
  save(change);
end;

define method print-xml (date :: <date>, #key base-url)
  list(with-xml()
         text(print-change(date))
       end);
end;

define method print-change (date :: <date>, #key base-url)
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
  concatenate(integer-to-string(iday), " ",
              $month-names[imonth - 1], "  ",
              //integer-to-string(iyear), "  ",
              wrap0(ihours), ":",
              wrap0(iminutes), ":",
              wrap0(iseconds));
end;
define method print-xml (change :: <change>, #key base-url)
  concatenate(print-xml(change.date),
              list(with-xml()
                     text(concatenate(" by ", change.author, ": "))
                   end),
              print-xml(change.command, base-url: base-url));
end;

define method print-change (change :: <change>, #key base-url)
  concatenate(print-change(change.date), " by ", change.author, ": ",
              print-change(change.command, base-url: base-url))
end;
