Module:    date-time-play
Synopsis:  A brief description of the project.
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.



define frame <template-frame> (<simple-frame>)
  pane button-pane (frame)
    make(<push-button>, 
      activate-callback: method(g) 
        frame.date2-pane.gadget-value := frame.date1-pane.gadget-value;
        frame.text-pane.gadget-value := frame.date1-pane.gadget-value.as-string;
        frame.date1-pane.gadget-value := $no-date-entered;
      end);
  pane text-pane (frame)
    make(<text-field>);
  pane date1-pane (frame)
    make(<date-selection-field>, value-changed-callback: on-date-changed);
  pane date2-pane (frame)
    make(<date-selection-field>, 
      value-changed-callback: on-date-changed,
      value: make(<date>, year: 1990, month: 12, day: 9),
      no-date-valid?: #t);
  layout (frame)
    vertically () 
      frame.button-pane;
      frame.text-pane;
      frame.date1-pane;
      frame.date2-pane 
    end;
  keyword title: = $application-name;
end frame <template-frame>;

define method as-string( date :: <date> ) => (s :: <string>)
  format-to-string("%d-%d-%d", date.date-day, date.date-month, date.date-year);
end method as-string;

define method as-string( date == $no-date-entered) => (s :: <string>)
  "No Date Entered.";
end method as-string;

define method as-string( date == #f) => (s :: <string>)
  "Date is invalid.";
end method as-string;

define method on-date-changed( gadget )
    gadget.sheet-frame.text-pane.gadget-value := gadget.gadget-value.as-string;
end method on-date-changed;


/// Start the template

define method start-template
    () => ()
  start-frame(make(<template-frame>));
end method start-template;
