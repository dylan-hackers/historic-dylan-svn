Module:    date-gadgets
Synopsis:  DUIM gadgets for selecting and entering dates and times
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

// The value used to represent a null date - ie. a date that is blank or
// not entered.
define constant $no-date-entered = #"no-date-entered";

// A type of value-gadget for dates. The no-date-valid? initialisation keyword
// should be #t if you wish the ability for dates to be blank. The value of the
// gadget is #"no-date-entered" if no date has been entered, an instance of <date>
// if a valid date has been entered, or #f if an invalid date or the gadget is in
// an invalid state.
define abstract class <date-gadget> (<value-gadget>)
    slot date-gadget-no-date-valid? = #t, init-keyword: no-date-valid?:;
end class <date-gadget>;

// A gadget for selecting and entering a date in dd/mm/yyyy format
define abstract class <date-selection-field> 
    (<bordered-gadget-mixin>,
     <date-gadget>,
     <basic-value-gadget>)
    inherited slot gadget-value = $no-date-entered;
end class <date-selection-field>;

// An implementation of <date-selection-field> for win32 which uses
// the standard microsoft common date controls. This requires the COMMCTRL.DLL
// file to be greater than version 4.70. I should probably check this on 
// initialisation and raise some sort of error...
define sealed class <win32-date-selection-field> 
    (<win32-gadget-mixin>, 
     <date-selection-field>, 
     <leaf-pane>)
end class <win32-date-selection-field>;

// By default, creating a <date-selection-field> results in the win32
// version being used.
define sealed method class-for-make-pane( 
    framem :: <win32-frame-manager>, 
    class == <date-selection-field>, 
    #key) 
 => (class :: <class>, options :: false-or(<sequence>))
   values(<win32-date-selection-field>, #f);
end method class-for-make-pane;

define sealed method make-gadget-control(
    gadget :: <win32-date-selection-field>,
    parent :: <HWND>,
    options,
    #key x, y, width, height)
 => (handle :: <HWND>)
   let ext-style = if(gadget.border-type == #"none") 0 else $WS-EX-CLIENTEDGE end;
   let handle :: <HWND> = CreateWindowEx(ext-style,
                                         "SysDateTimePick32",
                                         "",
                                         %logior(options, 
                                                 $WS-GROUP, 
                                                 $WS-TABSTOP,
                                                 if(gadget.date-gadget-no-date-valid?)
                                                   $DTS-SHOWNONE
                                                 else 
                                                   0
                                                 end if
                                                 ),
                                         x, y, width, height,
                                         parent,
                                         $null-hMenu,
                                         application-instance-handle(),
                                         $NULL-VOID);
   // Set the initial date value
   set-control-date(handle, gadget.gadget-value);
     
   handle;
end method make-gadget-control;

// Currently the <space-requirement> values are hard coded to values that
// display right on my machine. I need to find out how to set these
// properly...
define sealed method do-compose-space
    (gadget :: <win32-date-selection-field>, #key width, height)
 => (space-requirement :: <space-requirement>)
  let _port = port(gadget);
  let text-style = get-default-text-style(_port, gadget);
  let line-height = font-height(text-style, _port);
  let char-width  = font-width(text-style, _port);
  
  make(<space-requirement>,
       width: width | 50, 
       min-width: 50, 
       max-width: $fill,
       height: height | 25, 
       min-height: 25, max-height: $fill); 
end method do-compose-space;

// $DTN-DATETIMECHANGE is received from Windows when the user has
// changed the date in the control.
define sealed method handle-notify
    (gadget :: <win32-date-selection-field>, mirror :: <window-mirror>,
     wParam, lParam,
     id :: <integer>, code :: <integer>)
 => (handled? :: <boolean>)
  select (code)
    $DTN-DATETIMECHANGE => 
      handle-date-time-change(gadget, 
        c-type-cast(<LPNMDATETIMECHANGE>, lparam)); #t;
    otherwise => next-method();
  end;
end method handle-notify; 

// The user has changed the date so set the gadget-value of the gadget
// to represent the correct date.
define sealed method handle-date-time-change( gadget :: <win32-date-selection-field>, dtm :: <LPNMDATETIMECHANGE> )
 => ()
  let date = select(dtm.dwFlags-value)
               $GDT-NONE => $no-date-entered;
               $GDT-VALID => 
                 block()
                   let st = dtm.st-value;
                   make(<date>, year: st.wYear-value, month: st.wMonth-value, day: st.wDay-value);
                 end block;
               otherwise => #f;
             end select;
  distribute-value-changed-callback(gadget, date);  
end method handle-date-time-change;

// Called by DUIM when the gadget-value of the gadget has changed. 
// This is where we set the value of the windows control to reflect
// the gadget-value of the gadget.
define sealed method note-gadget-value-changed 
    (gadget :: <win32-date-selection-field>) => ()
  next-method();
  let mirror = sheet-direct-mirror(gadget);
  mirror & set-control-date(mirror.window-handle, gadget.gadget-value)
end method note-gadget-value-changed;

// Given a date, set the windows date control to display that date.
define sealed method set-control-date( hwnd :: <HWND>, date )
  select( date )
    $no-date-entered => SendMessage(hwnd, $DTM-SETSYSTEMTIME, $GDT-NONE, 0);
    otherwise => 
        with-stack-structure( st :: <LPSYSTEMTIME> )
          st.wYear-value := date.date-year;
          st.wMonth-value := date.date-month;
          st.wDay-value := date.date-day;
          st.wDayOfWeek-value := 0;
          st.wHour-value := 0;
          st.wMinute-value := 0;
          st.wSecond-value := 0;
          st.wMilliseconds-value := 0;
          SendMessage(hwnd, $DTM-SETSYSTEMTIME, $GDT-VALID, pointer-address(st));
        end with-stack-structure;
  end select;
end method set-control-date;
    
begin
  // Library initialization starts here ...
  initialize-win32-common-controls-ex($ICC-DATE-CLASSES);
end;
