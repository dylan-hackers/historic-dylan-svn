Module:       vanilla-duim
Synopsis:     Vanilla back-end
Author:	   Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Vanilla port class

define sealed class <vanilla-port> (<basic-port>)
end class <vanilla-port>;

define method initialize (_port :: <vanilla-port>, #key) => ()
  next-method();
end method initialize;
 

define sideways method class-for-make-port
    (type == #"vanilla", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  ignore(initargs);
  //--- You might want to return a new set of initargs here...
  values(<vanilla-port>, #f)
end method class-for-make-port;

// #"local" is the 'default' port type used if none is specified
define sideways method class-for-make-port
    (type == #"local", #rest initargs, #key)
 => (class :: <class>, initargs :: false-or(<sequence>))
  apply(class-for-make-port, #"vanilla", initargs)
end method class-for-make-port;

define method port-type (_port :: <vanilla-port>) => (type :: <symbol>)
  #"vanilla"
end method port-type;

define method port-name (_port :: <vanilla-port>) => (name :: false-or(<string>))
  #f
end method port-name;


/// Beeping, etc

define method force-display (_port :: <vanilla-port>)
  //--- Do it
end method force-display;

define method synchronize-display (_port :: <vanilla-port>)
  //--- Do it
end method synchronize-display;

define method beep (_port :: <vanilla-port>)
  //--- Do it
end method beep;


/// Pointers

define method do-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <sheet>)
 => (x :: <integer>, y :: <integer>)
  //--- Get pointer position w.r.t. sheet
end method do-pointer-position;

define method do-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <display>)
 => (x :: <integer>, y :: <integer>)
  //--- Get pointer position w.r.t. the display
end method do-pointer-position;

define method do-set-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <sheet>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. sheet
end method do-set-pointer-position;

define method do-set-pointer-position
    (_port :: <vanilla-port>, pointer :: <pointer>, sheet :: <display>, 
     x :: <integer>, y :: <integer>) => ()
  //--- Set pointer position w.r.t. the display
end method do-set-pointer-position;

define method do-set-pointer-cursor
    (_port :: <vanilla-port>, pointer :: <pointer>, cursor :: <cursor>) => ()
  //--- Set the pointer cursor
end method do-set-pointer-cursor;


define method do-set-sheet-cursor
    (_port :: <vanilla-port>, sheet :: <sheet>, cursor :: <cursor>) => ()
  //--- Set the cursor for the sheet
end method do-set-sheet-cursor;


//--- Define the keysyms for the port
