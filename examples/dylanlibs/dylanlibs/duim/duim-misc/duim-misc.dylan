Module:    duim-misc
Synopsis:  Extensions to DUIM library
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define method set-extended-table-style ( table :: <table-control> ) => ()
  let hwnd = table.window-handle;
  let LVS-EX-FULLROWSELECT = #x20;
  let LVM-FIRST = #x1000;
  let LVM-GETEXTENDEDLISTVIEWSTYLE = LVM-FIRST + #x37;
  let LVM-SETEXTENDEDLISTVIEWSTYLE = LVM-FIRST + #x36;
  let lstyle = SendMessage(hwnd, LVM-GETEXTENDEDLISTVIEWSTYLE, 0, 0);
  lstyle := %logior(lstyle, LVS-EX-FULLROWSELECT);
  SendMessage(hwnd, LVM-SETEXTENDEDLISTVIEWSTYLE, 0, lstyle);
end method set-extended-table-style;

