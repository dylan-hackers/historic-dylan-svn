Module:    win32-common-controls-ex
Synopsis:  Additional Win32 Common Control functionality
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define C-struct <INITCOMMONCONTROLSEX>
  sealed inline-only slot dwSize-value :: <DWORD>;
  sealed inline-only slot dwICC-value :: <DWORD>;
  pointer-type-name: <LPINITCOMMONCONTROLSEX>;
end C-struct <INITCOMMONCONTROLSEX>;
  
define inline-only C-function init-common-controls-ex
  parameter init :: <LPINITCOMMONCONTROLSEX>;
  result value :: <BOOL>;
  c-name: "InitCommonControlsEx", c-modifiers: "__stdcall";
end;
define inline-only constant $ICC-DATE-CLASSES = #x00000100;

define function initialize-win32-common-controls-ex(#rest args) 
  with-stack-structure( init :: <LPINITCOMMONCONTROLSEX> )
    init.dwSize-value := size-of(<INITCOMMONCONTROLSEX>);
    init.dwICC-value := apply(%logior, args);
    init-common-controls-ex(init);
  end with-stack-structure;
end function initialize-win32-common-controls-ex;


define constant $DTM-FIRST = #x1000;
define constant $DTM-GETMONTHCAL = $DTM-FIRST + 8;
define constant $DTM-GETSYSTEMTIME = $DTM-FIRST + 1;
define constant $DTM-SETSYSTEMTIME = $DTM-FIRST + 2;
define constant $DTN-FIRST = -760;
define constant $DTN-LAST = -799;
define constant $DTN-DATETIMECHANGE = $DTN-FIRST + 1;
define constant $DTN-DROPDOWN = $DTN-FIRST + 6;
define constant $DTN-CLOSEUP = $DTN-FIRST + 7;
define constant $GDT-VALID = 0;
define constant $GDT-NONE = 1;
define constant $GDT-ERROR = -1;
define constant $DTS-SHOWNONE = #x0002;

define C-struct <NMDATETIMECHANGE>
  sealed inline-only slot hdr-value      :: <NMHDR>;
  sealed inline-only slot dwFlags-value    :: <DWORD>;
  sealed inline-only slot st-value     :: <SYSTEMTIME>;
  pack: 1;
  pointer-type-name: <LPNMDATETIMECHANGE>;
end C-struct <NMDATETIMECHANGE>;


