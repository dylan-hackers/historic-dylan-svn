Module:    dylan-user
Synopsis:  Additional Win32 Common Control functionality
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define module win32-common-controls-ex
  use dylan;
  use c-ffi;
  use win32-kernel;
  use win32-common;
  use win32-controls;

  // Add binding exports here.
  export initialize-win32-common-controls-ex,
         $ICC-DATE-CLASSES,
         $DTM-FIRST,
         $DTM-GETMONTHCAL,
         $DTM-GETSYSTEMTIME,
         $DTM-SETSYSTEMTIME,
         $DTN-FIRST,
         $DTN-LAST,
         $DTN-DATETIMECHANGE,
         $DTN-DROPDOWN,
         $DTN-CLOSEUP,
         $GDT-VALID,
         $GDT-NONE,
         $GDT-ERROR,
         $DTS-SHOWNONE,
         <NMDATETIMECHANGE>,
         st-value,
         <LPNMDATETIMECHANGE>;
end module win32-common-controls-ex;
