Module:    win32-notifications
Synopsis:  Notification of system level events like system time changing
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// A system-time-changed notification is raised when 
// the system time changes. This is done by creating a
// Win32 invisible window that does nothing but look for
// the WM_TIMECHANGED message. When that message is received
// this library will release the $system-time-changed notification.
// All threads waiting on this notification can then perform some
// action to take note of the system time changing.
define constant $time-changed-lock = make(<lock>);
define constant $system-time-changed = make(<notification>, lock: $time-changed-lock);
define variable *system-time-monitor-started* = #f;

define function start-system-time-monitor()
 => ()
  unless(*system-time-monitor-started*)
    make(<thread>,
         name: "System Time Monitor",
         function: system-time-monitor-function);
  end unless;
end function start-system-time-monitor;

define method wait-for-time-change(#key timeout = #f)
  with-lock($time-changed-lock)
    wait-for($system-time-changed, timeout: timeout);
  end with-lock;
end method wait-for-time-change;

define method system-time-monitor-function()
  *system-time-monitor-started* := #t;

  let hinstance = application-instance-handle();
  with-stack-structure(wc :: <PWNDCLASS>)
    wc.style-value := 0;
    wc.lpfnWndProc-value := WndProc;
    wc.cbClsExtra-value := 0;
    wc.cbWndExtra-value := 0;
    wc.hInstance-value := hinstance;
    wc.hIcon-value := LoadIcon(null-handle(<HINSTANCE>), $IDI-APPLICATION);
    wc.hCursor-value := LoadCursor(null-handle(<HINSTANCE>), $IDC-ARROW);
    wc.hbrBackground-value := as(<HBRUSH>, $COLOR-WINDOW + 1);
    wc.lpszMenuName-value := null-pointer(<LPSTR>);
    wc.lpszClassName-value := "SystemTimeMonitor";
    when(zero?(RegisterClass(wc)))
      error("RegisterClass of SystemTimeMonitor failed.");
    end when;
  end with-stack-structure;

  let hwnd :: <hwnd> = CreateWindow("SystemTimeMonitor",
                          "SystemTimeMonitor",
                          $WS-OVERLAPPEDWINDOW,
                          0, 0, 0, 0,
                          $NULL-HWND,
                          null-handle(<HMENU>),
                          hinstance,
                          $NULL-VOID);
  when(null-handle?(hwnd))
    error("Could not create SystemTimeMonitor window.");
  end when;

  ShowWindow(hwnd, $SW-HIDE);

  with-stack-structure(msg :: <PMSG>)
    while(GetMessage(msg, $NULL-HWND, 0, 0))
      TranslateMessage(msg);
      DispatchMessage(msg);
    end while;
  end with-stack-structure;
end method system-time-monitor-function;

define callback WndProc :: <WNDPROC> = system-time-monitor-window-function;

define method system-time-monitor-window-function(hwnd :: <hwnd>,
                                                  message :: <integer>,
                                                  uParam, lParam)
 => (value :: <integer>)
  handle-message(hwnd, message, uParam, lParam);
end method system-time-monitor-window-function;

define method handle-message(hwnd, message, uParam, lParam)
 => (value :: <integer>)
  DefWindowProc(hwnd, message, uParam, lParam);
end method handle-message;

define method handle-message(hwnd, message == $WM-DESTROY, uParam, lParam)
 => (value :: <integer>)
  PostQuitMessage(0);
  0;
end method handle-message;

define method handle-message(hwnd, message == $WM-TIMECHANGE, uParam, lParam)
 => (value :: <integer>)
  debug-message("$WM-TIMECHANGE received");
  with-lock($time-changed-lock)
    release-all($system-time-changed);
  end with-lock;
  0;
end method handle-message;

begin
  // Library initialization starts here ...
end;

