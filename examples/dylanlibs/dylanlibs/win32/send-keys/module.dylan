Module:    dylan-user
Synopsis:  Library to send keystrokes to the keyboard buffer
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module send-keys-internal
  use functional-dylan;
  use threads, export: { sleep };
  use c-ffi;
  use win32-common;
  use win32-user, exclude: { 
    dy-value,
    dy-value-setter,
    dx-value,
    dx-value-setter,
    dwExtraInfo-value,
    dwExtraInfo-value-setter
    };
  use win32-kernel, exclude: { sleep };

  // Add binding exports here.
  export <key-entry>,
    key-entry-keysym,
    key-entry-state,
    send-keys,
    get-key-entries,
    char->keysym,
    get-console-text,
    wait-for-console-text,
    <KEYBDINPUT>,
    wVk-value,
    wVk-value-setter,
    wScan-value,
    wScan-value-setter,
    dwTime-value,
    dwTime-value-setter,
    dx-value,
    dx-value-setter,
    dy-value,
    dy-value-setter,
    mouseData-value,
    mouseData-value-setter,
    mi-value,
    mi-value-setter,
    ki-value,
    ki-value-setter,
    type-value,
    type-value-setter,
    is-value,
    is-value-setter,
    dwExtraInfo-value,
    create-up-down-key-press,
    $INPUT-MOUSE,
    $INPUT-HARDWARE;
end module send-keys-internal;

define module send-keys
  use send-keys-internal, export: {
    <key-entry>,
    key-entry-keysym,
    key-entry-state,
    send-keys,
    get-key-entries,
    create-up-down-key-press,
    char->keysym,
    wait-for-console-text,
    get-console-text };
end module send-keys;


