module: dylan-user
copyright: 1996 The Harlequin Group Limited. All rights reserved.

define library operating-system
  use harlequin-dylan;
  export operating-system;
end library operating-system;

define module operating-system
  use harlequin-dylan;
  use dylan-direct-c-ffi;
  use harlequin-extensions,
    import: { application-name,
              application-filename,
              application-arguments,
              tokenize-command-string,
              exit-application,
              register-application-exit-function },
    export: all;
  export
    $architecture-little-endian?,
    $os-name, $os-variant, $os-version,
    $platform-name, $machine-name,
    login-name, login-group, owner-name, owner-organization,
    environment-variable, environment-variable-setter,
    tokenize-environment-variable,
    run-application,
    create-application-event,
    wait-for-application-event,
    signal-application-event;
end module operating-system;

// eof
