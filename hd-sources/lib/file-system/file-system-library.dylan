Module:    dylan-user
Author:    Jonathan Bachrach, Gary Palter
Synopsis:  The File System library API
Copyright: 1996 The Harlequin Group Limited.  All rights reserved.

define library file-system
  use harlequin-dylan;
  use locators;
  use operating-system;
  use date;
  export file-system;
end library;

///---*** Should I export any of the types?
define module file-system
  use harlequin-dylan;
  use dylan-direct-c-ffi;
  use locators;
  use operating-system;
  use date;
  export
    <pathname>,
    <file-type>,
    <copy/rename-disposition>,
    <file-system-error>,
    $pathname-separator,
    $pathname-separator-as-string,
    shorten-pathname,
    file-exists?,
    file-type,
    delete-file,
    copy-file,
    rename-file,
    file-properties,
    file-property, file-property-setter,
    do-directory,
    directory-contents,
    create-directory,
    delete-directory,
    ensure-directories-exist,
    home-directory,
    working-directory, working-directory-setter,
    temp-directory,
    root-directories;
end module;
