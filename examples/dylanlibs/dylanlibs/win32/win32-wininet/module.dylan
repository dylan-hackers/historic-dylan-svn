Module:    dylan-user
Synopsis:  C-FFI wrappers for the WinInet libraries.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module win32-wininet
  use functional-dylan;
  use c-ffi;
  use win32-common;

  export
    <hinternet>,
    $internet-open-type-preconfig,
    $internet-open-type-direct,
    $internet-open-type-proxy,
    $internet-open-type-preconfig-with-no-autoproxy,
    $internet-flag-async,
    $internet-flag-passive,
    $internet-flag-no-cache-write,
    $internet-flag-dont-cache,
    $internet-flag-make-persistent,
    $internet-flag-keep-connection,
    $internet-flag-no-auto-redirect,
    $internet-flag-read-prefetch,
    $internet-flag-secure,
    $internet-flag-pragma-nocache,
    $internet-service-url,
    $internet-service-ftp,
    $internet-service-gopher,
    $internet-service-http,
    InternetOpen,
    InternetCloseHandle,
    InternetConnect,
    HttpOpenRequest,
    HttpSendRequest,
    InternetQueryDataAvailable,
    InternetReadFile;
end module win32-wininet;
