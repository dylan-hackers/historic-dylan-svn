Module:    win32-wininet
Synopsis:  C-FFI wrappers for the WinInet libraries.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define constant <HINTERNET> = <c-void*>;
define c-pointer-type <lpcstr*> => <lpcstr>;

define constant $internet-open-type-preconfig = 0;
define constant $internet-open-type-direct = 1;
define constant $internet-open-type-proxy = 3;
define constant $internet-open-type-preconfig-with-no-autoproxy = 4;

define constant $internet-flag-async = #x10000000;
define constant $internet-flag-passive = #x08000000;
define constant $internet-flag-no-cache-write = #x04000000;
define constant $internet-flag-dont-cache = $internet-flag-no-cache-write;
define constant $internet-flag-make-persistent = #x02000000;
define constant $internet-flag-keep-connection = #x00400000;
define constant $internet-flag-no-auto-redirect = #x00200000;
define constant $internet-flag-read-prefetch =  #x00100000;
define constant $internet-flag-secure =  #x00800000;
define constant $internet-flag-pragma-nocache =  #x00000100;

define constant $internet-service-url = 0;
define constant $internet-service-ftp = 1;
define constant $internet-service-gopher = 2;
define constant $internet-service-http = 3;


define C-function InternetOpen
  parameter lpszAgent :: <LPCSTR>;
  parameter dwAccessType :: <DWORD>;
  parameter lpszProxyName :: <LPCSTR>;
  parameter lpszProxyBypass :: <LPCSTR>;
  parameter dwFlags :: <DWORD>;
  result value :: <HINTERNET>;
  c-name: "InternetOpenA", c-modifiers: "__stdcall";
end;

define C-function InternetCloseHandle
  parameter hInternet :: <HINTERNET>;
  result value :: <BOOL>;
  c-name: "InternetCloseHandle", c-modifiers: "__stdcall";
end;

define C-function InternetConnect
  parameter hInternet :: <HINTERNET>;
  parameter lpszServerName :: <LPCSTR>;
  parameter nServerPort :: <WORD>;
  parameter lpszUserName :: <LPCSTR>;
  parameter lpszPassword :: <LPCSTR>;
  parameter dwService :: <DWORD>;
  parameter dwFlags :: <DWORD>;
  parameter dwContext :: <LPDWORD>;
  result value :: <HINTERNET>;
  c-name: "InternetConnectA", c-modifiers: "__stdcall";
end;

define C-function HttpOpenRequest
  parameter hInternet :: <HINTERNET>;
  parameter lpszVerb :: <LPCSTR>;
  parameter lpszObjectName :: <LPCSTR>;
  parameter lpszVersion :: <LPCSTR>;
  parameter lpszReferrer :: <LPCSTR>;
  parameter lpszAcceptTypes :: <LPCSTR*>;
  parameter dwFlags :: <DWORD>;
  parameter dwContext :: <LPDWORD>;
  result value :: <HINTERNET>;
  c-name: "HttpOpenRequestA", c-modifiers: "__stdcall";
end;

define C-function HttpSendRequest
  parameter hInternet :: <HINTERNET>;
  parameter lpszheaders :: <LPCSTR>;
  parameter dwHeadersLength :: <DWORD>;
  parameter lpOptional :: <lpvoid>;
  parameter dwOptionalLength :: <DWORD>;
  result value :: <BOOL>;
  c-name: "HttpSendRequestA", c-modifiers: "__stdcall";
end;

define C-function InternetQueryDataAvailable
  parameter hFile :: <HINTERNET>;
  output parameter lpdwNumberOfBytesAvailable :: <LPDWORD>;
  parameter dwFlags :: <DWORD>;
  parameter dwContext :: <DWORD>;
  result value :: <BOOL>;
  c-name: "InternetQueryDataAvailable", c-modifiers: "__stdcall";
end;

define C-function InternetReadFile
  parameter hFile :: <HINTERNET>;
  parameter lpBuffer :: <LPVOID>;
  parameter dwNumberOfBytesToRead :: <DWORD>;
  output parameter lpdwNumberOfBytesRead :: <LPDWORD>;
  result value :: <BOOL>;
  c-name: "InternetReadFile", c-modifiers: "__stdcall";
end;

