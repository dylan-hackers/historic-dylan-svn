Module:    ssl-sockets
Synopsis:  SSL Sockets
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

// Various c-ffi definitions for the SSL routines. Note
// that I dynamically load the DLL rather than statically
// link to the .lib file. I define the C-FUNCTION and then
// create a Dylan function that calls that c-ffi function passing
// the correct dynamically loaded procedure address.
define C-function %SSL-load-error-strings
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-load-error-strings () => ()
  %SSL-load-error-strings(*ssl-load-error-strings*);
end function ssl-load-error-strings;

define C-function %SSL-Library-Init
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-library-init () => (result :: <integer>)
  %SSL-library-init(*ssl-library-init*);
end function ssl-library-init;

define C-function %SSLv23-client-method
  result value :: <LPVOID>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function sslv23-client-method () => (result)
  %SSLv23-client-method(*sslv23-client-method*);
end function sslv23-client-method;

define C-function %SSL-ctx-new
  parameter meth :: <LPVOID>;
  result value :: <LPVOID>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-ctx-new (meth) => (result)
  %SSL-ctx-new(*ssl-ctx-new*, meth);
end function ssl-ctx-new;

define C-function %SSL-new
  parameter ctx :: <LPVOID>;
  result value :: <LPVOID>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-new (ctx) => (result)
  %SSL-new(*ssl-new*, ctx);
end function ssl-new;

define C-function %ssl-set-fd
  parameter s :: <LPVOID>;
  parameter fd :: <c-raw-int>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-set-fd (s, fd) => (result)
  %SSL-set-fd(*ssl-set-fd*, s, fd);
end function ssl-set-fd;

define C-function %ssl-connect
  parameter ssl :: <LPVOID>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-connect (ssl) => (result)
  %SSL-connect(*ssl-connect*, ssl);
end function ssl-connect;

define C-function %ssl-write
  parameter ssl :: <LPVOID>;
  parameter buf :: <C-buffer-offset>;
  parameter num :: <c-int>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-write (ssl, buf, num) => (result)
  %SSL-write(*ssl-write*, ssl, buf, num);
end function ssl-write;

define C-function %ssl-read
  parameter ssl :: <LPVOID>;
  parameter buf :: <C-buffer-offset>;
  parameter num :: <c-int>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-read (ssl, buf, num) => (result)
  %SSL-read(*ssl-read*, ssl, buf, num);
end function ssl-read;

define C-function %ssl-shutdown
  parameter ssl :: <LPVOID>;
  result value :: <c-int>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-shutdown (ssl) => (result)
  %SSL-shutdown(*ssl-shutdown*, ssl);
end function ssl-shutdown;

define C-function %ssl-free
  parameter ssl :: <LPVOID>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-free (ssl) => (result)
  %SSL-free(*ssl-free*, ssl);
end function ssl-free;

define C-function %ssl-ctx-free
  parameter ctx :: <LPVOID>;
  indirect: #t;
  c-modifiers: "__stdcall";
end;

define function ssl-ctx-free (ssl) => (result)
  %SSL-ctx-free(*ssl-ctx-free*, ssl);
end function ssl-ctx-free;

// Define variables to hold the procedure addresses.
define variable *ssl-module* = #f;
define variable *ssl-library-init* = #f;
define variable *ssl-load-error-strings* = #f;
define variable *sslv23-client-method* = #f;
define variable *ssl-ctx-new* = #f;
define variable *ssl-new* = #f;
define variable *ssl-set-fd* = #f;
define variable *ssl-connect* = #f;
define variable *ssl-write* = #f;
define variable *ssl-read* = #f;
define variable *ssl-shutdown* = #f;
define variable *ssl-free* = #f;
define variable *ssl-ctx-free* = #f;
define variable *ssl-get-error* = #f;

// Initialize the ssl library by dynamically loading
// the SSL DLL and finding the functions in the library
// and storing the procedure addresses.
define function start-ssl-sockets () => (result :: <boolean>)
  unless(*ssl-module*)
    *ssl-module* := LoadLibrary("ssleay32.dll");
    unless(*ssl-module* = null-pointer(<HMODULE>))
      *ssl-library-init* := GetProcAddress(*ssl-module*, "SSL_library_init");
      *ssl-load-error-strings* := GetProcAddress(*ssl-module*, "SSL_load_error_strings");
      *sslv23-client-method* := GetProcAddress(*ssl-module*, "SSLv23_client_method");
      *ssl-ctx-new* := GetProcAddress(*ssl-module*, "SSL_CTX_new");
      *ssl-new* := GetProcAddress(*ssl-module*, "SSL_new");
      *ssl-set-fd* := GetProcAddress(*ssl-module*, "SSL_set_fd");
      *ssl-connect* := GetProcAddress(*ssl-module*, "SSL_connect");
      *ssl-write* := GetProcAddress(*ssl-module*, "SSL_write");
      *ssl-read* := GetProcAddress(*ssl-module*, "SSL_read");
      *ssl-shutdown* := GetProcAddress(*ssl-module*, "SSL_shutdown");
      *ssl-free* := GetProcAddress(*ssl-module*, "SSL_free");
      *ssl-ctx-free* := GetProcAddress(*ssl-module*, "SSL_CTX_free");
      *ssl-get-error* := GetProcAddress(*ssl-module*, "SSL_get_error");
      SSL-Library-Init();
      ssl-load-error-strings();  
    end unless;
  end unless;
  *ssl-module* ~= null-pointer(<HMODULE>);
end function start-ssl-sockets;

// Socket class for SSL TCP sockets.
define class <ssl-tcp-socket> (<tcp-socket>)
end class <ssl-tcp-socket>;

define method make (class == <ssl-TCP-socket>, #rest initargs,                    
		    #key element-type = <byte-character>,
		    direction: requested-direction = #"input-output")
 => (stream :: <ssl-TCP-socket>)
  apply(make, client-class-for-element-type(class, element-type),
        direction: requested-direction,
        initargs)
end method make;

define sideways method client-class-for-protocol (protocol == #"SSL-TCP")
  => (class == <ssl-TCP-socket>)
  <ssl-TCP-socket>
end method;

define method type-for-socket (socket :: <ssl-tcp-socket>)
 => (type == #"SSL-TCP")
  #"SSL-TCP"
end method;

define class <general-ssl-TCP-socket>
    (<ssl-TCP-socket>,
     <general-typed-stream>)
  inherited slot stream-element-type = <character>;
end class <general-ssl-TCP-socket>;

define class <byte-char-ssl-TCP-socket>
    (<ssl-TCP-socket>,
     <byte-char-element-stream>)
  inherited slot stream-element-type = <byte-character>;
end class <byte-char-ssl-TCP-socket>;

define class <byte-ssl-TCP-socket>
    (<ssl-TCP-socket>,
     <byte-element-stream>)
  inherited slot stream-element-type = <byte>;
end class <byte-ssl-TCP-socket>;

define method client-class-for-element-type 
    (class == <ssl-tcp-socket>, element-type :: <type>)
  => (class == <general-ssl-tcp-socket>)
  <general-ssl-tcp-socket>
end method;

define method client-class-for-element-type
    (class == <ssl-TCP-socket>, element-type == <byte>) => (class == <byte-ssl-TCP-socket>)
  <byte-ssl-TCP-socket>
end method;

define method client-class-for-element-type
    (class == <ssl-TCP-socket>, element-type == <byte-character>) => (class == <byte-char-ssl-TCP-socket>)
  <byte-char-ssl-TCP-socket>
end method;

define class <ssl-tcp-socket-accessor> (<win32-tcp-accessor>)
    slot ssl-socket-ctx = #f;
    slot ssl-socket-handle = #f;
end class <ssl-tcp-socket-accessor>;

define sideways method platform-accessor-class
    (type == #"SSL-TCP", locator)
 => (class)
  ignore(locator);
  <ssl-tcp-socket-accessor>;
end method platform-accessor-class;

define method accessor-open
    (accessor :: <ssl-tcp-socket-accessor>, #key      direction, if-exists, if-does-not-exist,
 #all-keys) => ()
  next-method();

  let ssl-method = sslv23-client-method();

  let ssl-ctx = ssl-ctx-new(ssl-method);
  let ssl-handle = ssl-new(ssl-ctx);
  ssl-set-fd(ssl-handle, accessor.socket-descriptor);
  ssl-connect(ssl-handle);
  accessor.ssl-socket-ctx := ssl-ctx;
  accessor.ssl-socket-handle := ssl-handle;    
end method accessor-open;

define method accessor-close
    (accessor :: <ssl-tcp-socket-accessor>,
     #key abort? = #f, wait? = #t)
 => (closed? :: <boolean>)
  when(accessor.ssl-socket-handle)
    ssl-shutdown(accessor.ssl-socket-handle);
  end when;
  next-method();
  when(accessor.ssl-socket-handle)
    ssl-free(accessor.ssl-socket-handle);
    ssl-ctx-free(accessor.ssl-socket-ctx);
    accessor.ssl-socket-handle := #f;
    accessor.ssl-socket-ctx := #f;
  end when; 
end method accessor-close;

define method raw-accessor-write(accessor :: <ssl-tcp-socket-accessor>,
    descriptor, 
    buffer, count) => (nwritten)
  ssl-write(accessor.ssl-socket-handle, buffer, count);
end method raw-accessor-write;

define method raw-accessor-read
    (accessor :: <ssl-tcp-socket-accessor>, descriptor, buffer, count )
 => (nread :: <integer>)
   ssl-read(accessor.ssl-socket-handle, buffer, count);
end method raw-accessor-read;


