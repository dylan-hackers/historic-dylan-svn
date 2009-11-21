module: openssl-wrapper

define C-function SSL-library-init
  result success :: <C-int>;
  c-name: "SSL_library_init"
end;

define C-function SSL-load-error-strings
  result res :: <C-void*>;
  c-name: "SSL_load_error_strings"
end;

define C-function ERR-load-BIO-strings
  result res :: <C-void*>;
  c-name: "ERR_load_BIO_strings"
end;

define C-function RAND-load-file
  input parameter filename :: <C-string>;
  input parameter maximal-bytes :: <C-int>;
  result read-bytes :: <C-int>;
  c-name: "RAND_load_file"
end;

define C-function BIO-new-connect
  input parameter host-and-port :: <C-string>;
  result bio :: <basic-input-output*>;
  c-name: "BIO_new_connect"
end;

define C-struct <STACK>
  slot number :: <C-int>;
  slot data :: <C-string*>;
  slot sorted :: <C-int>;
  slot number-alloc :: <C-int>;
  slot whatever :: <C-int>;
  pointer-type-name: <STACK*>;
end;

define C-struct <crypto-ex-data>
  slot sk :: <STACK>;
  slot dummy :: <C-int>;
  pointer-type-name: <crypto-ex-data*>;
end;

define C-struct <basic-input-output>
  slot bio-method :: <C-void*>; //actually BIO_METHOD*
  slot callback :: <C-void*>; //bio_st*, int, const char*, int, long, long => long
  slot callback-argument :: <C-string>;
  slot init :: <C-int>;
  slot shutdown :: <C-int>;
  slot flags :: <C-int>;
  slot retry-reason :: <C-int>;
  slot num :: <C-int>;
  slot ptr :: <C-void*>;
  slot next-bio :: <basic-input-output*>;
  slot previous-bio :: <basic-input-output*>;
  slot references :: <C-int>;
  slot number-read :: <C-unsigned-long>;
  slot number-write :: <C-unsigned-long>;
  slot ex-data :: <crypto-ex-data>;
  pointer-type-name: <basic-input-output*>;
end;

define C-function BIO-read
  input parameter bio :: <basic-input-output*>;
  parameter data :: <C-void*>;
  input parameter length :: <C-int>;
  result read-bytes :: <C-int>;
  c-name: "BIO_read"
end;

define C-function BIO-write
  input parameter bio :: <basic-input-output*>;
  input parameter data :: <C-string>;
  input parameter length :: <C-int>;
  result written-bytes :: <C-int>;
  c-name: "BIO_write"
end;

define C-function my-BIO-do-connect
  input parameter bio :: <basic-input-output*>;
  result res :: <C-long>;
  c-name: "my_BIO_do_connect"
end;