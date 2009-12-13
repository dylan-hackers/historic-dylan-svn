module: openssl-wrapper
synopsis: 
author: 
copyright: 

define function init-ssl ()
  SSL-library-init();
  SSL-load-error-strings();
  ERR-load-BIO-strings();
  //OpenSSL-add-all-algorithms();
  RAND-load-file("/dev/urandom", 2048);
end;

define constant $nullp = null-pointer(<C-void*>);

define function read-pem (filename :: <string>) => (result :: <x509>)
  //if filename exists and is readable!
  let x = X509-new(); //need to manually free the X509 struct?
  if (null-pointer?(x))
    //error in allocation, this is bad.
  else
    PEM-read-X509(filename, C-pointer-at(<x509**>, x), $nullp, $nullp);
  end;
end;

define abstract class <ssl-socket> (<platform-socket>)
  slot underlying-socket :: <platform-socket>, init-keyword: lower:;
  slot ssl-context :: <SSL-CTX>;
end;

define class <ssl-client-socket> (<ssl-socket>)
end;

define method make (class == <ssl-socket>, #rest initargs, #key)
 => (sock :: <ssl-socket>)
  format-out("instantiating ssl socket\n");
  apply(make, <ssl-client-socket>, initargs)
end;

//define method stream-direction (s :: <ssl-socket>) => (direction :: <symbol>)
//  s.underlying-socket.stream-direction
//end;

define method initialize (sock :: <ssl-socket>, #rest rest, #key lower, requested-buffer-size, #all-keys)
 => ()
  //next-method();
  if (lower)
    format-out("initializing ssl socket\n");
    //already setup a connection! do SSL handshake over this connection
    sock.ssl-context := SSL-context-new(SSLv23-client-method());
    let ssl = SSL-new(sock.ssl-context);
    sock.accessor := new-accessor(type-for-socket(sock), descriptor: ssl);
    format-out("set ssl socket accessor\n");
    let direction = sock.stream-direction;
    sock.stream-direction := direction;
    let size-for-buffers :: <integer> =
      if (requested-buffer-size) 
	requested-buffer-size
      else
	accessor-preferred-buffer-size(sock.accessor)
      end if;
    if ((direction == #"input") | (direction == #"input-output"))
      stream-input-buffer(sock) := make(<buffer>, size: size-for-buffers) 
    end;
    if ((direction == #"output") | (direction == #"input-output"))
      stream-output-buffer(sock) := make(<buffer>, size: size-for-buffers)
    end;
    SSL-set-fd(ssl, lower.accessor.socket-descriptor);
    format-out("finished initializing %=, acc is %=\n", sock, sock.accessor);
  else //need to connect manually!
    format-out("FOO\n");
    next-method();
  end;
end;

define class <ssl-server-socket> (<server-socket>)
  constant slot underlying-socket :: <server-socket>, init-keyword: lower:;
  slot ssl-context :: <SSL-CTX>;
end;

define class <unix-ssl-socket-accessor> (<unix-socket-accessor>)
end;

define method initialize (s :: <ssl-server-socket>, #rest rest,
			  #key cert, key, ca, #all-keys)
 => ()
  next-method();
  s.ssl-context := SSL-context-new(SSLv23-server-method());
  SSL-context-use-certificate-file(s.ssl-context, cert, $SSL-FILETYPE-PEM);
  SSL-context-use-private-key-file(s.ssl-context, key, $SSL-FILETYPE-PEM);
  if (ca)
    let cas = instance?(ca, <string>) & list(ca) | ca;
    map(curry(SSL-context-add-extra-chain-certificate, s.ssl-context),
        map(read-pem, cas));
  end;
  s.socket-descriptor := s.underlying-socket.socket-descriptor;
end;

define method close
    (the-socket :: <ssl-server-socket>,
     #rest keys, 
     #key abort? = #f, wait? = #t, synchronize? = #f,
     already-unregistered? = #f) => ()
  SSL-context-free(the-socket.ssl-context);
  close(the-socket.underlying-socket);
  the-socket.socket-descriptor := #f;
end;

define method accessor-accept (s :: <ssl-server-socket>)
 => (connect-socket-descriptor)
  let cs = accessor-accept(s.underlying-socket);
  let ssl = SSL-new(s.ssl-context);
  SSL-set-fd(ssl, cs);
  SSL-accept(ssl);
  ssl;
end;

define method accessor-read-into!
    (accessor :: <unix-ssl-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer)
 => (nread :: <integer>)
  let the-buffer = buffer | stream-input-buffer(stream);
  format-out("want to read from %= (buf %=)\n", accessor.socket-descriptor, the-buffer);
  let nread =
    interruptible-system-call
      (SSL-read(accessor.socket-descriptor, buffer-offset(the-buffer, offset), count));
  format-out("read %d!\n", nread);
  nread;
end;

define method accessor-write-from
    (accessor :: <unix-ssl-socket-accessor>, stream :: <platform-socket>,
     offset :: <buffer-index>, count :: <buffer-index>, #key buffer,
     return-fresh-buffer?) => (nwritten :: <integer>, new-buffer :: <buffer>)
  let buffer = buffer | stream-output-buffer(stream);
  format-out("have buffer, be in write!\n");
  let nwritten = interruptible-system-call
    (SSL-write(accessor.socket-descriptor, buffer-offset(buffer, offset), count));
  format-out("wrote %d byte\n", nwritten);
  values(nwritten, buffer)
end;

define method accessor-close
    (accessor :: <unix-ssl-socket-accessor>, #key abort?, wait?) 
 => (closed? :: <boolean>)
  format-out("called close on ssl socket!\n");
  let ssl* = accessor.socket-descriptor;
  SSL-shutdown(ssl*); //if retval == 0 and we want to be sure, call it again (wait for shutdown from other side)
  let real-fd = SSL-get-fd(ssl*);
  SSL-free(ssl*);
  accessor-close-socket(real-fd);
  accessor.socket-descriptor := #f;
end;

define method accessor-open
    (accessor :: <unix-ssl-socket-accessor>, locator, #rest rest,
     #key remote-host, remote-port, descriptor, no-delay?, direction, if-exists, if-does-not-exist,
     #all-keys) => ()
  format-out("called accessor-open with %=\n", rest);
  if (descriptor)
    accessor.socket-descriptor := descriptor;
  end;
  //else //client socket, already connected!
    let ret = SSL-connect(accessor.socket-descriptor); //does the handshake
    format-out("return value of connect was %=\n", ret);
  //end
end;

define method client-class-for-server (server :: <ssl-server-socket>) => (res == <ssl-client-socket>)
  format-out("ccfs called\n");
  <ssl-client-socket>
end;

define method type-for-socket (s :: <ssl-client-socket>) => (res == #"SSL")
  format-out("TFS is SSL!\n");
  #"SSL"
end;

define sideways method platform-accessor-class
    (type == #"SSL", locator)
 => (class == <unix-ssl-socket-accessor>)
  ignore(locator);
  <unix-ssl-socket-accessor>
end method platform-accessor-class;

define function main(name, arguments)
  format-out("Hello, world!\n");
  start-sockets();
  init-ssl();
  format-out("initialized sockets and ssl\n");
/*  let cs = make(<TCP-socket>, host: "localhost", port: 1234);
  format-out("instantiated tcp socket\n");
  let scs = make(<ssl-socket>, lower: cs);
  format-out("instantiated ssl socket!\n");
  format(scs, "GET /\n\n");
  format-out("wrote to socket!\n");
  while (~ stream-at-end?(scs))
    let buf = read-line(scs);
    format-out("%s", buf);
  end;
  close(scs);
*/
  let s = make(<TCP-server-socket>, port: 1234);
  let ss = make(<ssl-server-socket>, lower: s, cert: "/Users/hannes/www.opendylan.org.cert.pem", key: "/Users/hannes/www.opendylan.org.key.pem", ca: #("/Users/hannes/cacert.class3.crt", "/Users/hannes/root.crt"));
  for (i from 0 below 2)
    let cs = accept(ss, element-type: <byte-character>);
    format(cs, "hello\n");
    let in = read-line(cs);
    format-out("read %=\n", in);
    format(cs, "bye %s\n", in);
    close(cs);
  end;
  close(ss);
end function main;



// Invoke our main() function.
main(application-name(), application-arguments());
