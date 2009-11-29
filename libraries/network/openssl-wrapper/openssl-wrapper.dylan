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

define method bio-connect (host, #key port)
  let bio = BIO-new-connect(concatenate(host, ":", integer-to-string(port)));
  if (null-pointer?(bio))
    format-out("bio is null!\n");
  elseif (BIO-do-connect(bio) <= 0)
    format-out("bio-do-connect failed!\n");
  end;
  bio;
end;

define method dylan-ssl-connect (host, #key port)
  let ctx = SSL-context-new(SSLv23-client-method());
  let conn = make(<tcp-socket>, host: host, port: port);
  let ssl = SSL-new(ctx);
  SSL-set-fd(ssl, conn.socket-descriptor);
  SSL-connect(ssl); //does the handshake
//  let bio = BIO-new-ssl-connect(ctx);
////  with-stack-structure (ssl :: <SSL>)
////    let res = BIO-get-ssl(bio, ssl);
////    SSL-set-mode(ssl, $SSL-MODE-AUTO-RETRY);
////  end;
//  BIO-set-connection-hostname(bio, concatenate(host, ":", integer-to-string(port)));
//  if (BIO-do-connect(bio) <= 0)
//    format-out("bio-do-connect failed!\n");
//  end;
  ssl;
end;  

define function read-bytes (bio :: <ssl*>) //bio :: <basic-input-output*>)
  let buf = make(<C-string>, size: 2000);
  let c = SSL-read(bio, pointer-cast(<C-void*>, buf), 2000);
  if (c > 0)
    as(<byte-string>, copy-sequence(buf, end: c))
  else
    "something went wrong"
  end
end;

define function write-bytes (bio :: <ssl*> /* <basic-input-output*> */, data)
  SSL-write(bio, pointer-cast(<C-void*>, as(<C-string>, data)), data.size);
end;

define constant $nullp = null-pointer(<C-void*>);

define function read-pem (filename :: <string>) => (result :: <x509>)
  //if filename exists and is readable!
  let x = X509-new();
  if (null-pointer?(x))
    //error in allocation, this is bad.
  else
    PEM-read-X509(filename, C-pointer-at(<x509**>, x), $nullp, $nullp);
  end;
end;

define method ssl-listen (cert, key, #key ca)
  let ctx = SSL-context-new(SSLv23-server-method());
  SSL-context-use-certificate-file(ctx, cert, $SSL-FILETYPE-PEM);
  SSL-context-use-private-key-file(ctx, key, $SSL-FILETYPE-PEM);
  if (ca)
    let cas = instance?(ca, <string>) & list(ca) | ca;
    map(curry(SSL-context-add-extra-chain-certificate, ctx),
	map(read-pem, cas));
  end;
  let s = make(<TCP-server-socket>, port: 1234);
  values(ctx, s);
//  let bio = BIO-new-ssl(ctx, 0);
//  let abio = BIO-new-accept("1234");
//  BIO-set-accept-bios(abio, bio);
//  BIO-do-accept(abio);
//  abio;
end;

define function main(name, arguments)
  format-out("Hello, world!\n");
  init-ssl();
  //let ssl = dylan-ssl-connect("www.opendylan.org", port: 443);
  //write-bytes(ssl, "GET /\n\n");
  //while (#t)
  //  let buf = read-bytes(ssl);
  //  format-out("read %s", buf);
  //end;
  let (ctx, s) = ssl-listen("/Users/hannes/www.opendylan.org.cert.pem", "/Users/hannes/www.opendylan.org.key.pem", ca: #("/Users/hannes/cacert.class3.crt", "/Users/hannes/root.crt"));
  while(#t)
    let cs = accept(s);
    let ssl = SSL-new(ctx);
    SSL-set-fd(ssl, cs.socket-descriptor);
    SSL-accept(ssl);
    while (#t)
      let in = read-bytes(ssl);
      format-out("read %=\n", in);
      write-bytes(ssl, in);
      format-out("wrote in\n");
    end;
  end;
//  force-output(*standard-output*);
//  exit-application(0);
end function main;



// Invoke our main() function.
main(application-name(), application-arguments());
