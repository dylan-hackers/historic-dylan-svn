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

define method ssl-connect (host, #key port)
  let ctx = SSL-context-new(SSLv23-client-method());
  let bio = BIO-new-ssl-connect(ctx);
//  with-stack-structure (ssl :: <SSL>)
//    let res = BIO-get-ssl(bio, ssl);
//    SSL-set-mode(ssl, $SSL-MODE-AUTO-RETRY);
//  end;
  BIO-set-connection-hostname(bio, concatenate(host, ":", integer-to-string(port)));
  if (BIO-do-connect(bio) <= 0)
    format-out("bio-do-connect failed!\n");
  end;
  bio;
end;  

define function read-bytes (bio :: <basic-input-output*>)
  let buf = make(<C-string>, size: 2000);
  let c = BIO-read(bio, pointer-cast(<C-void*>, buf), 2000);
  if (c > 0)
    as(<byte-string>, copy-sequence(buf, end: c))
  else
    "something went wrong"
  end
end;

define function write-bytes (bio :: <basic-input-output*>, data)
  BIO-write(bio, pointer-cast(<C-void*>, as(<C-string>, data)), data.size);
end;

define method ssl-listen (cert, key)
  let ctx = SSL-context-new(SSLv23-server-method());
  SSL-context-use-certificate-file(ctx, cert, $SSL-FILETYPE-PEM);
  SSL-context-use-private-key-file(ctx, key, $SSL-FILETYPE-PEM);
  let bio = BIO-new-ssl(ctx, 0);
  let abio = BIO-new-accept("1234");
  BIO-set-accept-bios(abio, bio);
  BIO-do-accept(abio);
  abio;
end;

define function main(name, arguments)
  format-out("Hello, world!\n");
  init-ssl();
  //let bio = ssl-connect("www.opendylan.org", port: 443);
  let bio = ssl-listen("/Users/hannes/cert.pem", "/Users/hannes/key.pem");
  while(#t)
    BIO-do-accept(bio);
    let out = BIO-pop(bio);
    BIO-do-accept(out);
    let in = read-bytes(out);
    format-out("read %=\n", in);
    write-bytes(out, in);
    format-out("wrote in\n");
  end;
//  force-output(*standard-output*);
//  exit-application(0);
end function main;



// Invoke our main() function.
main(application-name(), application-arguments());
