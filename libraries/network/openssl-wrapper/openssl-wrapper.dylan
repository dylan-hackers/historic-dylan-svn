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

//define function init-ssl-context ()
//  let ctx = SSL-CTX-new(TLSv1-method());
//  //SSL-CTX-set-options(ctx, SSL-OP-NO-SESSIO
//  SSL-CTX-set-mode(ctx, $SSL-MODE-AUTO-RETRY);
//end;

define method ssl-connect (host, #key port)
  let bio = BIO-new-connect(concatenate(host, ":", integer-to-string(port)));
  if (null-pointer?(bio))
    format-out("bio is null!\n");
  elseif (my-BIO-do-connect(bio) <= 0)
    format-out("bio-do-connect failed!\n");
  end;
  bio;
end;

define function main(name, arguments)
  format-out("Hello, world!\n");
  init-ssl();
  let bio = ssl-connect("127.0.0.1", port: 8000);
  BIO-write(bio, "foo", 3);
  format-out("wrote foo");
//  force-output(*standard-output*);
//  exit-application(0);
end function main;



// Invoke our main() function.
main(application-name(), application-arguments());
