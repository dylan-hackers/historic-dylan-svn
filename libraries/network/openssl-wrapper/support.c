#include <openssl/bio.h>

long my_BIO_do_connect (BIO* b) {
  return BIO_do_handshake(b);
}
