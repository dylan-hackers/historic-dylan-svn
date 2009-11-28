#include <openssl/bio.h>
#include <openssl/ssl.h>
#include <openssl/pem.h>
#include <openssl/x509.h>

long my_BIO_do_connect (BIO* b) {
  return BIO_do_handshake(b);
}

long my_BIO_set_conn_hostname (BIO* b, char* name) {
  return BIO_set_conn_hostname(b, name);
}

long my_BIO_get_ssl (BIO* b, SSL* s) {
  return BIO_get_ssl(b, s);
}

long my_BIO_set_accept_bios (BIO* b, BIO* bio) {
  return BIO_set_accept_bios(b, bio);
}

long my_BIO_do_accept (BIO* b) {
  return BIO_do_accept(b);
}

long my_SSL_set_mode (SSL* s, long op) {
  return SSL_set_mode(s, op);
}

X509* my_PEM_read_X509 (char* f, X509** x1, pem_password_cb* cb, void* u) {
  FILE* fp;
  X509* x;
  fp = fopen(f, "r");
  x = X509_new();
  return PEM_read_X509(fp, &x, cb, u);
}

long my_SSL_CTX_add_extra_chain_cert (SSL_CTX* ctx, X509* x509) {
  return SSL_CTX_add_extra_chain_cert(ctx, x509);
}
