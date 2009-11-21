library: openssl-wrapper
executable: openssl-wrapper
files: openssl-wrapper-exports
  c-wrapper
  openssl-wrapper
c-libraries: -lssl -lcrypto
c-source-files: support.c
