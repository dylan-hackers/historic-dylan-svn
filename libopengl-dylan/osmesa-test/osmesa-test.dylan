module: osmesa-test
synopsis: Test program for OSMesa interface
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jeff Dubrule.  See COPYING for license details.

define function main(name, arguments)

  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
