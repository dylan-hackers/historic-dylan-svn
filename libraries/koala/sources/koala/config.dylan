Module:    internals
Synopsis:  For processing the configuration init file, server.xml
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Things to configure:
//   *log-types*
//   *debugging-server*
//   What dlls to load?
//   *document-root*
//   Almost all variables in variables.dylan

// Process the server config file, config.xml.
// Assume a user directory structure like:
// http-server/
// http-server/bin               // server executable and libs
// http-server/www               // default web document root
// http-server/config            // config.xml etc
define method configure-server ()
  let exe-loc = as(<file-locator>, application-filename());
  let bin-dir = locator-directory(exe-loc);
  let app-dir = parent-directory(bin-dir);

  // Default document root is <koala-root>/www (assuming that the example
  // project is running).
  // This may be changed in config.xml (eventually)
  *document-root* := subdirectory-locator(app-dir, "www");
  log-info("Document root is %s", *document-root*);

  // config.xml is in <app-dir>/../config/config.xml
  let config-loc = merge-locators(as(<file-locator>, "config/config.xml"),
                                  app-dir);
  //---TODO
end configure-server;

//---TODO: Read mime types from a file and set *mime-type-map*.  Get a more complete set of types.
// ftp://ftp.isi.edu/in-notes/iana/assignments/media-types/media-types

/*
application/mac-binhex40        hqx
application/mac-compactpro      cpt
application/msword              doc
application/octet-stream        bin dms lha lzh exe class
application/oda                 oda
application/pdf                 pdf
application/postscript          ai eps ps
application/powerpoint          ppt
application/rtf                 rtf
application/x-bcpio             bcpio
application/x-cdlink            vcd
application/x-cpio              cpio
application/x-csh               csh
application/x-director          dcr dir dxr
application/x-dvi               dvi
application/x-gtar              gtar
application/x-gzip
application/x-hdf               hdf
application/x-koan              skp skd skt skm
application/x-latex             latex
application/x-mif               mif
application/x-netcdf            nc cdf
application/x-sh                sh
application/x-shar              shar
application/x-stuffit           sit
application/x-sv4cpio           sv4cpio
application/x-sv4crc            sv4crc
application/x-tar               tar
application/x-tcl               tcl
application/x-tex               tex
application/x-texinfo           texinfo texi
application/x-troff             t tr roff
application/x-troff-man         man
application/x-troff-me          me
application/x-troff-ms          ms
application/x-ustar             ustar
application/x-wais-source       src
application/zip                 zip
audio/basic                     au snd
audio/midi                      mid midi kar
audio/mpeg                      mpga mp2
audio/x-aiff                    aif aiff aifc
audio/x-pn-realaudio            ram rm ra
audio/x-pn-realaudio-plugin     rpm
audio/x-realaudio               ra
audio/x-wav                     wav
chemical/x-pdb                  pdb xyz
image/gif                       gif
image/ief                       ief
image/jpeg                      jpeg jpg jpe
image/png                       png
image/tiff                      tiff tif
image/x-cmu-raster              ras
image/x-portable-anymap         pnm
image/x-portable-bitmap         pbm
image/x-portable-graymap        pgm
image/x-portable-pixmap         ppm
image/x-rgb                     rgb
image/x-xbitmap                 xbm
image/x-xpixmap                 xpm
image/x-xwindowdump             xwd
message/external-body
message/news
message/partial
message/rfc822
multipart/alternative
multipart/appledouble
multipart/digest
multipart/mixed
multipart/parallel
text/html                       html htm
text/plain                      txt
text/richtext                   rtx
text/tab-separated-values       tsv
text/x-setext                   etx
text/x-sgml                     sgml sgm
video/mpeg                      mpeg mpg mpe
video/quicktime                 qt mov
video/x-msvideo                 avi
video/x-sgi-movie               movie
x-conference/x-cooltalk         ice
x-world/x-vrml                  wrl vrml


text/plain                               [RFC2646,RFC2046]
                richtext                            [RFC2045,RFC2046]
                enriched                                    [RFC1896]
                tab-separated-values                   [Paul Lindner]
                html                                        [RFC2854]
                sgml                                        [RFC1874]
                vnd.latex-z                                   [Lubos]
                vnd.fmi.flexstor                             [Hurtta]
		uri-list				    [RFC2483]
		vnd.abc					      [Allen]
		rfc822-headers                              [RFC1892]
		vnd.in3d.3dml				     [Powers]
		prs.lines.tag				      [Lines]
		vnd.in3d.spot                                [Powers]
                css                                         [RFC2318]
                xml                                         [RFC3023]
                xml-external-parsed-entity                  [RFC3023]
		rtf					    [Lindner]
                directory
*/

