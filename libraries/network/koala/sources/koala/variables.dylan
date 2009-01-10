Module:    httpi
Author:    Carl Gay
Copyright: Copyright (c) 2001-2008 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND
Synopsis:  Variables and utilities 

// Entries in this table may be overridden by entries in the mime-type-map
// file specified in the Koala config file, if any.
//
define table $default-mime-type-map = {
    #"ez" => "application/andrew-inset",
    #"bz2" => "application/x-bzip2",
    #"tar" => "application/tar",
    #"rpm" => "application/x-rpm",
    #"deb" => "application/x-deb",
    #"gz"  => "application/x-gzip",
    #"tgz" => "application/x-gzip",
    #"hqx" => "application/mac-binhex40",
    #"cpt" => "application/mac-compactpro",
    #"mathml" => "application/mathml+xml",
    #"doc" => "application/msword",
    #"bin" => "application/octet-stream",
    #"dms" => "application/octet-stream",
    #"lha" => "application/octet-stream",
    #"lzh" => "application/octet-stream",
    #"exe" => "application/octet-stream",
    #"class" => "application/octet-stream",
    #"so" => "application/octet-stream",
    #"dll" => "application/octet-stream",
    #"dmg" => "application/octet-stream",
    #"oda" => "application/oda",
    #"ogg" => "application/ogg",
    #"pdf" => "application/pdf",
    #"ai" => "application/postscript",
    #"eps" => "application/postscript",
    #"ps" => "application/postscript",
    #"rdf" => "application/rdf+xml",
    #"rtf" => "application/rtf",
    #"smi" => "application/smil",
    #"smil" => "application/smil",
    #"gram" => "application/srgs",
    #"grxml" => "application/srgs+xml",
    #"mif" => "application/vnd.mif",
    #"xls" => "application/vnd.ms-excel",
    #"ppt" => "application/vnd.ms-powerpoint",
    #"wbxml" => "application/vnd.wap.wbxml",
    #"wmlc" => "application/vnd.wap.wmlc",
    #"wmlsc" => "application/vnd.wap.wmlscriptc",
    #"vxml" => "application/voicexml+xml",
    #"bcpio" => "application/x-bcpio",
    #"vcd" => "application/x-cdlink",
    #"pgn" => "application/x-chess-pgn",
    #"cpio" => "application/x-cpio",
    #"csh" => "application/x-csh",
    #"dcr" => "application/x-director",
    #"dir" => "application/x-director",
    #"dxr" => "application/x-director",
    #"dvi" => "application/x-dvi",
    #"spl" => "application/x-futuresplash",
    #"gtar" => "application/x-gtar",
    #"hdf" => "application/x-hdf",
    #"js" => "application/x-javascript",
    #"jnlp" => "application/x-java-jnlp-file",
    #"skp" => "application/x-koan",
    #"skd" => "application/x-koan",
    #"skt" => "application/x-koan",
    #"skm" => "application/x-koan",
    #"latex" => "application/x-latex",
    #"nc" => "application/x-netcdf",
    #"cdf" => "application/x-netcdf",
    #"sh" => "application/x-sh",
    #"shar" => "application/x-shar",
    #"swf" => "application/x-shockwave-flash",
    #"sit" => "application/x-stuffit",
    #"sv4cpio" => "application/x-sv4cpio",
    #"sv4crc" => "application/x-sv4crc",
    #"tar" => "application/x-tar",
    #"tcl" => "application/x-tcl",
    #"tex" => "application/x-tex",
    #"texinfo" => "application/x-texinfo",
    #"texi" => "application/x-texinfo",
    #"t" => "application/x-troff",
    #"tr" => "application/x-troff",
    #"roff" => "application/x-troff",
    #"man" => "application/x-troff-man",
    #"me" => "application/x-troff-me",
    #"ms" => "application/x-troff-ms",
    #"ustar" => "application/x-ustar",
    #"src" => "application/x-wais-source",
    #"xhtml" => "application/xhtml+xml",
    #"xht" => "application/xhtml+xml",
    #"xslt" => "application/xslt+xml",
    #"xml" => "application/xml",
    #"xsl" => "application/xml",
    #"dtd" => "application/xml-dtd",
    #"zip" => "application/zip",
    #"au" => "audio/basic",
    #"snd" => "audio/basic",
    #"mid" => "audio/midi",
    #"midi" => "audio/midi",
    #"kar" => "audio/midi",
    #"mpga" => "audio/mpeg",
    #"mp2" => "audio/mpeg",
    #"mp3" => "audio/mpeg",
    #"aif" => "audio/x-aiff",
    #"aiff" => "audio/x-aiff",
    #"aifc" => "audio/x-aiff",
    #"m3u" => "audio/x-mpegurl",
    #"ram" => "audio/x-pn-realaudio",
    #"rm" => "audio/x-pn-realaudio",
    #"rpm" => "audio/x-pn-realaudio-plugin",
    #"ra" => "audio/x-realaudio",
    #"wav" => "audio/x-wav",
    #"pdb" => "chemical/x-pdb",
    #"xyz" => "chemical/x-xyz",
    #"bmp" => "image/bmp",
    #"cgm" => "image/cgm",
    #"gif" => "image/gif",
    #"ief" => "image/ief",
    #"jpeg" => "image/jpeg",
    #"jpg" => "image/jpeg",
    #"jpe" => "image/jpeg",
    #"jp2" => "image/jp2",
    #"pict" => "image/pict",
    #"pic" => "image/pict",
    #"pct" => "image/pict",
    #"png" => "image/png",
    #"tga" => "image/targa",
    #"jng" => "image/x-jng",
    #"svg" => "image/svg+xml",
    #"tiff" => "image/tiff",
    #"tif" => "image/tiff",
    #"djvu" => "image/vnd.djvu",
    #"djv" => "image/vnd.djvu",
    #"wbmp" => "image/vnd.wap.wbmp",
    #"ras" => "image/x-cmu-raster",
    #"pntg" => "image/x-macpaint",
    #"pnt" => "image/x-macpaint",
    #"mac" => "image/x-macpaint",
    #"ico" => "image/x-icon",
    #"pnm" => "image/x-portable-anymap",
    #"pbm" => "image/x-portable-bitmap",
    #"pgm" => "image/x-portable-graymap",
    #"ppm" => "image/x-portable-pixmap",
    #"qtif" => "image/x-quicktime",
    #"qti" => "image/x-quicktime",
    #"rgb" => "image/x-rgb",
    #"xbm" => "image/x-xbitmap",
    #"xpm" => "image/x-xpixmap",
    #"xwd" => "image/x-xwindowdump",
    #"igs" => "model/iges",
    #"iges" => "model/iges",
    #"msh" => "model/mesh",
    #"mesh" => "model/mesh",
    #"silo" => "model/mesh",
    #"wrl" => "model/vrml",
    #"vrml" => "model/vrml",
    #"ics" => "text/calendar",
    #"ifb" => "text/calendar",
    #"css" => "text/css",
    #"html" => "text/html; charset=utf-8",
    #"htm" => "text/html; charset=utf-8",
    #"asc" => "text/plain",
    #"txt" => "text/plain",
    #"rtx" => "text/richtext",
    #"rtf" => "text/rtf",
    #"sgml" => "text/sgml",
    #"sgm" => "text/sgml",
    #"tsv" => "text/tab-separated-values",
    #"wml" => "text/vnd.wap.wml",
    #"wmls" => "text/vnd.wap.wmlscript",
    #"etx" => "text/x-setext",
    #"mp4" => "video/mp4",
    #"mpeg" => "video/mpeg",
    #"mpg" => "video/mpeg",
    #"mpe" => "video/mpeg",
    #"mng" => "video/x-mng",
    #"qt" => "video/quicktime",
    #"mov" => "video/quicktime",
    #"mp4" => "video/mp4",
    #"mxu" => "video/vnd.mpegurl",
    #"dv" => "video/x-dv",
    #"dif" => "video/x-dv",
    #"avi" => "video/x-msvideo",
    #"movie" => "video/x-sgi-movie",
    #"ice" => "x-conference/x-cooltalk"
  };


// Command-line arguments parser.  The expectation is that libraries that use
// and extend koala (e.g., wiki) may want to add their own <option-parser>s to
// this before calling koala-main().
define variable *argument-list-parser* :: <argument-list-parser>
  = make(<argument-list-parser>);


// Max size of data in a POST.
define variable *max-post-size* :: false-or(<integer>) = 16 * 1024 * 1024;


define function file-contents
    (filename :: <pathname>, #key error? :: <boolean>)
 => (contents :: false-or(<string>))
  // In FD 2.0 SP1 if-does-not-exist: #f still signals an error if the file doesn't exist.
  // Remove this block when fixed.  (Reported to Fun-O August 2001.)
  block ()
    with-open-file(input-stream = filename,
                   direction: #"input",
                   if-does-not-exist: if (error?) #"error" else #f end)
      read-to-end(input-stream)
    end
  exception (ex :: <file-does-not-exist-error>)
    if (error?)
      signal(ex)
    else
      #f
    end
  end
end function file-contents;

define method parent-directory
    (dir :: <locator>, #key levels = 1) => (dir :: <directory-locator>)
  for (i from 1 to levels)
    // is there a better way to get the containing directory?
    dir := simplify-locator(subdirectory-locator(dir, ".."));
  end;
  dir
end;


// These loggers are used if no other loggers are configured.
// Usually that should only happen very early during startup when
// *server* isn't bound, if at all.

// Logger used as last resort if no other loggers are defined.
// This is the initial value used for the *request-logger*, *debug-logger*, and
// *error-logger* variables, which in turn are used as the default loggers for
// each <http-server>, which in turn are used as the default loggers for
// each <virtua-host> on that server.
//
define constant $default-logger
  = make(<logger>,
         name: "http.server",
         targets: list($stdout-log-target));

// These are thread variables for efficiency.  They can be bound once
// per request rather than figuring out which logger to use each time
// one of the log-* methods below is called.  That would be slow due
// to the need for two levels of fallback:
//   ((*virtual-host* & *virtual-host*.debug-logger)
//    | (*server* & *server*.default-virtual-host.debug-logger)
//    | *debug-logger*)

define thread variable *debug-logger* :: <logger> = $default-logger;

define thread variable *error-logger* :: <logger> = $default-logger;

define thread variable *request-logger* :: <logger> = $default-logger;

define method log-trace (format-string, #rest format-args)
  apply(%log-trace, *debug-logger*, format-string, format-args);
end;

define method log-debug (format-string, #rest format-args)
  apply(%log-debug, *debug-logger*, format-string, format-args);
end;

define method log-info (format-string, #rest format-args)
  apply(%log-info, *debug-logger*, format-string, format-args);
end;

define method log-warning (format-string, #rest format-args)
  apply(%log-warning, *error-logger*, format-string, format-args);
end;

define method log-error (format-string, #rest format-args)
  apply(%log-error, *error-logger*,  format-string, format-args);
end;


