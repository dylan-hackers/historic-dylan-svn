Module: mime-internal
Synopsis: MIME tools
Author: Carl Gay

// So far just simple mime data types and mappings.  Should be expanded to
// handle MIME message parsing etc.

define open class <mime-error> (<format-string-condition>, <error>)
end;

define open class <invalid-mime-type-error> (<mime-error>)
end;

define open generic mime-type    (mt :: <mime-type>) => (type :: <byte-string>);
define open generic mime-subtype (mt :: <mime-type>) => (subtype :: <byte-string>);
define open generic mime-name    (mt :: <mime-type>) => (name :: <byte-string>);

define open class <mime-type> (<object>)
  constant slot mime-type :: <byte-string>,
    required-init-keyword: type:;
  constant slot mime-subtype :: <byte-string>,
    required-init-keyword: subtype:;
  slot mime-name :: <byte-string>;
end class <mime-type>;

define method initialize
    (mt :: <mime-type>, #key)
  next-method();
  mt.mime-name := concatenate(mt.mime-type, "/", mt.mime-subtype);
end;

define method print-object
    (mt :: <mime-type>, stream :: <stream>) => ()
  write(stream, mt.mime-name);
end;

define method \=
    (mt1 :: <mime-type>, mt2 :: <mime-type>) => (equal? :: <boolean>)
  mt1.mime-type = mt2.mime-type & mt1.mime-subtype = mt2.mime-subtype
end;

define method mime-type-to-string
    (mtype :: <mime-type>) => (string :: <byte-string>)
  mtype.mime-name
end;

define method string-to-mime-type
    (string :: <string>,
     #key class :: subclass(<mime-type>) = <mime-type>)
 => (mime-type :: <mime-type>)
  let parts = split(string, '/');
  if (parts.size ~= 2)
    signal(make(<invalid-mime-type-error>,
                format-string: "Invalid MIME type: %s",
                format-arguments: list(string)));
  else
    // TODO: all other validation. :)
    make(class, type: parts[0], subtype: parts[1])
  end
end method string-to-mime-type;


//// <mime-type-map>

// Looks like there's some support for limited tables, but no way to
// specify the key type so roll our own....
define open class <mime-type-map> (<table>)
end;

// Can't subclass <string-table> because it's sealed. :-(
define sealed method table-protocol
    (table :: <mime-type-map>) => (test :: <function>, hash :: <function>);
  values(method (x :: <string>, y :: <string>) x = y end,
         string-hash);
end;

/* frak.  sealing also prevents this.
define method element-setter
    (new-value, collection :: <mime-type-map>, key)
 => (new-value)
  if (~instance?(new-value, <mime-type>))
    signal(make(<type-error>, value: new-value, type: <mime-type>))
  else
    next-method()
  end;
end method element-setter;
*/

// Load a standard mime.types file, storing the mappings in the
// given <mime-type-map>.  File format is:
//    type/subtype ext1 ext2 ...
// Blank lines and lines starting with '#' are ignored.
//
define open generic load-mime-types
    (type-map :: <mime-type-map>, pathname :: <pathname>)
 => ();

define method load-mime-types
    (type-map :: <mime-type-map>, pathname :: <pathname>)
 => ()
  with-open-file (stream = pathname)
    iterate loop (line = read-line(stream, on-end-of-file: #f))
      if (line)
        line := trim(line);
        if (~empty?(line) & line[0] ~= '#')
          let line-parts = split(line, rcurry(member?, " \t"));
          let type-parts = split(line-parts[0], '/');
          if (line-parts.size > 1 & type-parts = 2)
            let (type, subtype) = apply(values, type-parts);
            for (extension in copy-sequence(line-parts, start: 1))
              let mt = extension-to-mime-type(extension, type-map)
                         | make(<mime-type>, type: type, subtype: subtype);
              extension-to-mime-type(extension, type-map) := mt;
            end;
          end if;
        end if;
        loop(read-line(stream, on-end-of-file: #f));
      end if;
    end iterate;
  end with-open-file;
end method load-mime-types;

define open generic extension-to-mime-type
    (extension :: <string>, type-map :: <mime-type-map>)
 => (mt :: false-or(<mime-type>));

define method extension-to-mime-type
    (extension :: <byte-string>, type-map :: <mime-type-map>)
 => (mt :: false-or(<mime-type>))
  element(type-map, extension, default: #f)
end;

define open generic extension-to-mime-type-setter
    (mt :: <mime-type>, extension :: <string>, type-map :: <mime-type-map>)
 => (mt :: <mime-type>);

define method extension-to-mime-type-setter
    (mt :: <mime-type>, extension :: <byte-string>, type-map :: <mime-type-map>)
 => (mt :: <mime-type>)
  type-map[extension] := mt
end;

/* Not sure I want to pull in the locators code just yet...
define open generic locator-mime-type
    (o :: <object>) => (mt :: false-or(<mime-type>));

define method locator-mime-type
    (locator :: <file-locator>,
     #key type-map :: <mime-type-map> = $default-mime-type-map)
 => (mt :: false-or(<mime-type>))
  extension-to-mime-type(type-map, locator-extension(locator))
end;
*/

define constant $default-mime-type-map :: <mime-type-map>
  = begin
      let mt-map = make(<mime-type-map>);
      local method mt (ext, type, subtype)
              mt-map[ext] := make(<mime-type>, type: type, subtype: subtype);
            end;

      mt("ez",      "application", "andrew-inset");
      mt("bz2",     "application", "x-bzip2");
      mt("tar",     "application", "tar");
      mt("rpm",     "application", "x-rpm");
      mt("deb",     "application", "x-deb");
      mt("gz",      "application", "x-gzip");
      mt("tgz",     "application", "x-gzip");
      mt("hqx",     "application", "mac-binhex40");
      mt("cpt",     "application", "mac-compactpro");
      mt("mathml",  "application", "mathml+xml");
      mt("doc",     "application", "msword");
      mt("bin",     "application", "octet-stream");
      mt("dms",     "application", "octet-stream");
      mt("lha",     "application", "octet-stream");
      mt("lzh",     "application", "octet-stream");
      mt("exe",     "application", "octet-stream");
      mt("class",   "application", "octet-stream");
      mt("so",      "application", "octet-stream");
      mt("dll",     "application", "octet-stream");
      mt("dmg",     "application", "octet-stream");
      mt("oda",     "application", "oda");
      mt("ogg",     "application", "ogg");
      mt("pdf",     "application", "pdf");
      mt("ai",      "application", "postscript");
      mt("eps",     "application", "postscript");
      mt("ps",      "application", "postscript");
      mt("rdf",     "application", "rdf+xml");
      mt("rtf",     "application", "rtf");
      mt("smi",     "application", "smil");
      mt("smil",    "application", "smil");
      mt("gram",    "application", "srgs");
      mt("grxml",   "application", "srgs+xml");
      mt("mif",     "application", "vnd.mif");
      mt("xls",     "application", "vnd.ms-excel");
      mt("ppt",     "application", "vnd.ms-powerpoint");
      mt("wbxml",   "application", "vnd.wap.wbxml");
      mt("wmlc",    "application", "vnd.wap.wmlc");
      mt("wmlsc",   "application", "vnd.wap.wmlscriptc");
      mt("vxml",    "application", "voicexml+xml");
      mt("bcpio",   "application", "x-bcpio");
      mt("vcd",     "application", "x-cdlink");
      mt("pgn",     "application", "x-chess-pgn");
      mt("cpio",    "application", "x-cpio");
      mt("csh",     "application", "x-csh");
      mt("dcr",     "application", "x-director");
      mt("dir",     "application", "x-director");
      mt("dxr",     "application", "x-director");
      mt("dvi",     "application", "x-dvi");
      mt("spl",     "application", "x-futuresplash");
      mt("gtar",    "application", "x-gtar");
      mt("hdf",     "application", "x-hdf");
      mt("js",      "application", "x-javascript");
      mt("jnlp",    "application", "x-java-jnlp-file");
      mt("skp",     "application", "x-koan");
      mt("skd",     "application", "x-koan");
      mt("skt",     "application", "x-koan");
      mt("skm",     "application", "x-koan");
      mt("latex",   "application", "x-latex");
      mt("nc",      "application", "x-netcdf");
      mt("cdf",     "application", "x-netcdf");
      mt("sh",      "application", "x-sh");
      mt("shar",    "application", "x-shar");
      mt("swf",     "application", "x-shockwave-flash");
      mt("sit",     "application", "x-stuffit");
      mt("sv4cpio", "application", "x-sv4cpio");
      mt("sv4crc",  "application", "x-sv4crc");
      mt("tar",     "application", "x-tar");
      mt("tcl",     "application", "x-tcl");
      mt("tex",     "application", "x-tex");
      mt("texinfo", "application", "x-texinfo");
      mt("texi",    "application", "x-texinfo");
      mt("t",       "application", "x-troff");
      mt("tr",      "application", "x-troff");
      mt("roff",    "application", "x-troff");
      mt("man",     "application", "x-troff-man");
      mt("me",      "application", "x-troff-me");
      mt("ms",      "application", "x-troff-ms");
      mt("ustar",   "application", "x-ustar");
      mt("src",     "application", "x-wais-source");
      mt("xhtml",   "application", "xhtml+xml");
      mt("xht",     "application", "xhtml+xml");
      mt("xslt",    "application", "xslt+xml");
      mt("xml",     "application", "xml");
      mt("xsl",     "application", "xml");
      mt("dtd",     "application", "xml-dtd");
      mt("zip",     "application", "zip");
      mt("au",    "audio", "basic");
      mt("snd",   "audio", "basic");
      mt("mid",   "audio", "midi");
      mt("midi",  "audio", "midi");
      mt("kar",   "audio", "midi");
      mt("mpga",  "audio", "mpeg");
      mt("mp2",   "audio", "mpeg");
      mt("mp3",   "audio", "mpeg");
      mt("aif",   "audio", "x-aiff");
      mt("aiff",  "audio", "x-aiff");
      mt("aifc",  "audio", "x-aiff");
      mt("m3u",   "audio", "x-mpegurl");
      mt("ram",   "audio", "x-pn-realaudio");
      mt("rm",    "audio", "x-pn-realaudio");
      mt("rpm",   "audio", "x-pn-realaudio-plugin");
      mt("ra",    "audio", "x-realaudio");
      mt("wav",   "audio", "x-wav");
      mt("pdb",   "chemical", "x-pdb");
      mt("xyz",   "chemical", "x-xyz");
      mt("bmp",   "image", "bmp");
      mt("cgm",   "image", "cgm");
      mt("gif",   "image", "gif");
      mt("ief",   "image", "ief");
      mt("jpeg",  "image", "jpeg");
      mt("jpg",   "image", "jpeg");
      mt("jpe",   "image", "jpeg");
      mt("jp2",   "image", "jp2");
      mt("pict",  "image", "pict");
      mt("pic",   "image", "pict");
      mt("pct",   "image", "pict");
      mt("png",   "image", "png");
      mt("tga",   "image", "targa");
      mt("jng",   "image", "x-jng");
      mt("svg",   "image", "svg+xml");
      mt("tiff",  "image", "tiff");
      mt("tif",   "image", "tiff");
      mt("djvu",  "image", "vnd.djvu");
      mt("djv",   "image", "vnd.djvu");
      mt("wbmp",  "image", "vnd.wap.wbmp");
      mt("ras",   "image", "x-cmu-raster");
      mt("pntg",  "image", "x-macpaint");
      mt("pnt",   "image", "x-macpaint");
      mt("mac",   "image", "x-macpaint");
      mt("ico",   "image", "x-icon");
      mt("pnm",   "image", "x-portable-anymap");
      mt("pbm",   "image", "x-portable-bitmap");
      mt("pgm",   "image", "x-portable-graymap");
      mt("ppm",   "image", "x-portable-pixmap");
      mt("qtif",  "image", "x-quicktime");
      mt("qti",   "image", "x-quicktime");
      mt("rgb",   "image", "x-rgb");
      mt("xbm",   "image", "x-xbitmap");
      mt("xpm",   "image", "x-xpixmap");
      mt("xwd",   "image", "x-xwindowdump");
      mt("igs",   "model", "iges");
      mt("iges",  "model", "iges");
      mt("msh",   "model", "mesh");
      mt("mesh",  "model", "mesh");
      mt("silo",  "model", "mesh");
      mt("wrl",   "model", "vrml");
      mt("vrml",  "model", "vrml");
      mt("ics",   "text", "calendar");
      mt("ifb",   "text", "calendar");
      mt("css",   "text", "css");
      mt("html",  "text", "html");  // ; charset=utf-8
      mt("htm",   "text", "html");  // ; charset=utf-8
      mt("asc",   "text", "plain");
      mt("txt",   "text", "plain");
      mt("rtx",   "text", "richtext");
      mt("rtf",   "text", "rtf");
      mt("sgml",  "text", "sgml");
      mt("sgm",   "text", "sgml");
      mt("tsv",   "text", "tab-separated-values");
      mt("wml",   "text", "vnd.wap.wml");
      mt("wmls",  "text", "vnd.wap.wmlscript");
      mt("etx",   "text", "x-setext");
      mt("mp4",   "video", "mp4");
      mt("mpeg",  "video", "mpeg");
      mt("mpg",   "video", "mpeg");
      mt("mpe",   "video", "mpeg");
      mt("mng",   "video", "x-mng");
      mt("qt",    "video", "quicktime");
      mt("mov",   "video", "quicktime");
      mt("mp4",   "video", "mp4");
      mt("mxu",   "video", "vnd.mpegurl");
      mt("dv",    "video", "x-dv");
      mt("dif",   "video", "x-dv");
      mt("avi",   "video", "x-msvideo");
      mt("movie", "video", "x-sgi-movie");
      mt("ice",   "x-conference", "x-cooltalk");
      mt-map
    end;


