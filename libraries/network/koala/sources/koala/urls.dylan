Module:    httpi
Synopsis:  HTTP Support
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//define class <sealed-constructor> (<object>) end;
define sealed domain make(subclass(<sealed-constructor>));
define sealed domain initialize(<sealed-constructor>);

define function make-locator (netloc :: false-or(<http-server-url>),
                              dir :: <simple-object-vector>,
                              name :: false-or(<string>),
                              type :: false-or(<string>),
                              query :: false-or(<string>),
                              // note request url's don't have tags, it's a browser thang.
                              tag :: false-or(<string>))
  let dir = make(<directory-url>, server: netloc, relative?: #f, path: dir);
  if (type | name | tag | query)
    make(<file-url>, directory: dir, base: name, extension: type,
         cgi-string: query, index: tag)
  else
    dir
  end;
end make-locator;

define function decode-url
    (str :: <byte-string>, bpos :: <integer>, epos :: <integer>)
 => (str :: <byte-string>)
  // Note: n accumulates how many chars are NOT needed in the copy.
  iterate count (pos :: <integer> = bpos, n :: <integer> = 0)
    let pos = char-position('%', str, pos, epos);
    if (pos)
      if (pos + 3 <= epos)
        count(pos + 3, n + 2)
      else
        invalid-url-encoding-error();
      end;
    elseif (n == 0)
      substring(str, bpos, epos)
    else // Ok, really have to copy...
      let nlen = epos - bpos - n;
      let nstr = make(<byte-string>, size: nlen);
      iterate copy (i :: <integer> = 0, pos :: <integer> = bpos)
        unless (pos == epos)
          let ch = str[pos];
          if (ch ~== '%')
            nstr[i] := ch;
            copy(i + 1, pos + 1);
          else
            let c1 = digit-weight(str[pos + 1]);
            let c2 = digit-weight(str[pos + 2]);
            if (c1 & c2)
              nstr[i] := as(<byte-character>, c1 * 16 + c2);
              copy(i + 1, pos + 3);
            else
              invalid-url-encoding-error();
            end;
          end;
        end unless;
      end iterate;
      nstr
    end if;
  end iterate;
end decode-url;

define function parse-request-url (str, bpos, epos)
  => (url :: <url>) // <http-url>, but that's bogus.
  parse-url(str, bpos, epos)
    | invalid-url-error(url: substring(str, bpos, epos));
end;

define function parse-url (str, str-beg, str-end)
    => (url :: false-or(<url>))
  // Assumed to be either absolute URL (i.e. "scheme:...") or
  // absolute path (i.e. "/...").  Doesn't accept relative path.
  // For now, only accepts http: as scheme.
  if (str-beg == str-end)
    #f  // This should probably treat "" the same as "/" (according to RFC 2616) --sigue
  elseif (str[str-beg] == '/')
    let (dir, name, type, query, tag) = parse-url-path(str, str-beg, str-end);
    dir & make-locator(#f, dir, name, type, query, tag);
  elseif (looking-at?("http://", str, str-beg, str-end))
    let net-beg = str-beg + 7;
    let net-end = char-position('/', str, net-beg, str-end) | str-end;
    let netloc = parse-http-server(str, net-beg, net-end);
    let (dir, name, type, query, tag) = if (net-end == str-end)
                                          parse-url-path("/", 0, 1)
                                        else
                                          parse-url-path(str, net-end, str-end)
                                        end;
    dir & netloc & make-locator(netloc, dir, name, type, query, tag);
  else
    //---TODO: here should distinguish between an unknown scheme and a relative path.
    #f
  end;
end parse-url;


define function parse-http-server (str :: <byte-string>,
                                   net-beg :: <integer>,
                                   net-end :: <integer>)
  => (netloc :: false-or(<http-server-url>))
  let host-end = char-position(':', str, net-beg, net-end) | net-end;
  let host = decode-url(str, net-beg, host-end);
  let port = if (host-end == net-end)
               80
             else
               //---TODO: should decode-url this as well, in theory...
               string->integer(str, host-end + 1, net-end)
             end;
  host & port & make(<http-server-url>, host: host, port: port);
end parse-http-server;

//---TODO: should intern these, i.e. map the whole thing to its parsed version...

// dir is #f if parse failed.
define function parse-url-path
    (str, str-beg, str-end)
 => (dir :: false-or(<simple-object-vector>),
     name :: false-or(<string>),
     type :: false-or(<string>),
     query :: false-or(<string>))
  assert(str[str-beg] == '/');
  let path-end = char-position('?', str, str-beg, str-end) | str-end;
  let segs = make(<stretchy-vector>);
  iterate loop (beg = str-beg)
    let beg = beg + 1;
    let pos = char-position('/', str, beg, path-end);
    if (pos)
      let seg = decode-url(str, beg, pos);
      if (seg)
        add!(segs, seg);
        loop(pos);
      else
        values(#f, #f, #f, #f);
      end;
    else
      let segs = as(<simple-object-vector>, segs);
      let dot-pos = char-position-from-end('.', str, beg, path-end);
      let name = decode-url(str, str-beg, dot-pos | path-end);
      let type = dot-pos & decode-url(str, dot-pos + 1, path-end);
      let query = (path-end ~== str-end) & substring(str, path-end + 1, str-end);
      values(segs, name, type, query)
    end;
  end iterate;
end parse-url-path;


