Module:    httpi
Synopsis:  Request header parsing
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//// Header logging


// Logging of headers needs a separate logging class since it's so verbose
// and will normally be turned off.
define class <log-headers> (<log-level>)
end;

define constant $log-headers = make(<log-headers>, name: "HDR ");

define method log-header
    (format-string, #rest format-args) => ()
  apply(log-message, $log-headers, format-string, format-args);
end;



//// Headers


// Put a total limit on header size, so don't get wedged reading bogus headers.
// Make it largish though, since cookies come in via a header
define variable *max-single-header-size* :: false-or(<integer>) = 16384;

// Grow header buffer by this much
define variable *header-buffer-growth-amount* :: limited(<integer>, min: 1) = 1024;

// Max size of data in a POST.
define variable *max-post-size* :: false-or(<integer>) = 16384;

// The buffer/epos values are for internal use, wouldn't expect to doc 'em.
define function read-message-headers (stream :: <stream>,
                                      #key buffer :: <byte-string> = grow-header-buffer("", 0),
                                           start :: <integer> = 0,
                                           headers :: <header-table> = make(<header-table>))
  => (headers :: <header-table>, buffer :: <byte-string>, epos :: <integer>)
  iterate loop (buffer :: <byte-string> = buffer,
                bpos :: <integer> = start,
                peek-ch :: false-or(<character>) = #f)
    let (buffer, bpos, epos, peek-ch) = read-header-line(stream, buffer, bpos, peek-ch);
    if (epos == bpos) // blank line, done.
      values(headers, buffer, epos)
    else
      let (key, data) = split-header(buffer, bpos, epos);
      add-header(headers, key, data);
      log-header("<--%s: %s", key, data);
      loop(buffer, epos, peek-ch);
    end if;
  end iterate;
end read-message-headers;

define method add-header
    (headers :: <header-table>, key :: <string>, data,
     #key if-exists? :: <symbol> = #"append")
  let old = element(headers, key, default: #f);
  // typically there is only one header for given key, so favor that.
  if (old == #f | if-exists? == #"replace")
    headers[key] := data;
  elseif (if-exists? == #"append")
    headers[key] := iff(instance?(old, <pair>),
                        concatenate!(old, list(data)),
                        list(old, data));
  elseif (if-exists? == #"error")
    error("Attempt to add header %= which has already been added", key);
  end;
end add-header;

define function grow-header-buffer (old :: <byte-string>, len :: <integer>)
  if (*max-single-header-size* & len >= *max-single-header-size*)
    header-too-large-error(max-size: *max-single-header-size*);
  else
    let nlen = len + *header-buffer-growth-amount*;
    let new :: <byte-string>
      = make(<byte-string>, size: min(*max-single-header-size* | nlen, nlen));
    let bpos :: <integer> = old.size - len;
    // Move the last len bytes of old to new.
    for (i :: <integer> from 0 below len)
      new[i] := old[bpos + i]
    end;
    new
  end;
end grow-header-buffer;

// Read a header line, including continuation if any.  Grows buffer as needed.
// Removes all crlf's, just leaves the text of line itself.
define function read-header-line (stream :: <stream>,
                                  buffer :: <byte-string>,
                                  bpos :: <integer>,
                                  peek-ch :: false-or(<byte-character>))
    => (buffer :: <byte-string>,
        bpos :: <integer>, epos :: <integer>,
        peek-ch :: false-or(<byte-character>))
  iterate loop (buffer :: <byte-string> = buffer,
                bpos :: <integer> = bpos,
                epos :: <integer> = buffer.size,
                pos :: <integer> = bpos,
                peek-ch :: false-or(<byte-character>) = peek-ch)
    // Complain even if don't really need room for one more char, makes the
    // program logic simpler...
    if (pos == epos)
      if (*max-single-header-size* & epos - bpos >= *max-single-header-size*)
        header-too-large-error(max-size: *max-single-header-size*);
      else
        let len = epos - bpos;
        let new = grow-header-buffer(buffer, len);
        loop(new, 0, new.size, len, peek-ch);
      end;
    else
      let ch :: <byte-character> = peek-ch | read-element(stream);
      if (ch == $cr)
        let ch = read-element(stream);
        if (ch == $lf)
          if (pos == bpos) // empty line means end.
            values(buffer, bpos, pos, #f)
          else
            let ch = read-element(stream);
            if (ch == ' ' | ch == '\t') // continuation line
              loop(buffer, bpos, epos, pos, ch);
            else
              values(buffer, bpos, pos, ch)
            end;
          end;
        else
          buffer[pos] := $cr;
          loop(buffer, bpos, epos, pos + 1, ch);
        end;
      else
        buffer[pos] := ch;
        loop(buffer, bpos, epos, pos + 1, #f);
      end if;
    end;
  end iterate;
end read-header-line;

// Split header into header name (the part preceding the ':') and header value.
define function split-header (buffer :: <byte-string>,
                              bpos :: <integer>,
                              epos :: <integer>)
=> (header-key :: <string>, header-data :: <string>)
  let pos = char-position(':', buffer, bpos, epos);
  if (~pos)
    bad-header-error()
  else
    // We don't use keywords for header keys, because don't want to end
    // up interning some huge bogus header that would then drag us down
    // for the rest of eternity...
    let key = substring(buffer, bpos, pos);
    let (data-start, data-end) = trim-whitespace(buffer, pos + 1, epos);
    let data = substring(buffer, data-start, data-end);
    values(key, data)
  end;
end split-header;

////////////////////////////////////////////////////////////////////////////////

define class <header-table> (<table>)
end;

// define constant $empty-header-table = make(<header-table>);

define sealed method table-protocol (table :: <header-table>)
  => (test-fn :: <function>, hash-fn :: <function>);
  ignore(table);
  values(string-equal?, sstring-hash);
end method table-protocol;

define method sstring-hash (s :: <substring>, state)
  values(string-hash-2(s.substring-base, s.substring-start, s.size), state)
end;

define method sstring-hash (s :: <byte-string>, state)
  values(string-hash-2(s, 0, s.size), state)
end;

define inline function string-hash-2 (s :: <byte-string>,
                                      bpos :: <integer>,
                                      len :: <integer>)
  let epos :: <integer> = bpos + len;
  for (i :: <integer> from bpos below epos,
       hash :: <integer> = 0 then
	 modulo(ash(hash, 6) + logand(as(<integer>, s[i]), #x9F), 970747))
  finally
    hash
  end;
end string-hash-2;

////////////////////////////////////////////////////////////////////////////////
//
// header-value(symbol)
//

define function header-value (key :: <symbol>)
  request-header-value(*request*, key)
end;

define function request-header-value (request :: <request>, key :: <symbol>)
  let cache = request.request-header-values;
  let cached = element(cache, key, default: not-found());
  if (found?(cached))
    cached
  else
    let raw-data = element(request.request-headers, as(<string>, key),
                           default: #f);
    cache[key] := (raw-data & parse-header-value(key, raw-data))
  end
end request-header-value;

define constant <field-type> = type-union(<list>, <string>);

define open generic parse-header-value (field-name :: <symbol>,
                                        field-values :: <field-type>)
  => (parsed-field-value :: <object>);

// default method, just returns a string
define method parse-header-value (key :: <symbol>, data :: <field-type>) => (v :: <string>)
  parse-header(data)
end;

// Media type is an <avalue> whose primary value is #(type . subtype)
// and whose alist corresponds to the parameters, including the
// "q" parameter and any other accept extensions.
define sealed method parse-header-value (key == #"accept", data :: <field-type>)
    => (media-type-list :: <list>)
  parse-comma-separated-values(data, parse-media-type)
end;

// returns alist mapping charset to qvalue as an integer between 0 and 1000.
// the primary value is unused.
define sealed method parse-header-value (key == #"accept-charset", data :: <field-type>)
  => (alist :: <avalue>)
  parse-comma-separated-pairs(data, parse-quality-pair)
end;

// returns alist: (content-coding:string . qvalue:integer)
define sealed method parse-header-value (key == #"accept-encoding", data :: <field-type>)
  => (alist :: <avalue>)
  parse-comma-separated-pairs(data, parse-quality-pair)
end;

// returns alist: (language:string . qvalue:integer)
// where qvalue is an integer between 0 and 1000.
define sealed method parse-header-value (key == #"accept-language", data :: <field-type>)
  => (alist :: <avalue>)
  parse-comma-separated-pairs(data, parse-quality-pair)
end;

// cf RFC 2069
// Returns string for "basic" and <avalue> for others.
define sealed method parse-header-value (key == #"authorization", data :: <field-type>)
  => (credentials :: type-union(<string>, <avalue>))
  //(define-header-keywords "realm" "nonce" "username" "uri" "response" "digest" "algorithm" "opaque"
  //			"basic" "digest")
  parse-single-header(data, parse-authorization-value)
end;

define sealed method parse-header-value (key == #"cache-control", data :: <field-type>)
  => (params :: <avalue>)
  parse-comma-separated-pairs(data, parse-attribute-value-pair)
end;

define sealed method parse-header-value (key == #"connection", data :: <field-type>)
  => (tokens :: <list>)
  parse-comma-separated-values(data, parse-token-value);
end;

define sealed method parse-header-value (key == #"date", data :: <field-type>)
  => (date :: <date>)
  parse-single-header(data, parse-date-value)
end;


// ---TODO: *** If a server receives a request containing an Expect field
// that includes an expectation-extension that it does not support, it
// MUST respond with a 417 Expectation Failed status. ***
// So need to come up with some user-extensible API for this.
define sealed method parse-header-value (key == #"expect", data :: <field-type>)
  => (expect :: <avalue>)
  //(define-header-keywords "100-continue")
  parse-comma-separated-pairs(data, parse-expectation-pair)
end;

// This is nominally just a single from field, but what do we care...
define sealed method parse-header-value (key == #"from", data :: <field-type>)
  => (froms :: <list>)
  // parse-single-header(data, parse-string-value)
  parse-comma-separated-values(data, parse-string-value);
end;

define sealed method parse-header-value (key == #"host", data :: <field-type>)
  => (host+port :: <pair>)
  parse-single-header(data, parse-host-value)
end;

define sealed method parse-header-value (key == #"if-match", data :: <field-type>)
  => (entity-tags :: <list>)
  parse-comma-separated-values(data, parse-entity-tag-value);
end;

define sealed method parse-header-value (key == #"if-modified-since", data :: <field-type>)
  => (date :: <date>)
  parse-single-header(data, parse-date-value);
end;

define sealed method parse-header-value (key == #"if-none-match", data :: <field-type>)
  => (entity-tags :: <list>)
  parse-comma-separated-values(data, parse-entity-tag-value);
end;

define sealed method parse-header-value (key == #"if-unmodified-since", data :: <field-type>)
 => (date :: <date>)
  parse-single-header(data, parse-date-value)
end;

// ?????
//define method parse-header-value (key == #"keep-alive", data :: <field-type>)
//  parse-keep-alive-header(data)
//end;

define sealed method parse-header-value (key == #"max-forwards", data :: <field-type>)
  => (max :: <integer>)
  parse-single-header(data, parse-integer-value)
end;

// ?????
//define method parse-header-value (key == #"method", data :: <field-type>)
//  parse-comma-separated-values(data)
//end;

// HTTP/1.1 caches SHOULD treat "Pragma: no-cache" as if the client had
// sent "Cache-control: no-cache".
define sealed method parse-header-value (key == #"pragma", data :: <field-type>)
  => (params :: <avalue>)
  parse-comma-separated-pairs(data, parse-attribute-value-pair)
end;

//	;deprecated 1.0 Extension
define sealed method parse-header-value (key == #"proxy-connection", data :: <field-type>)
  => (tokens :: <list>)
  parse-comma-separated-values(data, parse-token-value);
end;

define sealed method parse-header-value (key == #"proxy-authorization", data :: <field-type>)
  => (credentials :: type-union(<string>, <avalue>))
  parse-single-header(data, parse-authorization-value)
end;

define sealed method parse-header-value (key == #"range", data :: <field-type>)
  => (ranges :: <list>)
 parse-single-header(data, parse-ranges-value)
end;

define sealed method parse-header-value (key == #"referer", data :: <field-type>)
  => (data :: <string>)
  parse-single-header(data, parse-string-value)
end;

// returns a list of avalues
define sealed method parse-header-value (key == #"TE", data :: <field-type>)
  => (encodings :: <list>)
  parse-comma-separated-values(data, parse-parameterized-value)
end;

define sealed method parse-header-value (key == #"trailer", data :: <field-type>)
  => (trailers :: <list>)
  parse-comma-separated-values(data, parse-token-value);
end;

define sealed method parse-header-value (key == #"transfer-encoding", data :: <field-type>)
  => (encodings :: <list>)
  parse-comma-separated-values(data, parse-parameterized-value)
end;

// should we parse the product?  Will anybody ever care?
define sealed method parse-header-value (key == #"upgrade", data :: <field-type>)
 => (products :: <list>)
  parse-comma-separated-values(data, parse-string-value);
end;

// Might want to parse the main product field, sometimes need to
// use that to decide what extensions are supported.
define sealed method parse-header-value (key == #"user-agent", data :: <field-type>)
  => (agent :: <string>)
  parse-single-header(data, parse-string-value)
end;

define sealed method parse-header-value (key == #"via", data :: <field-type>)
  => (vias :: <list>)
  parse-comma-separated-values(data, parse-string-value)
end;

// Might want to parse... very structured.
define sealed method parse-header-value (key == #"warning", data :: <field-type>)
  => (warnings :: <list>)
  parse-comma-separated-values(data, parse-string-value)
end;

/// Entity headers

define sealed method parse-header-value (key == #"allow", data :: <field-type>)
  => (methods :: <list>)
  parse-comma-separated-values(data, parse-token-value);
end;

define sealed method parse-header-value (key == #"content-encoding", data :: <field-type>)
  => (encodings :: <list>)
  parse-comma-separated-values(data, parse-parameterized-value)
end;

// not in 1.1
define sealed method parse-header-value (key == #"content-disposition", data :: <field-type>)
  => (disp :: <avalue>)
  parse-single-header(data, parse-parameterized-value)
end;

define sealed method parse-header-value (key == #"content-language", data :: <field-type>)
 => (langs :: <list>)
  parse-comma-separated-values(data, parse-token-value);
end;

define sealed method parse-header-value (key == #"content-length", data :: <field-type>)
  => (len :: <integer>)
  parse-single-header(data, parse-integer-value);
end;

define sealed method parse-header-value (key == #"content-location", data :: <field-type>)
  => (url :: <string>)
  parse-single-header(data, parse-string-value)
end;

define sealed method parse-header-value (key == #"content-md5", data :: <field-type>)
 => (md5 :: <string>)
  parse-single-header(data, parse-string-value)
end;

// returns #((first . last) . total)
define sealed method parse-header-value (key == #"content-range", data :: <field-type>)
  => (range :: <pair>)
  parse-single-header(data, parse-range-value)
end;

define sealed method parse-header-value (key == #"content-type", data :: <field-type>)
  => (type :: <avalue>)
  parse-single-header(data, parse-media-type)
end;

define sealed method parse-header-value (key == #"expires", data :: <field-type>)
  => (date :: <date>)
  parse-single-header(data, parse-date-value)
end;

define sealed method parse-header-value (key == #"last-modified", data :: <field-type>)
  => (date :: <date>)
  parse-single-header(data, parse-date-value)
end;


//---TODO: Verify that all strings are valid HTTP/1.1 tokens

define constant $default-cookie-version :: <byte-string> = "1";

define class <cookie> (<object>)
  slot cookie-name  :: <string>, required-init-keyword: #"name";
  slot cookie-value :: <string>, required-init-keyword: #"value";
  slot cookie-domain  :: false-or(<string>) = #f, init-keyword: #"domain";
  slot cookie-path    :: false-or(<string>) = #f, init-keyword: #"path";
  // The maximum lifetime of the cookie, in seconds.  #f means "until the user agent exits".
  slot cookie-max-age :: false-or(<integer>) = #f, init-keyword: #"max-age";
  slot cookie-comment :: false-or(<string>) = #f, init-keyword: #"comment";
  slot cookie-version :: <string> = $default-cookie-version, init-keyword: #"version";
end;

define method extract-cookies
    (str :: <byte-string>, bpos :: <integer>, epos :: <integer>, cookies :: <list>) => (cookies :: <list>)
  let cookies :: <list> = #();
  let version = "1";  // default
  let (name, value, path, domain) = values(#f, #f, #f, #f);
  local method add-cookie ()
          cookies := add(cookies,
                         make(<cookie>,
                              name: name, value: value, path: path, domain: domain,
                              version: version));
          pset (name, value, path, domain) <= values(#f, #f, #f, #f) end;
        end;
  iterate loop (bpos = bpos)
    let bpos = skip-whitespace(str, bpos, epos);
    let lim = char-position(';', str, bpos, epos)
              | char-position(',', str, bpos, epos)
              | epos;
    let (attr, val) = extract-attribute+value(str, bpos, lim);
    select (attr by string-equal?)
      "$Version" => version := val;
      "$Path"    => path := val;
      "$Domain"  => domain := val;
      otherwise  => begin
                      name & add-cookie();  // if name is set then first cookie is baked
                      name := attr;
                      value := val;
                    end;
    end select;
    unless (lim == epos)
      loop(lim + 1)
    end;
  end iterate;
  name & add-cookie();
  cookies
end;

define method parse-header-value
    (key == #"cookie", data :: <string>) => (cookies :: <list>)
  extract-cookies(data, 0, size(data), #())
end;

define method parse-header-value
    (key == #"cookie", data :: <list>) => (cookies :: <list>)
  let cookies :: <list> = #();
  for (header in data)
    cookies := concatenate(cookies, extract-cookies(header, 0, size(header), cookies))
  end;
  cookies
end;

