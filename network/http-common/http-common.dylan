Module: http-common
Synopsis: Code shared by HTTP client and server.

/////////////// Parsing //////////////


// RFC 2616, 2.2
define constant $token-character-map
  = begin
      let vec = make(<vector>, size: 128, fill: #t);
      let separator-chars = "()<>@,;:\\\"/[]?={} \t";
      for (char in separator-chars)
        vec[as(<integer>, char)] := #f;
      end;
      // US ASCII control characters...
      for (code from 0 to 32)
        vec[code] := #f;
      end;
      vec[127] := #f;   // DEL
      vec
    end;

define inline function token-char?
    (char :: <byte-character>) => (token-char? :: <boolean>)
  let code :: <integer> = as(<integer>, char);
  code <= 127 & $token-character-map[code]
end;

define inline function non-token-char?
    (char :: <byte-character>) => (non-token-char? :: <boolean>)
  ~token-char?(char)
end;

define function token-end-position (buf :: <byte-string>,
                                    bpos :: <integer>,
                                    epos :: <integer>)
  char-position-if(non-token-char?, buf, bpos, epos)
end;

define method validate-http-version
    (version :: <string>)
 => (version :: <symbol>)
  // Take care not to intern arbitrary symbols...
  select (version by string-equal?)
    "HTTP/0.9" => #"HTTP/0.9";
    "HTTP/1.0" => #"HTTP/1.0";
    "HTTP/1.1" => #"HTTP/1.1";
    otherwise => unsupported-http-version-error();
  end select;
end method validate-http-version;

define method validate-http-status-code
    (status-code :: <string>)
 => (status-code :: <integer>)
  let code = string-to-integer(status-code, default: -1);
  if (code < 100 | code > 599)
    signal(make(<internal-server-error>,
                format-string: "Invalid HTTP status code: %=",
                format-arguments: list(status-code),
                code: 500));
  else
    code
  end
end method validate-http-status-code;

///////////// Requests ////////////

define open class <base-http-request> (<object>)

  // todo -- url, raw-url, method, and version (everything that comes in the request
  // line) should be constant slots.  The server needs to be updated to read the
  // request line before the request object is created before this can happen, which
  // should not be a problem.

  /* constant */ slot request-url :: false-or(<url>),
    /* required- */ init-keyword: url:;

  /* constant */ slot request-raw-url-string :: false-or(<byte-string>),
    init-keyword: raw-url:;

  // todo -- RFC 2616, 5.1.1 -- The request method is case-sensitive.
  //         So it shouldn't be a <symbol>.
  /* constant */ slot request-method :: <symbol>,
    init-keyword: method:,
    init-value: #"GET";

  /* constant */ slot request-version :: <symbol>,
    init-keyword: version:,
    init-value: #"http/1.1";

  // Raw headers, mapping case-insensitive-header-name to unparsed header value.
  constant slot request-raw-headers :: <header-table>,
    init-keyword: headers:,
    init-function: curry(make, <header-table>);

  // Parsed headers.  Header values are parsed on demand only.
  constant slot request-parsed-headers :: <header-table>,
    init-function: curry(make, <header-table>);

  // The body content of the request.  Only present for POST?
  slot request-content :: <string>,
    init-keyword: content:,
    init-value: "";

end class <base-http-request>;

define method make
    (request :: subclass(<base-http-request>), #rest args, #key url)
 => (request :: subclass(<base-http-request>))
  if (instance?(url, <string>))
    apply(next-method, request, raw-url: url, url: parse-uri(url), args)
  else
    // url is a <url> or #f
    next-method()
  end
end method make;

// Read a line of input from the stream, dealing with CRLF correctly.
// See also: read-header-line
//
define method read-http-line
    (stream :: <stream>)
 => (buffer :: <byte-string>, len :: <integer>)
  let buffer = grow-header-buffer("", 0);
  iterate loop (buffer :: <byte-string> = buffer,
                len :: <integer> = buffer.size,
                pos :: <integer> = 0,
                peek-ch :: false-or(<character>) = #f)
    if (pos == len)
      let buffer = grow-header-buffer(buffer, len);
      loop(buffer, buffer.size, pos, peek-ch)
    else
      let ch :: <byte-character> = peek-ch | read-element(stream);
      if (ch == $cr)
        let ch = read-element(stream);
        if (ch == $lf)
          values(buffer, pos)
        else
          buffer[pos] := $cr;
          loop(buffer, len, pos + 1, ch)
        end;
      else
        buffer[pos] := ch;
        loop(buffer, len, pos + 1, #f)
      end if;
    end;
  end iterate;
end method read-http-line;


///////////// Responses ///////////////

define open class <base-http-response> (<object>)

  constant slot response-request :: <base-http-request>,
    required-init-keyword: #"request";

  // Headers received in (or to send with) the response.
  // @see add-header
  constant slot response-headers :: <header-table>,
    required-init-keyword: #"headers";

  slot response-code :: <integer>,
    init-keyword: code:,
    init-value: 200;

  slot response-reason-phrase :: <string>,
    init-keyword: reason-phrase:,
    init-value: "OK";

end class <base-http-response>;



