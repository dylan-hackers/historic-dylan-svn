module: uri
author: turbo24prg 
synopsis: RFC 3986: Uniform Resource Identifier (URI): Generic Syntax

define class <uri> (<object>)
  slot uri-scheme :: <string> = "",
    init-keyword: scheme:;
  slot uri-userinfo :: <string> = "",
    init-keyword: userinfo:;
  slot uri-host :: <string> = "",
    init-keyword: host:;
  slot uri-port :: false-or(<integer>) = #f,
    init-keyword: port:;
  slot uri-path :: <deque> = make(<deque>),
    init-keyword: path:;
  // keys without vaule are #t
  slot uri-query :: <string-table> = make(<string-table>),
    init-keyword: query:;
  slot uri-fragment :: <string> = "",
    init-keyword: fragment:;
end;

define class <url> (<uri>) end;

define method uri-authority (uri :: <uri>) => (result :: <string>);
  let result = "";
  unless (empty?(uri.uri-userinfo))
    result := concatenate(result, percent-encode(#"userinfo", uri.uri-userinfo), "@");
  end;
  result := concatenate(result, uri.uri-host | "");
  if (uri.uri-port)
    result := concatenate(result, ":", integer-to-string(uri.uri-port));  
  end if;
  result;
end;

define constant $alpha =
  #('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z');
define constant $digit = #('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

define constant $uri-parts :: <table> = make(<table>);

define constant $uri-scheme = concatenate($alpha, $digit, #('+', '-', '.'));
define constant $uri-gen-delims = #(':', '/', '?', '#', '[', ']', '@');
define constant $uri-sub-delims = #('!', '$', '&', '\'', '(', ')',  '*', '+', ',', ';', '=');
define constant $uri-reserved = concatenate($uri-gen-delims, $uri-sub-delims);
define constant $uri-unreserved = concatenate($alpha, $digit, #('-', '.', '_', '~'));
define constant $uri-pchar = concatenate($uri-unreserved, $uri-sub-delims, #(':', '@'));
define constant $uri-userinfo = concatenate($uri-unreserved, $uri-sub-delims, #(':'));
$uri-parts[#"userinfo"] := $uri-userinfo;
define constant $uri-query = concatenate($uri-pchar, #('/', '?'));
$uri-parts[#"query"] := $uri-query;
define constant $uri-port = $digit;
define constant $uri-segment = $uri-pchar;
$uri-parts[#"segment"] := $uri-segment;
define constant $uri-fragment = $uri-query;

define method parse-uri-as (class :: subclass(<uri>), uri :: <string>) => (result :: <uri>);
  let (uri, _scheme, scheme, _authority, authority, 
       _userinfo, userinfo, host, _port, port,
       path, _query, query, _fragment, fragment) = 
    regex-search-strings("^(([^:/?#]+):)?(//((([^/?#]*)@)?([^/?#:]*)(:([^/?#]*))?))?([^?#]*)(\\?([^#]*))?(#(.*))?", uri);
  // inside generic method to save code duplication
  if (class == <url> & query)
    query := regex-replace(query, "\\+", " ");
  end if;
  let (scheme, userinfo, host, path, query, fragment)
    = apply(values, map(method (slot)
			  if (instance?(slot, <string>)) 
			    percent-decode(slot) 
			  else 
			    slot 
			  end if;
			end,
      list(scheme, userinfo, host, path, query, fragment)));
  let uri = make(class, scheme: scheme | "", userinfo: userinfo | "", host: host | "", 
    port: port & string-to-integer(port), fragment: fragment | "");
  if (~empty?(path))
    uri.uri-path := split-path(path);
  end if;
  if (query)
    uri.uri-query := split-query(query);
  end if;
  if (absolute?(uri))
    uri.uri-path := remove-dot-segments(uri.uri-path);
  end if;
  uri;
end;

define constant parse-uri = curry(parse-uri-as, <uri>);

define constant parse-url = curry(parse-uri-as, <url>);

// relative / absolute

define function relative? (uri :: <uri>) => (result :: <boolean>);
  empty?(uri.uri-scheme)
end;

define constant absolute? = complement(relative?);

// split parts 

define method split-path (path :: <string>) => (parts :: <sequence>);
  split(path, "/", remove-empty-items: #f);
end;

define method split-query
    (query :: <string>, #key replacements :: false-or(<sequence>))
 => (parts :: <string-table>);
  let parts = split(query, "&");
  let table = make(<string-table>); 
  for (part in parts)
    let (key, value) = apply(values, 
      split(part, "=", remove-empty-items: #f));
    if (value)
      if (replacements)
        for (replacement in replacements)
          value := regex-replace(value,
            head(replacement), tail(replacement));
        end for;
      end if;
      value := percent-decode(value);
    else
      value := #t;
    end if;
    table[key] := value;
  end for;
  table;
end method split-query;

define function relative? (uri :: <uri>) => (is-relative? :: <boolean>);
  empty?(uri.uri-scheme);
end;

define function absolute? (uri :: <uri>) => (is-absolute? :: <boolean>);
  ~relative?(uri);
end;

// build-uri 

define open generic build-uri (uri :: <uri>) => (result :: <string>); 

define method build-uri (uri :: <uri>) => (result :: <string>);
  let result :: <string> = "";
  unless (empty?(uri.uri-scheme))
    result := concatenate(result, uri.uri-scheme, ":");
  end;
  unless (empty?(uri.uri-authority))
    result := concatenate(result, "//", uri.uri-authority);
  end; 
  result := concatenate(result, build-path(uri));
  unless (empty?(uri.uri-query))
    result := concatenate(result, "?", build-query(uri));
  end;
  unless (empty?(uri.uri-fragment))
    result := concatenate(result, "#", uri.uri-fragment);
  end;
  result;
end;

// build-path

define open generic build-path (path :: <object>, #key) => (encoded-path :: <string>);

define method build-path (uri :: <uri>, #key include :: <sequence> = #()) => (encoded-path :: <string>);    
  if (empty?(uri.uri-path)) "" else  
    apply(join, "/", map(method (segment)
        percent-encode(#"segment", segment, include: include)
      end, uri.uri-path));
  end if;
end;

// build-query

define open generic build-query (query :: <object>, #key) => (encoded-query :: <string>);

define method build-query (uri :: <uri>, #key include :: <sequence> = #()) => (encoded-query :: <string>);
  if (empty?(uri.uri-query)) "" else 
    let parts = make(<stretchy-vector>);
    for (value keyed-by key in uri.uri-query)
      key := percent-encode(#"query", key, include: include);
      add!(parts, if (value == #t)
          key
        else
          concatenate(key, "=", percent-encode(#"query", value, include: include));
        end if);
    end for;
    apply(join, "&", parts);
  end if; 
end;

define method build-query (url :: <url>, #key) => (encoded-query :: <string>);
  next-method(url, include: #('+'));
end;

// percent-encode

define generic percent-encode (part :: <object>, unencoded :: <object>, #key) => (encoded :: <string>);

define method percent-encode (part, unencoded :: <byte-string>, #key include :: <sequence> = #()) => (encoded :: <string>);
  let encoded = "";
  for (char in unencoded)
    encoded := concatenate(encoded, if (member?(char, $uri-parts[part]) & ~member?(char, include))
      list(char) else percent-encode(part, char) end if);
  end for;
  encoded;
end method percent-encode;

define method percent-encode (part, unencoded :: <character>, #key) => (encoded :: <string>);
  format-to-string("%%%X", as(<byte>, unencoded));
end;

// percent-decode

define method percent-decode (encoded :: <byte-string>) => (unencoded :: <string>);
  let result = "";
  let (decode?, ignore?) = values(#f, #f);
  for (char in encoded, position from 0)
    if (ignore?)
      ignore? := #f;
    else
      if (char = '%' & ~decode?)
        decode? := #t;
      else
        if (decode? & size(encoded) > position + 1)
	  let low = encoded[position + 1];
	  char := as(<string>, list(char, low));
	  char := string-to-integer(char, base: 16);
          char := as(<byte-character>, char);
          ignore? := #t;
	  decode? := #f;
        end if;
	unless (decode?)
	  result := concatenate(result, list(char));
	end unless;
      end if;
    end if;
  end for;
  result;
end method percent-decode;

// remove-dot-segments

define generic remove-dot-segments (path :: <object>) => (result :: <object>);

define method remove-dot-segments (path :: <string>) => (result :: <string>);
  let path = split(path, "/", remove-empty-items: #f);
  path := remove-dot-segments(path);
  apply(join, "/", path);
end;

define method remove-dot-segments (path :: <sequence>) => (result :: <sequence>);
  let input = make(<deque>);
  do(curry(push-last, input), path);
  let output = make(<deque>);
  for (segment in input, i from 0)
    let last? = (i = size(input) - 1);
    if ((segment = "." | segment = "") & last?)
      push-last(output, "");
    elseif (segment = ".." & last?)
      last(output) := "";
    elseif (segment = "..")
      if (size(output) > 0 & last(output) ~= "")
        pop-last(output);
      end if;
    elseif (segment = ".")
    else
      push-last(output, segment);    
    end if;
  end for;
  output;
end;

define method transform-uris (base :: <uri>, reference :: <uri>, 
 #key as :: subclass(<uri>) = <uri>) => (target :: <uri>);
  local method merge (base, reference)
      if (~empty?(base.uri-authority) & empty?(base.uri-path))
        concatenate(#(""), reference.uri-path);
      else
	concatenate(copy-sequence(base.uri-path, end: base.uri-path.size - 1), reference.uri-path)
      end if;
    end;
  let target = make(as);
  if (~empty?(reference.uri-scheme))
    target.uri-scheme := reference.uri-scheme;
    // target.uri-authority = reference.uri-authority;
      target.uri-userinfo := reference.uri-userinfo;
      target.uri-host := reference.uri-host;
      target.uri-port := reference.uri-port;
    target.uri-path := remove-dot-segments(reference.uri-path);
    target.uri-query := reference.uri-query;
  else
    if (~empty?(reference.uri-authority))
      // target.uri-authority = reference.uri-authority;
        target.uri-userinfo := reference.uri-userinfo;
        target.uri-host := reference.uri-host;
        target.uri-port := reference.uri-port;
      target.uri-path := remove-dot-segments(reference.uri-path);
      target.uri-query := reference.uri-query;
    else
      if (empty?(reference.uri-path))
        target.uri-path := base.uri-path;
        if (~empty?(reference.uri-query))
          target.uri-query := reference.uri-query;
        else
          target.uri-query := base.uri-query;
        end if;
      else
        if (~empty?(reference.uri-path) & first(reference.uri-path) = "")
          target.uri-path := remove-dot-segments(reference.uri-path);
        else
          target.uri-path := remove-dot-segments(merge(base, reference));  
	end if;
        target.uri-query := reference.uri-query;
      end if;
      // target.uri-authority = base.uri-authority;
        target.uri-userinfo := base.uri-userinfo;
        target.uri-host := base.uri-host;    
        target.uri-port := base.uri-port;
    end if;
    target.uri-scheme := base.uri-scheme;
  end if;
  target.uri-fragment := reference.uri-fragment;
  target;
end;

define method print-message (uri :: <uri>, stream :: <stream>) => ();
  format(stream, "%s", build-uri(uri))
end;


begin
/*
  format-out("%s\n", split-query("foo=bar+blub&baz", replacements: list(pair("\\+", " ")))["foo"]);

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?fo%20=ba+r&q1=q2&q3=&q4#extra");
  let url = parse-url("http://foo:bar@baz.blub:23/path/test/../page?fo%20o=b+r&q1=q2&q3=&q4#extra");
  format-out("%=\n", uri.uri-query);
  format-out("%=\n", url.uri-query);
*/
/*
  format-out("%=\n", percent-decode("foo%20bar"));
  format-out("%=\n", percent-decode("%2"));
  format-out("%=\n", percent-decode("%"));
  format-out("%=\n", percent-decode("%rg"));
*/
end;


/*
let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?foo=bar&q1=q2#extra");
format-out("%s\n", build-uri(uri)); 
uri := make(<uri>, scheme: "http", userinfo: "foo@bar:blub");
format-out("%s\n", build-uri(uri));
uri := make(<uri>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
last(uri.uri-path) := "foo/bar+baz";
format-out("%s\n", build-uri(uri));
let url = make(<url>, scheme: "http", host: "foobar", path: "/p1/p2/p3", query: "k1=v1&k2=v2");
last(url.uri-path) := "foo/bar+baz";
format-out("%s\n", build-uri(url));

let uri1 = parse-uri("http://foo.bar/test");
format-out("\n");
format-out("uri1: %=\n", uri);
format-out("uri1 (built): %=\n", build-uri(uri1));
let uri2 = make(<uri>, path: "../foo/../../bar");
format-out("\n");
format-out("uri2: %=\n", uri2);
format-out("uri2 (built): %=\n", build-uri(uri2));
let uri3 = transform-uris(uri1, uri2);
format-out("\n");
format-out("uri3: %=\n", uri3);
format-out("uri3 (built): %=\n", build-uri(uri3));

//format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "../foo/../../bar"))));
//format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "/foo/bar"))));
*/
