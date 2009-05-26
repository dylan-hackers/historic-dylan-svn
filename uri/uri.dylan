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
  // Do you really want this to be a stretchy type?
  slot uri-path :: <sequence> = make(<deque>),
    init-keyword: path:;
  // keys without values are #t
  slot uri-query :: <string-table> = make(<string-table>),
    init-keyword: query:;
  slot uri-fragment :: <string> = "",
    init-keyword: fragment:;
end class <uri>;

// FIXME -- Implement the following restrictions in the initialize method
//          for the <uri> class...
//   The scheme and path components are required, though the path may be
//   empty (no characters).  When authority is present, the path must
//   either be empty or begin with a slash ("/") character.  When
//   authority is not present, the path cannot begin with two slash
//   characters ("//").  When authority is present it must have a host
//   component.
//   Need an error class for these.  e.g., <invalid-uri-error>

define class <url> (<uri>) end;

define method has-authority-part?
    (uri :: <uri>) => (has-it? :: <boolean>)
  ~empty?(uri.uri-userinfo) | ~empty?(uri.uri-host) | (uri.uri-port ~= #f)
end method has-authority-part?;


// RFC 3986, Section 3.2
//       authority   = [ userinfo "@" ] host [ ":" port ]
define method uri-authority
    (uri :: <uri>)
 => (result :: <string>)
  let result = "";
  unless (empty?(uri.uri-userinfo))
    result := concatenate(result, percent-encode($uri-userinfo, uri.uri-userinfo), "@");
  end;
  result := concatenate(result, uri.uri-host | "");
  if (uri.uri-port)
    result := concatenate(result, ":", integer-to-string(uri.uri-port));  
  end if;
  result;
end method uri-authority;

// Could use alpha?() and digit?() from string-extensions.
// Why are these not strings?
define constant $alpha =
  #('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z');
define constant $digit = #('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');

//define constant $uri-scheme = concatenate($alpha, $digit, #('+', '-', '.'));
//define constant $uri-gen-delims = #(':', '/', '?', '#', '[', ']', '@');
define constant $uri-sub-delims = #('!', '$', '&', '\'', '(', ')',  '*', '+', ',', ';', '=');
//define constant $uri-reserved = concatenate($uri-gen-delims, $uri-sub-delims);
define constant $uri-unreserved = concatenate($alpha, $digit, #('-', '.', '_', '~'));
define constant $uri-pchar = concatenate($uri-unreserved, $uri-sub-delims, #(':', '@'));
define constant $uri-userinfo = concatenate($uri-unreserved, $uri-sub-delims, #(':'));
define constant $uri-query = concatenate($uri-pchar, #('/', '?'));
define constant $uri-query-no-plus = remove($uri-query, '+');
//define constant $uri-port = $digit;
define constant $uri-segment = $uri-pchar;
//define constant $uri-fragment = $uri-query;

define constant $uri-regex :: <regex>
    = compile-regex("^(([^:/?#]+):)?(//((([^/?#]*)@)?([^/?#:]*)(:([^/?#]*))?))?([^?#]*)"
                    "(\\?([^#]*))?(#(.*))?");

define method parse-uri-as
    (class :: subclass(<uri>), uri :: <string>)
 => (result :: <uri>)
  let (uri, _scheme, scheme, _authority, authority,
       _userinfo, userinfo, host, _port, port,
       path, _query, query, _fragment, fragment)
    = regex-search-strings($uri-regex, uri);
  // inside generic method to save code duplication
  if (class == <url> & query)
    query := regex-replace(query, "\\+", " ");
  end if;
  if (scheme) scheme := percent-decode(scheme); end;
  if (userinfo) userinfo := percent-decode(userinfo); end;
  if (host) host := percent-decode(host); end;
  if (fragment) fragment := percent-decode(fragment); end;
  let uri = make(class,
                 scheme: scheme | "",
                 userinfo: userinfo | "",
                 host: host | "", 
                 port: port & string-to-integer(port),
                 fragment: fragment | "");
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
end method parse-uri-as;

define constant parse-uri = curry(parse-uri-as, <uri>);

define constant parse-url = curry(parse-uri-as, <url>);

// relative / absolute

define function relative?
    (uri :: <uri>)
 => (result :: <boolean>);
  empty?(uri.uri-scheme)
end;

define constant absolute? = complement(relative?);

// split parts 

define method split-path
    (path :: <string>)
 => (parts :: <sequence>);
  let parts = split(path, "/", remove-if-empty: #f);
  map(percent-decode, parts);
end;

define method split-query
    (query :: <string>, #key replacements :: false-or(<sequence>))
 => (parts :: <string-table>);
  let parts = split(query, "&");
  let table = make(<string-table>, size: parts.size);
  for (part in parts)
    let (qname, qvalue) = apply(values, split(part, "=",
                                              remove-if-empty: #f,
                                              count: 2));
    qname := percent-decode(qname);
    // Right now "&foo=&" is different from &foo&.  The former sets the
    // qvalue to "" and the latter sets it to #t.  Does it matter?  Is
    // &foo& even valid?
    if (qvalue)
      if (replacements)
        for (replacement in replacements)
          let old = head(replacement);
          let new = tail(replacement);
          qvalue := regex-replace(qvalue, old, new);
        end for;
      end if;
      qvalue := percent-decode(qvalue);
    else
      qvalue := #t;
    end if;
    if (element(table, qname, default: #f))
      table[qname] := if (instance?(table[qname], <string>))
                        list(table[qname], qvalue);
                      else
                        pair(qvalue, table[qname]);
                      end if;
    else
      table[qname] := qvalue;
    end if;
  end for;
  table;
end method split-query;

define method as
    (class == <string>, uri :: <uri>)
 => (result :: <string>)
  build-uri(uri)
end method as;

// build-uri 

// Turn a uri into a string
define open generic build-uri
    (uri :: <uri>, #key include-scheme, include-authority)
 => (result :: <string>); 

define method build-uri
    (uri :: <uri>,
     #key include-scheme = #t, include-authority = #t)
 => (result :: <string>)
  let parts :: <stretchy-vector> = make(<stretchy-vector>);
  if (include-scheme & ~empty?(uri.uri-scheme))
    add!(parts, uri.uri-scheme);
    add!(parts, ":")
  end;
  if (include-authority & has-authority-part?(uri))
    add!(parts, "//");
    add!(parts, uri.uri-authority);
  end;
  add!(parts, build-path(uri));
  unless (empty?(uri.uri-query))
    add!(parts, "?");
    add!(parts, build-query(uri));
  end;
  unless (empty?(uri.uri-fragment))
    add!(parts, "#");
    add!(parts, uri.uri-fragment);
  end;
  join(parts, "")
end method build-uri;

// build-path

define method build-path
    (uri :: <uri>)
 => (encoded-path :: <string>)
  if (empty?(uri.uri-path))
    ""
  else  
    join(map(curry(percent-encode, $uri-segment),
             uri.uri-path),
         "/")
  end if;
end;

// build-query

define method build-query
    (uri :: <uri>) => (encoded-query :: <string>)
  build-query-internal(uri, $uri-query)
end;

define method build-query
    (url :: <url>) => (encoded-query :: <string>)
  build-query-internal(url, $uri-query-no-plus)
end;

define method build-query-internal
    (uri :: <uri>, chars-not-to-encode :: <sequence>)
 => (encoded-query :: <string>)
  if (empty?(uri.uri-query))
    ""
  else 
    let parts = make(<stretchy-vector>);
    for (value keyed-by key in uri.uri-query)
      key := percent-encode(chars-not-to-encode, key);
      add-key-value(parts, key, value);
    end for;
    join(parts, "&")
  end if; 
end method build-query-internal;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, value :: <string>)
 => (parts :: <stretchy-vector>);
  add!(parts, concatenate(key, "=", percent-encode($uri-query, value)));
end;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, value == #t)
 => (parts :: <stretchy-vector>)
  add!(parts, key);
end;

define method add-key-value
    (parts :: <stretchy-vector>, key :: <string>, values :: <list>)
 => (parts :: <stretchy-vector>);
  for (value in values)
    add-key-value(parts, key, value);
  end for;
  parts;
end;


// percent-encode

define method percent-encode
    (chars :: <sequence>, unencoded :: <byte-string>)
 => (encoded :: <string>)
  let encoded = "";
  for (char in unencoded)
    encoded := concatenate(encoded, if (member?(char, chars))
                                      list(char)
                                    else
                                      format-to-string("%%%X", as(<byte>, char))
                                    end if);
  end for;
  encoded;
end method percent-encode;

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
  let path = split(path, "/", remove-if-empty: #f);
  path := remove-dot-segments(path);
  join(path, "/")
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

// Why is this called transform-uris?  It seems mork like a merge.
// It could use some documentation.  --cgay
//
define method transform-uris
    (base :: <uri>, reference :: <uri>, 
     #key as :: subclass(<uri>) = <uri>)
 => (target :: <uri>)
  local method merge (base, reference)
      if (has-authority-part?(base) & empty?(base.uri-path))
        // what's this about?  --cgay
        concatenate(#(""), reference.uri-path);
      else
	concatenate(copy-sequence(base.uri-path, end: base.uri-path.size - 1),
                    reference.uri-path)
      end if;
    end;
  let target = make(as);
  if (~empty?(reference.uri-scheme))
    // If the reference uri has a scheme then it is fully-qualified and
    // is used in its entirety, except for the fragment.  (If <uri> were
    // immutable we could just return the original.)
    target.uri-scheme := reference.uri-scheme;
    target.uri-userinfo := reference.uri-userinfo;
    target.uri-host := reference.uri-host;
    target.uri-port := reference.uri-port;
    target.uri-path := remove-dot-segments(reference.uri-path);
    target.uri-query := reference.uri-query;
  else
    if (has-authority-part?(reference))
      target.uri-userinfo := reference.uri-userinfo;
      target.uri-host := reference.uri-host;
      target.uri-port := reference.uri-port;
      target.uri-path := remove-dot-segments(reference.uri-path);
      target.uri-query := reference.uri-query;
    else
      // reference's scheme and authority were both empty...
      if (empty?(reference.uri-path))
        target.uri-path := base.uri-path;
        target.uri-query := if (empty?(reference.uri-query))
                              base.uri-query
                            else
                              reference.uri-query
                            end;
      else
        target.uri-path := if ( // what's this for???  --cgay
                               first(reference.uri-path) = "")
                             remove-dot-segments(reference.uri-path)
                           else
                             remove-dot-segments(merge(base, reference))
                           end;
        target.uri-query := reference.uri-query;
      end if;
      target.uri-userinfo := base.uri-userinfo;
      target.uri-host := base.uri-host;    
      target.uri-port := base.uri-port;
    end if;
    target.uri-scheme := base.uri-scheme;
  end if;
  // Why is the reference fragment used unconditionally?  --cgay
  target.uri-fragment := reference.uri-fragment;
  target;
end method transform-uris;

define method print-message (uri :: <uri>, stream :: <stream>) => ();
  format(stream, "%s", build-uri(uri))
end;

/* 

// example / usage / testing

begin
  let bar = "/foo?users=admin&users=1&users=2&members=3&members=4&comment=&add=Add";
  let foo = parse-url(bar);
  format-out("%=, %=\n", foo.uri-query["users"], foo.uri-query["members"]);
  format-out("%s\n%s\n", bar, foo);

  let foo = parse-url("http://baz.blub/pat%2fh/test?fo%20o=ba%2f%20r");
  format-out("%s, %=,%s\n", foo.uri-query, foo.uri-path, foo);

  format-out("%s\n", split-query("foo=bar+blub&baz", replacements: list(pair("\\+", " ")))["foo"]);

  let uri = parse-uri("http://foo:bar@baz.blub:23/path/test/../page?fo%20=ba+r&q1=q2&q3=&q4#extra");
  let url = parse-url("http://foo:bar@baz.blub:23/path/test/../page?fo%20o=b+r&q1=q2&q3=&q4#extra");
  format-out("%=\n", uri.uri-query);
  format-out("%=\n", url.uri-query);

  format-out("%=\n", percent-decode("foo%20bar"));
  format-out("%=\n", percent-decode("%2"));
  format-out("%=\n", percent-decode("%"));
  format-out("%=\n", percent-decode("%rg"));

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
  format-out("uri1: %=\n", uri);
  format-out("uri1 (built): %=\n", build-uri(uri1));
  let uri2 = make(<uri>, path: "../foo/../../bar");
  format-out("uri2: %=\n", uri2);
  format-out("uri2 (built): %=\n", build-uri(uri2));
  let uri3 = transform-uris(uri1, uri2);
  format-out("uri3: %=\n", uri3);
  format-out("uri3 (built): %=\n", build-uri(uri3));

  format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "../foo/../../bar"))));
  format-out("%s\n", build-uri(transform-uris(parse-uri("http://foo.bar/test"), make(<uri>, path: "/foo/bar"))));

  format-out("as: %s\n", as(<string>, parse-uri("http://foo?a=1&b=2#anchor")));
end;
*/

