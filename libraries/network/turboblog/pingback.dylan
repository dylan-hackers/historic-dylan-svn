module: turboblog
author: turbo24prg

define class <blog-pingback> (<pingback>)
  slot pingback-number :: <integer>,
    init-keyword: pingback-number:;
  slot pingback-entry :: <blog-entry>,
    init-keyword: pingback-entry:;
end;

define method add-pingback (entry :: <blog-entry>, pingback :: <blog-pingback>)
 => (pingback :: <blog-pingback>);
  let new-number = entry.pingbacks-count;
  pingback.pingback-number := new-number;
  entry.pingbacks[new-number] := pingback;
  pingback.pingback-entry := entry;
  pingback;
end;

define method http-request (url :: <string>)
 => (socket :: <tcp-socket>);
  let url = parse-uri(url, as: <url>);
  let socket = make(<tcp-socket>, host: url.uri-host, port: url.uri-port);
  let path = concatenate(build-path(url), build-query(url));
  let request = concatenate("GET ", path, " HTTP/1.1\r\n",
			     "Host: ", url.uri-host, "\r\n\r\n");
  write(socket, request);
  socket;
end;

define method ping (source, target)
  let entry = entry-from-url(target);
  unless (entry)
    xml-rpc-fault(33, "The specified target URI cannot be used as a target. It either doesn't exist, or it is not a pingback-enabled resource.");
  end;

  if (any?(method (pingback)
      pingback.pingback-source = source
    end, map-as(<vector>, method (entry) entry end, entry.pingbacks)))
    xml-rpc-fault(48, "The pingback has already been registered.");
  end if;

  let url = parse-uri(source, as: <url>);
  if (empty?(url.uri-host))
    xml-rpc-fault(16, "The source URI does not exist.");
  end if;

  let socket = http-request(source);
  let response = read-line(socket);
  unless (substring-position(response, "200 OK"))
    xml-rpc-fault(16, "The source URI does not exist.");
  end;

  let (content-type, content-length) = values(#f, #f);
  let current = #f;
  while ((current := read-line(socket)) ~= "")
    let (header, content-type-value) = regex-search-strings(compile-regex("Content-Type: (.*)"), current);
    if (content-type-value)
      content-type := remove!(content-type-value, '\r', test: \=);
    else
      let (header, content-length-value) = regex-search-strings(compile-regex("Content-Length: (\\d+)"), current); 
      if (content-length-value)
        content-length := remove!(content-length-value, '\r', test: \=);
      end if;
    end if;
  end while;

  unless (substring-position(content-type, "text"))
    xml-rpc-fault(0, "The source URI isn't text.");
  end unless;

  let content = "";  
  if (content-length)
    content := read(socket, string-to-integer(content-length));
  else
    while ((content-length := read-line(socket)) ~= "0")
      content := concatenate(content,
        read(socket, string-to-integer(content-length, base: 16)));
      read-line(socket);
    end while;
  end if;
  
  let link = regex-search-strings(compile-regex(concatenate("<a(.*) href=\"", target,"\"(.*)>")), content);
  unless (link)
    xml-rpc-fault(17, "The source URI does not contain a link to the target URI, and so cannot be used as a source.");
  end unless;

  let (title, title-value) = regex-search-strings(compile-regex("<title>((.|\n)*)</title>"), content);
  unless (title-value)
    xml-rpc-fault(0, "The source URI does not contain a title.");
  end unless;

  let link-position = substring-position(content, link);

//  let text-end = link-position + size(link);
  let text = copy-sequence(content, 
    start: link-position - 100,
    end: link-position + size(link) + 100);

  let plain-text = regex-replace(text, compile-regex("<[a-zA-Z\\/][^>]*>"), "");
  plain-text := regex-replace(plain-text, compile-regex("(.*)>"), "");
  plain-text := regex-replace(plain-text, compile-regex("<(.*)"), "");
  plain-text := concatenate("[...] ", plain-text," [...]");

  let pingback = make(<blog-pingback>, source: source,
    title: title-value, content: make(<xhtml-content>, content: plain-text));

  add-pingback(entry, pingback);
  concatenate("Pingback from ", source," to ", target," registered. Thanks! ;)");
end;

define method entry-from-url (url :: <string>)
 => (entry :: false-or(<entry>));
  let url = parse-uri(url, as: <url>);
  if (~empty?(url.uri-path))
    let (blogs-string, blog, archive-string, year, month, day, title) =
      apply(values, url.uri-path);
    if (title)
      let (year, month, day) = validate-dates(year, month, day);

      blog := find-blog(*blog-server*, blog);
      if (blog)
        let date = make(<date>, year: year, month: month, day: day);
        let entry = find-entry(blog, title, date);
        unless (entry)
          entry := find-entry(blog, decode(title), date); 
        end unless;
        entry;
      end if;
    end if;
  end if;
end;

define method do-pingbacks (entry :: <blog-entry>)
  let source = build-uri(permanent-link(entry, full?: #t, escaped?: #t));
  let content = entry.content.content;
  let regex = compile-regex("<a([^>]+)href=\"([^>\"]+)\"([^>]*)>");
  let start = 0;
  while (regex-position(regex, content, start: start))
    let (#rest matches) = regex-search-strings(regex, copy-sequence(content, start: start));
    if (first(matches))
      discover-pingback(source, third(matches));
    end if;
    let (#rest positions) = regex-position(regex, content, start: start);    
    start := last(positions) | size(content);
  end while;
end;


define method discover-pingback (source :: <string>, target :: <string>)
  let url = parse-uri(target, as: <uri>);
  if (~empty?(url.uri-host))
    let response = read(http-request(target), 5*1024);
    let (header, pingback-url) = 
      regex-search-strings(compile-regex("X-Pingback: (.*)\r"), response);
    unless (pingback-url)
      let (link, pingback-url) = 
        regex-search-strings(compile-regex("<link rel=\"pingback\" href=\"([^\"]+)\" ?/?>"), response);
    end;
    if (pingback-url)
      let url = parse-uri(pingback-url, as: <url>);
      if (~empty?(url.uri-host))
        let response = xml-rpc-call-2(url.uri-host, url.uri-port, build-path(url),
          "pingback.ping", source, target);
      end if;
    end if;
  end if;
end;

define tag show-pingback-pingback-number in turboblog (page :: <turboblog-page>)
 ()
  if (*pingback*)
    output("%s", *pingback*.pingback-number);
  end if;
end;

define tag show-pingback-pingback-source in turboblog (page :: <turboblog-page>)
 ()
  if (*pingback*)
    output("%s", *pingback*.pingback-source);
  end if;
end;

define tag show-pingback-title in turboblog (page :: <turboblog-page>)
 ()
  if (*pingback*)
    output("%s", *pingback*.title);
  end if;
end;

define tag show-pingback-body in turboblog (page :: <turboblog-page>)
 ()
  if (*pingback* & *pingback*.content)
    output("%s", *pingback*.content.content);
  end if;
end;

define tag show-pingback-published in turboblog (page :: <turboblog-page>)
 (formatted :: <string>)
  if (*pingback*)
    output("%s", format-date(formatted, *pingback*.published));
  end if;
end;

