module: turboblog
author: turbo24prg

define function validate-dates (year, month, day)
 => (year :: false-or(<integer>),
     month :: false-or(<integer>),
     day :: false-or(<integer>));
  block ()
    if (year)
      year := string-to-integer(year);
    end if;
  exception (<error>)
    year := #f;
  end;

  block ()
    if (month)
      month := string-to-integer(month);
    end if;
  exception (<error>)
    month := #f;
  end;

  block ()
    if (day)
      day := string-to-integer(day);
    end if;
  exception (<error>)
    day := #f;
  end;
  values(year, month, day)
end function; 

define function requested-url (#key escaped? :: <boolean>) 
  let url = make(<url>, scheme: "http", host: current-request().request-host,
    path: current-request().request-url, query: current-request().request-query-string);
  build-uri(url);
end;

define function id (title :: <string>, date :: <date>)
 => (res :: <string>)
  concatenate(format-date("%Y/%m/%d/", date), title)
end;

define function feed-link (blog :: <blog>, type :: <string>)
  let url = permanent-link(blog);
  url.uri-query := list(concatenate("feed=", type));
  build-uri(url);
end;

// content-type

define method process-request-content
 (content-type == #"application/atom+xml", request :: <request>,
  buffer :: <byte-string>, content-length :: <integer>)
 => (content :: <string>)
  request-content(request) := buffer
end;

// is-numeric?

define constant is-numeric? :: <function> = curry(every?, digit?);

// sort-table

define function sort-table (table :: <table>, getter :: <function>,
                              #key order :: false-or(<function>))
 => (entries :: <sequence>);
  sort(map-as(<vector>, method (element) element end, table),
    test: method(first, second)
      (order | \>)(getter(first), getter(second))
    end);
end;

// decode 

define generic decode (object :: <object>);

define method decode (string :: <byte-string>)
  decode-url(string, 0, size(string));
end;

define method decode (sequence :: <sequence>)
  map(decode, sequence);
end;

define method decode (element == #f) #f end;

// redirect

define method redirect-to (object :: <object>, #key)
  redirect-to(build-uri(permanent-link(object)));
end;

define method redirect-to (entry :: <blog-entry>, #key 
 edit?, comment :: false-or(<blog-comment>), comments?)
  let location = permanent-link(entry);
  case
    edit? => location.uri-query := #("edit");
    comments? => location.uri-fragment := "comments";
    comment => location.uri-fragment := concatenate("comment-", integer-to-string(comment.comment-number));
    otherwise => #f;
  end case;
  redirect-to(build-uri(location));
end;

define method redirect-to (page :: <view-files-page>, #key)
  let location = permanent-link(*blog*);
  location.uri-path := concatenate(location.uri-path, list("files"));
  redirect-to(build-uri(location));
end;

// permanent link

define method permanent-link (blog-server :: <blog-server>, #key escaped?)
 => (url :: <url>);
  *blog-server*.blog-server-url;
end;

define method permanent-link (user :: <blog-user>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: "/users/");
  last(location.uri-path) := user.username;
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (blog :: <blog>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: "/blogs/");
  last(location.uri-path) := blog.title;
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (participant :: <blog-participant>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: "/participants/");
  last(location.uri-path) := participant.participant-user.username;
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (link :: <blog-link>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: "links/");
  location.uri-path := concatenate(permanent-link(link.link-blog).uri-path, location.uri-path);
  last(location.uri-path) := link.title;
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (entry :: <blog-entry>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: concatenate("archive/", format-date("%Y/%m/%d/", entry.published)));
  location.uri-path := concatenate(permanent-link(entry.entry-blog).uri-path, location.uri-path);
  last(location.uri-path) := entry.title;
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (comment :: <blog-comment>, #key escaped?, full?)
 => (url :: <url>);
  let location = make(<url>, path: "comments/");
  location.uri-path := concatenate(permanent-link(comment.comment-entry).uri-path, location.uri-path);
  last(location.uri-path) := integer-to-string(comment.comment-number);
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

define method permanent-link (feed :: <blog-feed>,
 #key escaped?, full?, type :: <symbol> = #"atom")
 => (url :: <url>);
  let location = make(<url>, query: concatenate("feed=", as(<string>, type)));
  transform-uris(permanent-link(feed.feed-blog), location, as: <url>);
end;

// archive url

define method archive-url (month :: <sequence>, #key escaped?, full?)
 => (url :: <url>);
  let location  = make(<url>, path: concatenate("archive/", format-date("%Y/%m/", first(*month*).published)));
  location.uri-path := concatenate(permanent-link(first(*month*).entry-blog).uri-path, location.uri-path);
  transform-uris(*blog-server*.blog-server-url, location, as: <url>);
end;

// tags

define function extract-tags (tag-string :: <string>)
  => (tags :: <sequence>);
  remove-duplicates!(split(tag-string, separator: " "), test: \=);
end;

