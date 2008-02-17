module: dylan-user

define library uri
  use common-dylan;
  use collection-extensions;
  use io;
  use regular-expressions;
  export uri;
end library;

define module uri
  use common-dylan;
  use common-extensions;
  use vector-search;
  use subseq;
  use format;
  use format-out;
  use regular-expressions;
  use streams;
  export <uri>, <url>, 
    uri-scheme, uri-scheme-setter,
    uri-userinfo, uri-userinfo-setter,
    uri-host, uri-host-setter,
    uri-port, uri-port-setter,
    uri-path, uri-path-setter, 
    uri-query, uri-query-setter,
    uri-fragment, uri-fragment-setter,
    uri-authority, uri-authority-setter;
  export parse-uri, parse-url,
    build-uri, transform-uris, 
    build-path, build-query;
  export remove-dot-segments,
    split-path, split-query;
  export absolute?, relative?;
  export print-message;
end module;
