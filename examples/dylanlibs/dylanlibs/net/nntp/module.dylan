Module:    dylan-user
Synopsis:  NNTP Protocol implementation
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module nntp
  use functional-dylan;
  use format;
  use streams;
  use sockets, export: { start-sockets };
  use date;
  use sequence-utilities;

  // Add binding exports here.
  export
    $default-nntp-host,
    $default-nntp-port,
    delimited-article-id?,
    tidy-article-id,
    <nntp-client-response>,
    code,
    code-setter,
    response,
    response-setter,
    data,
    data-setter,
    make-nntp-client-response,
    temporary-error?,
    permanent-error?,
    error?,
    <nntp-article-id>,
    article-number,
    article-number-setter,
    article-id,
    article-id-setter,
    parse-stat-result,
    <nntp-group>,
    group-name,
    group-name-setter,
    first-article,
    first-article-setter,
    last-article,
    last-article-setter,
    <nntp-group-info>,
    group-flags,
    group-flags-setter,
    parse-group-info,
    <nntp-group-details>,
    article-count,
    article-count-setter,
    parse-group-details,
    <nntp-xhdr-details>,
    header-value,
    header-value-setter,
    parse-xhdr-details,
    <nntp-client>,
    host,
    host-setter,
    port,
    port-setter,
    socket,
    socket-setter,
    make-nntp-client,
    connected?,
    connected-check,
    nntp-connect,
    get-line,
    put-line,
    get-short-response,
    get-long-response,
    short-command,
    long-command,
    nntp-server-help,
    nntp-mode,
    nntp-group-list,
    nntp-new-groups,
    nntp-new-news,
    nntp-group,
    nntp-stat,
    nntp-next,
    nntp-previous,
    nntp-head,
    nntp-body,
    nntp-article,
    nntp-xhdrs,
    nntp-xhdr,
    nntp-disconnect,
    \with-nntp-client;
end module nntp;
