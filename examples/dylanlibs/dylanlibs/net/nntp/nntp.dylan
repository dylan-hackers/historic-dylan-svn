Module:    nntp
Synopsis:  NNTP Protocol implementation
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// The following NNTP code was based on NNTP dot Lisp, a Common Lisp
// NNTP library, obtained from http://www.davep.org/misc/#nntp.lisp.
//
// Dave Pearson has kindly given permission to allow me to provide this
// Dylan port of nntp.lisp in the dylanlibs collection. Thank you very
// much Dave! Visit Dave's website for some neat Common Lisp tools and
// other things: http://www.davep.org

// Host name to use when a default host is required
define constant $default-nntp-host :: <string> = "localhost";

// Port to use when a default port is required
define constant $default-nntp-port :: <integer> = 119;

// Support functions
//
// Is an article ID delimited with <> characters?
define function delimited-article-id?(id :: <string>) => (r :: <boolean>)
  let length = id.size;
  length > 1 &
    id[0] == '<' &
    id[length - 1] == '>';
end function delimited-article-id?;

// Ensure that an article ID is properly delmited
define function tidy-article-id (id :: <string>) => (r :: <string>)
  if (delimited-article-id?(id))
    id
  else
    format-to-string("<%s>", id)
  end if;
end function tidy-article-id;

// Class for holding responses from the server
define class <nntp-client-response> (<object>)
  slot code :: <integer>, init-keyword: code:;
  slot response :: <string>, init-keyword: response:;
  slot data :: <object> = #f, init-keyword: data:;
end class <nntp-client-response>;

// Take text response and create a populated instance of a <nntp-client-response>
define function make-nntp-client-response(text :: <string>) => (r :: <nntp-client-response>)
  make(<nntp-client-response>,
       code: string-to-integer(text, start: 0, end: 3),
       response: if(text.size > 3) copy-sequence(text, start: 4) else "" end if);
end function make-nntp-client-response;

// Is RESPONSE a temporary error?
define method temporary-error? (response :: <nntp-client-response>) => (r :: <boolean>)
  response.code >= 400 & response.code < 500
end method temporary-error?;

// Is RESPONSE a permanent error?
define method permanent-error?(response :: <nntp-client-response>) => (r :: <boolean>)
  response.code >= 500 & response.code < 600
end method permanent-error?;

// Is RESPONSE any kind of error?
define method error?(response :: <nntp-client-response>) => (r :: <boolean>)
  temporary-error?(response) | permanent-error?(response)
end method error?;

// Class for holding an article ID as returned from stat-like commands
define class <nntp-article-id> (<object>)
  slot article-number :: <integer>, required-init-keyword: number:;
  slot article-id :: <string>, required-init-keyword: id:;
end class <nntp-article-id>;

// Create an Article ID object from a "stat" result string
define function parse-stat-result (result :: <string>) => (r :: <nntp-article-id>)
  make(<nntp-article-id>,
       number: string-to-integer(result),
       id: copy-sequence(result,
                         start: sequence-position(result, '<'),
                         end: 1 + sequence-position(result, '>')));
end function parse-stat-result;

// Base class for group information
define class <nntp-group> (<object>)
  slot group-name :: <string>, required-init-keyword: group-name:;
  slot first-article :: <integer>, required-init-keyword: first-article:;
  slot last-article :: <integer>, required-init-keyword: last-article:;
end class <nntp-group>;

// Class for holding the information of a group
define class <nntp-group-info> (<nntp-group>)
  slot group-flags :: <string> = "", init-keyword: group-flags:;
end class <nntp-group-info>;

// Create a group information object from a "new-groups" result string.
define function parse-group-info(result :: <string>) => (r :: <nntp-group-info>)
  let result = split-string(result, " "); 
  make(<nntp-group-info>,
       group-name: result[0],
       first-article: string-to-integer(result[2]),
       last-article: string-to-integer(result[1]),
       group-flags: result[3])
end function parse-group-info;

// Class for holding the details of a group
define class <nntp-group-details> (<nntp-group>)
  slot article-count :: <integer>, required-init-keyword: article-count:;
end class <nntp-group-details>;

// Create a group detail object from a GROUP result string
define function parse-group-details (result :: <string>) => (r :: <nntp-group-details>)
  let result = split-string(result, " "); 
  make(<nntp-group-details>,
       article-count: string-to-integer(result[0]),
       first-article: string-to-integer(result[1]),
       last-article: string-to-integer(result[2]),
       group-name: result[3])
end function parse-group-details;

// Class for holding an xhdr result
define class <nntp-xhdr-details> (<object>)
  slot article-id, required-init-keyword: article-id:;
  slot header-value :: <string>, required-init-keyword: header-value:;
end class <nntp-xhdr-details>;

// Create a XHDR details object
define function parse-xhdr-details (result :: <string>) => (r :: <nntp-xhdr-details>)
  let (id, value) =
    if(result.size > 1 & result[0] == '<')
      let  id = copy-sequence(result, start: 0, end: (1 + sequence-position(result, '<')));
      values(id, copy-sequence(result, start: id.size + 1));
    else
      let (number, value-start) = string-to-integer(result);
      values(number, copy-sequence(result, start: value-start));
    end if;

  make(<nntp-xhdr-details>, article-id: id, header-value: value);
end function parse-xhdr-details;

// NNTP client class
define class <nntp-client> (<object>)
  slot nntp-host :: <string> = $default-nntp-host, init-keyword: host:;
  slot nntp-port :: <integer> = $default-nntp-port, init-keyword: port:;
  slot nntp-socket :: false-or(<tcp-socket>) = #f, init-keyword: socket:;
end class <nntp-client>;

// Create an NNTP client
define function make-nntp-client(#key host :: <string> = $default-nntp-host, port :: <integer> = $default-nntp-port)
 => (r :: <nntp-client>)
  make(<nntp-client>, host: host, port: port);
end function make-nntp-client;

// Is NNTP-CLIENT connected to an NNTP server?
define method connected? (nntp-client :: <nntp-client>) => (r :: <boolean>)
  nntp-client.nntp-socket ~= #f
end method connected?;

// Check that NNTP-CLIENT is connected to a server, error if not
define method connected-check (nntp-client :: <nntp-client>) => ()
  unless(connected?(nntp-client))
    error("Not connected to an NNTP server")
  end
end method connected-check;

// Connect the NNTP client to the NNTP server
define method nntp-connect (nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  when(connected?(nntp-client))
    error(format-to-string("Already connected to host %s on port %d",
                           nntp-client.nntp-host, nntp-client.nntp-port))
  end when;

  nntp-client.nntp-socket := make(<tcp-socket>, host: nntp-client.nntp-host, port: nntp-client.nntp-port);
  get-short-response(nntp-client);
end method nntp-connect;

// Read a line from the NNTP server
define method nntp-get-line (nntp-client :: <nntp-client>) => (r :: <string>)
  connected-check(nntp-client);

  let line = read-line(nntp-client.nntp-socket);
  let length = line.size;
  if (length > 0 & line.last == as(<character>, 13))
    copy-sequence(line, end: length - 1)
  else
    line
  end if;
end method nntp-get-line;

define method put-line(nntp-client :: <nntp-client>, line :: <string>) => ()
  connected-check(nntp-client);

  format(nntp-client.nntp-socket, "%s%c%c", line, as(<character>, 13), as(<character>, 10));
end method put-line;

// Get a short (one line) response from the NNTP server
define method get-short-response(nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  make-nntp-client-response(nntp-get-line(nntp-client));
end method get-short-response;

// Get a long (multi line) response from the NNTP server
define method get-long-response(nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  let response = get-short-response(nntp-client);
  unless(error?(response))
    let response-data = make(<stretchy-vector>);
    for(line = nntp-get-line(nntp-client) then nntp-get-line(nntp-client),
        until: line = ".")
      response-data := add!(response-data, line);
    end for;
    response.data := response-data;
  end unless;
  response;
end method get-long-response;

// Send COMMAND to server and get its single line response.
define method short-command (nntp-client :: <nntp-client>, command :: <string>) => (r :: <nntp-client-response>)
  put-line(nntp-client, command);
  get-short-response(nntp-client);
end method short-command;

// Send COMMAND to server and get its multi-line response.
define method long-command(nntp-client :: <nntp-client>, command :: <string>) => (r :: <nntp-client-response>)
  put-line(nntp-client, command);
  get-long-response(nntp-client);
end method long-command;

// Request help from the NNTP server.
define method nntp-server-help (nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  long-command(nntp-client, "help");
end method nntp-server-help;

// Perform a "mode reader".
define method nntp-mode (nntp-client :: <nntp-client>, mode :: <string>) => (r :: <nntp-client-response>)
  short-command(nntp-client, format-to-string("mode %s", mode));
end method nntp-mode;

// Get the newsgroup list from the NNTP server.
define method nntp-group-list (nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  let response = long-command(nntp-client, "list");
  unless(error?(response))
    response.data := map(parse-group-info, response.data);
  end unless;
  response;
end method nntp-group-list;

// Get the list of new groups added to the server since DATE-TIME.
// DATE-TIME should be a string in the format "YYMMDD HHMMSS [GMT]".
define method nntp-new-groups (nntp-client :: <nntp-client>, date-time :: <string>) => (r :: <nntp-client-response>)
  let response = long-command(nntp-client, format-to-string("newgroups %s", date-time));
  unless(error?(response))
    response.data := map(parse-group-info, response.data);
  end unless;
  response;
end method nntp-new-groups;

// Get the list of new groups added to the server since DATE-TIME.
// DATE-TIME should be a <date> value.
define method nntp-new-groups(nntp-client :: <nntp-client>, date-time :: <date>) => (r :: <nntp-client-response>)
  nntp-new-groups(nntp-client, 
    format-to-string("%s%s%s %s%s%s",
                     integer-to-string(remainder(date-time.date-year, 100), size: 2, fill: '0'),
                     integer-to-string(date-time.date-month, size: 2, fill: '0'),
                     integer-to-string(date-time.date-day, size: 2, fill: '0'),
                     integer-to-string(date-time.date-hours, size: 2, fill: '0'),
                     integer-to-string(date-time.date-minutes, size: 2, fill: '0'),
                     integer-to-string(date-time.date-seconds, size: 2, fill: '0')));
end method nntp-new-groups;

// Get the list of new articles in GROUP posted since DATE and TIME.
define method nntp-new-news(nntp-client :: <nntp-client>, group :: <string>, date :: <string>, time :: <string>) => (r :: <nntp-client-response>)
  long-command(nntp-client, format-to-string("newnews %s %s %s", group, date, time));
end method nntp-new-news;

// Send the group command
define method nntp-group(nntp-client :: <nntp-client>, group :: <string>) => (r :: <nntp-client-response>)
  let r = short-command(nntp-client, format-to-string("group %s", group));
  unless(error?(r))
    r.data := parse-group-details(r.response);
  end unless;
  r
end method nntp-group;

// Populate a RESPONSE from a stat command.
define method stat-populate(r :: <nntp-client-response>) => (r :: <nntp-client-response>)
  unless(error?(r))
    r.data := parse-stat-result(r.response)
  end unless;
  r;
end method stat-populate;

// Stat article by ID.
define method nntp-stat(nntp-client :: <nntp-client>, id :: <string>) => (r :: <nntp-client-response>)
  stat-populate(short-command(nntp-client, format-to-string("stat %s", tidy-article-id(id))));
end method nntp-stat;

// Start article by number
define method nntp-stat(nntp-client :: <nntp-client>, number :: <integer>) => (r :: <nntp-client-response>)
  stat-populate(short-command(nntp-client, format-to-string("stat %s", number)));
end method nntp-stat;

// Move to the next article
define method nntp-next(nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  stat-populate(short-command(nntp-client, "next"));
end method nntp-next;

// Move to the previous article
define method nntp-previous(nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  stat-populate(short-command(nntp-client, "last"));
end method nntp-previous;

// Return the header of the article by ID.
define method nntp-head (nntp-client :: <nntp-client>, id :: <string>) => (r :: <nntp-client-response>)
  long-command(nntp-client, format-to-string("head %s", tidy-article-id(id)));
end method nntp-head;

// Return the header of article by number
define method nntp-head(nntp-client :: <nntp-client>, number :: <integer>) => (r :: <nntp-client-response>)
  long-command(nntp-client, format-to-string("head %s", number));
end method nntp-head;

// Return the header of an article
define method nntp-head(nntp-client :: <nntp-client>, article :: <nntp-article-id>) => (r :: <nntp-client-response>)
  nntp-head(nntp-client, article-id(article));
end method nntp-head;

// Return the body of article by ID.
define method nntp-body(nntp-client :: <nntp-client>, id :: <string>) => (r :: <nntp-client-response>)
  long-command(nntp-client, format-to-string("body %s", tidy-article-id(id)));
end method nntp-body;

// Return the body of article by number
define method nntp-body(nntp-client :: <nntp-client>, number :: <integer>) => (r :: <nntp-client-response>)
  long-command(nntp-client, format-to-string("body %s", number));
end method nntp-body;

// Return the body of an article 
define method nntp-body(nntp-client :: <nntp-client>, article :: <nntp-article-id>) => (r :: <nntp-client-response>)
  nntp-body(nntp-client, article-id(article));
end method nntp-body;
                           
// Return the header and body of an article as a list of lines.
define method nntp-article (nntp-client :: <nntp-client>, id :: <string>) => (r :: <sequence>)
  concatenate(data(nntp-head(nntp-client, id)), list(""), data(nntp-body(nntp-client, id)));
end method nntp-article;

// Return the header and body of an article as a list of lines.
define method nntp-article (nntp-client :: <nntp-client>, number :: <integer>) => (r :: <sequence>)
  concatenate(data(nntp-head(nntp-client, number)), list(""), data(nntp-body(nntp-client, number)));
end method nntp-article;

// Return the header and body of an article as a list of lines.
define method nntp-article (nntp-client :: <nntp-client>, a :: <nntp-article-id>) => (r :: <sequence>)
  nntp-article(nntp-client, article-id(a));
end method nntp-article;

// Get header HEADER for a rance of articles.
define method nntp-xhdrs (nntp-client :: <nntp-client>, header :: <string>, from :: <integer>, to :: <integer>) => (r :: <nntp-client-response>)
  let response = long-command(nntp-client, format-to-string("xhdr %s %d %d", header, from, to));
  unless(error?(response))
    response.data := map(parse-xhdr-details, response.data);
  end unless;
  response;
end method nntp-xhdrs;

// Get header HEADER from article NUMBER
define method nntp-xhdr(nntp-client :: <nntp-client>, header :: <string>, number :: <integer>) => (r :: <nntp-client-response>)
  let result = nntp-xhdrs(nntp-client, header, number, number);
  unless(error?(result))
    result.data := first(result.data);
  end unless;
  result;
end method nntp-xhdr;

// Get header HEADER from article ID
define method nntp-xhdr(nntp-client :: <nntp-client>, header :: <string>, id :: <string>) => (r :: <nntp-client-response>)
  let response = long-command(nntp-client, format-to-string("xhdr %s %s", header, tidy-article-id(id)));  
  unless(error?(response))
    response.data := parse-xhdr-details(first(response.data));
  end unless;
  response;
end method nntp-xhdr;

// Get header HEADER of ARTICLE.
define method nntp-xhdr(nntp-client :: <nntp-client>, header :: <string>, article :: <nntp-article-id>) => (r :: <nntp-client-response>)
  nntp-xhdr(nntp-client, header, article-id(article));
end method nntp-xhdr;

// Quite the NNTP session
define method nntp-disconnect(nntp-client :: <nntp-client>) => (r :: <nntp-client-response>)
  let result = short-command(nntp-client, "quit");
  unless(error?(result))
    close(nntp-socket(nntp-client));
    nntp-client.nntp-socket := #f;
  end unless;
  result;
end method nntp-disconnect;

// Create an NNTP client called CLIENT and evaluate BODY.
define macro with-nntp-client
{ with-nntp-client(?client:name = (?host:expression, ?port:expression)) ?:body end }
  => { begin
         let ?client = make-nntp-client(host: ?host, port: ?port);
         nntp-connect(?client);
         block()
           ?body
         cleanup
           when(connected?(?client))
             nntp-disconnect(?client)
           end when;
         end block
       end;
  }
end macro with-nntp-client;
