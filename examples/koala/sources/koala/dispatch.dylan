Module:    httpi
Synopsis:  Given a URL, dispatch to the appropriate responder
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


/*
define function match-directories (pdir :: <list>, udir :: <list>)
  => (matches :: <list>)
  iterate match (pdir = pdir, udir = udir, matches = #())
    if (empty?(pdir))
      empty?(udir) & matches
    elseif (~empty?(udir))
      let p = pdir.head;
      let u = udir.head;
      if (p == #"wild-inferiors")
        let pdir = pdir.tail;
        if (empty?(pdir))
          pair(udir, matches)
        else
          let matches = pair(#(), matches);
          iterate match1 (udir = udir)
             ~empty?(udir) &
             (match(pdir, udir, matches) |
               begin
                 matches.head := concatenate!(matches.head, list(udir.head));
                 match1(udir.tail)
               end);
          end;
        end;
      elseif (p == #"wild")
        match(pdir.tail, udir.tail, pair(u, matches))
      elseif (pathname=(p, u))
        match(pdir.tail, udir.tail, matches)
      else
        #f
      end;

define function match-translation (pattern :: <url>, url :: <url>)
  let pdir = pattern.url-directory;
  let udir = url.url-directory;
        
define function canonicalize-url (client :: <client>, url :: <url>)
  let server = client.client-server;
  for (translation in server.server-pathname-translations)
    let translated = match-translation(url, translation.head, translation.tail);
  end;
end;

define function page-for-request (client :: <client>)
    => (false-or-page)
  let full-url = client.client-request-url;
  let server = client.client-server;
  let (full-url, bpos, epos) = string-extent(full-url);
  let qpos = char-position('?', full-url, bpos, epos);
  let url-string = if (qpos) substring(full-url, bpos, qpos) else full-url end;
  //---TODO: should record last reference time, so can gc the page table
  // as needed.
  let page = element(*page-table*, url-string, default: #f) |
               begin
                 let url = parse-url(client, url);
                 let can-url = canonicalize-url(client, url-string);
                 let page = can-url & element(*page-table*, can-url, default: #f);
                 when (page)
                   element(*page-table*, url-string) := page;
                 end
               end;
  page
end;
*/
