module: network

define method tcp-client-connection (hostname :: <byte-string>,
                                     port :: <integer>)
 => (socket-input :: <fd-stream>, socket-output :: <fd-stream>);

  let host = gethostbyname(hostname);
  if(host = as(<hostent>, 0))
    error("Name lookup failed");
  end if;

  let server-address = make(<sockaddr-in>);
  server-address.get-sin-family := host.get-h-addrtype;
  for(i from 0 below host.get-h-length)
    server-address.get-sa-data[i + 2] // yuck
      := host.get-h-addr-list[0][i];  // choose the first
  end for;
  server-address.get-sin-port := htons(port);

  let foo = socket($PF-INET, $SOCK-STREAM, $IPPROTO-TCP);
  if(foo == -1)
    error("socket() failed");
  end if;

  let rc = connect(foo, server-address, <sockaddr-in>.content-size);
  if(foo == -1)
    error("connect() failed");
  end if;

  let input-stream = make(<fd-stream>, fd: foo, direction: #"input");
  let output-stream = make(<fd-stream>, fd: foo, direction: #"output");
  values(input-stream, output-stream);
end method tcp-client-connection;

/*
  let poll-list = make(<pollfd>, element-count: 4);
  map(get-fd-setter, list(foo, foo,
                       *standard-input*.file-descriptor,
                       *standard-output*.file-descriptor),
      poll-list);
  
  map(get-events-setter, 
      list($POLLIN, $POLLOUT, $POLLIN, $POLLOUT), 
      poll-list);
  
//  format(output-stream, "%s\n", request);
//  force-output(output-stream);
  let running = #t;
  while(running)
    let rc = poll(poll-list, 4, 1000);
//    for(i from 0 below 4)
//      format(*standard-error*, "revents[%=] = %=  ", i, poll-list[i].get-revents);
//    end for;
//    format(*standard-error*, "\n");
    if(logand(poll-list[0].get-revents, $POLLIN) > 0)
      format(*standard-output*, "%s\n", read-line(input-stream));
      force-output(*standard-output*);
    end if;
    if(logand(poll-list[2].get-revents, $POLLIN) > 0)
      format(output-stream, "%s\n", read-line(*standard-input*));
      force-output(output-stream);
    end if;
  end while;
end;

define method as(class == <double-float>, tv :: <timeval>) 
 => (d :: <double-float>);
  as(<double-float>, tv.get-tv-sec) +
    as(<double-float>, tv.get-tv-usec) / 1000000.0;
end method as;

define method as(class == <timeval>, d :: <double-float>)
 => (tv :: <timeval>);
  if(d < 0.0)
    error("Cannot convert negative time value to <timeval>");
  end if;
  let tv = make(<timeval>);
  let (seconds, rest) = floor(d);
  tv.get-tv-sec := seconds;
  tv.get-tv-usec := floor/(rest, 1000000.0);
  tv;
end method as;

define method now()
  let tv = make(<timeval>);
  gettimeofday(tv, as(<timezone>, 0));
  as(<double-float>, tv);
end method now;

define method print-object(tv :: <timeval>, s :: <stream>) => ()
  format(s, "{<timeval>: %=.%= sec}", tv.get-tv-sec, tv.get-tv-usec);
end method;

*/
