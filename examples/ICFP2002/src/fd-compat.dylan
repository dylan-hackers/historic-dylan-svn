module: fd-compat

define method split(delim, str)
  split-string(str, delim);
end;

define variable *sockets-started?* = #f;

define method tcp-client-connection( hostname, port )
  unless(*sockets-started?*)
    start-sockets();
    *sockets-started?* := #t;
  end;
  let socket = make(<tcp-socket>, host: hostname, port: port);
  values(socket, socket);
end;

define method digit?(ch) 
  ch == '0' |
  ch == '1' |
  ch == '2' |
  ch == '3' |
  ch == '4' |
  ch == '5' |
  ch == '6' |
  ch == '7' |
  ch == '8' |
  ch == '9';
end;

