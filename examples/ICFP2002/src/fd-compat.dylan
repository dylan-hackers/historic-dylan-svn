module: fd-compat

// Functional developer has a bug whereby read(...) does not work
// on sockets. Workaround here.
define function fd-read(stream :: <stream>, size :: <integer>)
  let temp = make(<byte-string>, size: size);
  block(return)
    for(n from 0 below size)
      let ch = read-element(stream, on-end-of-stream: #f);
      if(ch)
        temp[n] := ch;
      else
        return(temp)
      end if;
    end for;
    temp;
  end block;
end function fd-read;

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

