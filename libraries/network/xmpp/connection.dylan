module: xmpp
synopsis: 
author: 
copyright:

/*
define class <xmpp-host> (<host>)

end class <xmpp-host>;

define class <xmpp-connection> (<object>)
  slot socket :: <tcp-socket>,
    init-keyword: socket:;
  slot state :: <type> = #"disconnected",
    init-keyword: state:;
  slot jid :: <jid>,
    init-keyword: jid:;
end class <xmpp-connection>;

define method connect (connection :: <xmpp-connection>, host, port)
  let socket = make(<tcp-socket>, host: host, port: port);
end method connect;

*/
