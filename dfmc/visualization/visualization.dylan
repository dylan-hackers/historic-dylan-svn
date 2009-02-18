module: dfmc-visualization
author: Hannes Mehnert
copyright: 2009, all rights reversed
synopsis: Dylan side of graphical visualization of DFM control flow graphs

define constant $default-host = "127.0.0.1";
define constant $default-port = 1234;

define class <dfmc-graph-visualization> (<object>)
  slot socket :: false-or(<socket>) = #f;
  constant slot connection-id :: <symbol>, required-init-keyword: id:;
  slot report-enabled? :: <boolean> = #t;
  slot dfm-report-enabled? :: <boolean> = #t;
  slot dfm-index :: <integer> = -1;
end;

define function write-to-visualizer (v :: <dfmc-graph-visualization>, data)
  if (v.report-enabled? & v.dfm-report-enabled?)
    let newstream = make(<string-stream>, direction: #"output");
    print-s-expression(newstream, data);
    let s-expression = stream-contents(newstream);
    //format(*standard-error*, "write: %s\n", s-expression);
    let siz = integer-to-string(s-expression.size, base: 16, size: 6);
    block()
      format(v.socket, "%s%s", siz, s-expression);
      force-output(v.socket);
      let res = read-from-visualizer(v);
      unless (res = #(#"ok"))
        format(*standard-output*, "expected ok, but got %=\n", res);
      end;
    exception (c :: <condition>)
      format(*standard-output*, "failed communication: %=\n", c);
    end;
  end;
end;

define function read-from-visualizer (v :: <dfmc-graph-visualization>) => (result)
  let stream = v.socket;
  let length = string-to-integer(read(stream, 6), base: 16);
  let line = read(stream, length);
  //format(*standard-output*, "read: %s\n", line);
  let expr = read-lisp(make(<string-stream>, direction: #"input", contents: line));
  //format(*standard-output*, "parsed: %=\n", expr);
  expr;
end;

define function connect-to-server
 (v :: <dfmc-graph-visualization>, #key host, port)
  unless (v.socket)
    v.socket := make(<tcp-socket>,
                     host: host | $default-host,
                     port: port | $default-port);
    write-to-visualizer(v, list(#"connection-identifier", v.connection-id))
  end;
end;
 
begin
  start-sockets();
end;

