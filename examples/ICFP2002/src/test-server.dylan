module: test-server
synopsis: Our own game server for testing purposes
copyright: this program may be freely used by anyone, for any purpose

define function main(name, arguments)
  if(arguments.size < 1)
    format-out("Wrong number of arguments passed.\n");
    exit-application(1);
  end if;
  let server-socket = tcp-server-socket(string-to-integer(arguments[0]));

  let (input-stream, output-stream) 
    = tcp-server-accept(server-socket);
  let running = #t;
  while(running)
    let line = read-line(input-stream, on-end-of-stream: #f);
    if(line)
      write-line(output-stream, line);
      force-output(output-stream);
    else
      running := #f;
    end if;
  end while;
  close(input-stream);
  close(output-stream);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
