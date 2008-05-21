module: tcp-command-server

define variable *working-directory* = #f;

define function main(name, arguments)
  start-sockets();
  if (arguments.size == 1)
    *working-directory* := as(<directory-locator>, arguments[0]);
  else
    format-out("Usage: %s working-directory\n", name);
    exit-application(-1);
  end;
  let lsocket = make(<server-socket>, port: 1234);
  while(#t)
    let socket = accept(lsocket);
    make(<thread>, function: curry(reply-to-request, socket));
  end
end function main;

//code from koala, static-file-responder
define method locator-below-root?
    (locator :: <physical-locator>, root :: <directory-locator>)
 => (below? :: <boolean>)
  let relative = relative-locator(locator, root);
  // do they at least share a common ancestor?
  if (locator-relative?(relative))
    let relative-parent = locator-directory(relative);
    // is it a file directly in the root dir?
    ~relative-parent | begin
      let relative-path = locator-path(relative-parent);
      // again, is it directly in the root dir?
      empty?(relative-path) | 
        relative-path[0] ~= #"parent"  // does it start with ".."?
    end;
  end if;
end method locator-below-root?;

define function duration-to-string (duration :: <day/time-duration>) => (res :: <string>)
  let (days, hours, minutes, seconds) = decode-duration(duration);
  if (days > 0)
    format-to-string("%dd %dh %dm %ds",
		     days, hours, minutes, seconds);
  elseif (hours > 0)
    format-to-string("%dh %dm %ds",
		     hours, minutes, seconds);
  elseif (minutes > 0)
    format-to-string("%dm %ds", minutes, seconds);
  else
    format-to-string("%ds", seconds);
  end;
end;

define function reply-to-request (socket :: <socket>)
  block(ret)
    let now = current-date();
    format-out("%s host %s ",
	       format-date("%d/%b/%Y:%T %z", now),
	       as(<string>, host-address(remote-host(socket))));

    let command =
      block ()
	let line = read-line(socket);
	close(socket);
	line;
      exception (e :: <error>)
	format-out("communication error %=\n", e);
	ret();
      end;
    
    format-out("\"%s\" ", command);
    let exec = as(<file-locator>,
		  concatenate(as(<string>, *working-directory*), command));
    if (file-exists?(exec)
	  & locator-below-root?(exec, *working-directory*))
      let exit = run-application(as(<string>, exec));
      format-out("succeeded with %= in %s\n",
		 exit, duration-to-string(current-date() - now));
    else
      format-out("unknown command\n");
    end;
  exception (e :: <error>)
    format-out("got exception %=\n", e);
  end
end;

main(application-name(), application-arguments());
