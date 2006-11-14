module: test
synopsis: 
author: 
copyright: 

define function main(name, arguments)
	with-cgi-header()
	end;
	
	let name :: <string> = $remote-user | "world";
	let host :: <string> = $server-name | "the server";
	cgi-format-out("<html><head><title>Dylan CGI</title></head><body>Hello %s from %s!</body></html>", name, host );
	
  exit-application(0);
end function main;

// Invoke our main() function.
main(application-name(), application-arguments());
