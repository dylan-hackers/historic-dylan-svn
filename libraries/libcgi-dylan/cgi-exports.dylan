module: dylan-user

define library cgi
	use common-dylan;
  use io;
	use system;

  export cgi;
end library;

define module cgi
  use common-dylan;
  use format-out;
	use streams;
	use standard-io;
	use operating-system;

  export 
		*cgi-output-stream*,
	
		$gateway-interface, $server-name, $server-software,

		$auth-type , $content-length, $content-type, $http-accept, $http-referer, 
		$http-user-agent, $path-info, $path-translated, $query-string, 
		$remote-address, $remote-host, $remote-ident, $remote-user, 
		$request-method, $script-name, $server-port, $server-protocol,

		$http-accept, $http-user-agent,
		
		write-header, write-content-type-header, write-html-content-type-header,
		write-text-content-type-header, write-location-header, 
		write-status-header, write-end-header,
		
		cgi-write, cgi-format-out,
		
		\with-cgi-stream, \with-cgi-header
	;
end module;
