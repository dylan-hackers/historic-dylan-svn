module: cgi
synopsis: CGI library for Gwydion Dylan
author: Rob Myers
copyright: Copyright ( c ) 2003 GwydionDylan Maintainers
license: lgpl

// Output stream

define variable *cgi-output-stream* :: <stream> = *standard-output*;

// Environment Variables

// Static

define constant $gateway-interface :: false-or( <string> ) = environment-variable( "GATEWAY_INTERFACE" );
define constant $server-name :: false-or( <string> ) = environment-variable( "SERVER_NAME" );
define constant $server-software :: false-or( <string> ) = environment-variable( "SERVER_SOFTWARE" );

// Per request

define constant $auth-type :: false-or( <string> ) = environment-variable( "AUTH_TYPE" );
define constant $content-length :: false-or( <integer> ) = maybe-integer( environment-variable( "CONTENT_LENGTH" ) );
define constant $content-type :: false-or( <string> ) = environment-variable( "CONTENT_TYPE" );
define constant $http-referer :: false-or( <string> ) = environment-variable( "HTTP_REFERER" );
define constant $path-info :: false-or( <string> ) = environment-variable( "PATH_INFO" );
define constant $path-translated :: false-or( <string> ) = environment-variable( "PATH_TRANSLATED" );
define constant $query-string :: false-or( <string> ) = environment-variable( "QUERY_STRING" );
define constant $remote-address :: false-or( <string> ) = environment-variable( "REMOTE_ADDR" );
define constant $remote-host :: false-or( <string> ) = environment-variable( "REMOTE_HOST" );
define constant $remote-ident :: false-or( <string> ) = environment-variable( "REMOTE_IDENT" );
define constant $remote-user :: false-or( <string> ) = environment-variable( "REMOTE_USER" );
define constant $request-method :: false-or( <string> ) = environment-variable( "REQUEST_METHOD" );
define constant $script-name :: false-or( <string> ) = environment-variable( "SCRIPT_NAME" );
define constant $server-port :: false-or( <integer> ) = maybe-integer(  environment-variable( "SERVER_PORT" ) );
define constant $server-protocol :: false-or( <string> ) = environment-variable( "SERVER_PROTOCOL" );

// Header lines for the request, if any

define constant $http-accept :: false-or( <string> ) = environment-variable( "HTTP_ACCEPT" );
define constant $http-user-agent :: false-or( <string> ) = environment-variable( "HTTP_USER_AGENT" );


// Writing Headers

define constant $line-ending :: <string> = "\n";

define method write-header( header :: <string>, #key stream: stream = *cgi-output-stream* )
=> ()
	write( stream, header );
	write( stream, $line-ending );
end method write-header;	
	
define method write-content-type-header( major :: <string>, minor :: <string>, #key stream: stream = *cgi-output-stream* )
=> ()
			format( stream, "Content-type: %s/%s%s", major, minor, $line-ending );
end method write-content-type-header;

define method write-html-content-type-header( #key stream: stream = *cgi-output-stream* )
=> ()
			write-content-type-header( "text", "html", stream: stream );
end method write-html-content-type-header;

define method write-text-content-type-header( #key stream: stream = *cgi-output-stream* )
=> ()
			write-content-type-header( "text", "plain", stream: stream );
end method write-text-content-type-header;

define method write-location-header( location :: <string>, #key stream: stream = *cgi-output-stream* )
=> ()
			format( stream, "Location: %s", location, $line-ending );
end method write-location-header;

define method write-status-header( code :: <integer>, reason :: <string>, #key stream: stream = *cgi-output-stream* )
=> ()
			format( stream, "%d %s%s", code, reason, $line-ending );
end method write-status-header;
	
define method write-end-header( #key stream: stream = *cgi-output-stream*  )
=> ()
			format( stream, $line-ending );
end method write-end-header;


// Writing

define method cgi-write( string :: <string> ) 
=> ()
	write( *cgi-output-stream*, string );
end method cgi-write;

define method cgi-format-out( string :: <string>, #rest rest ) 
=> ()
	apply( curry( format, *cgi-output-stream*, string ), rest );
end method cgi-format-out;

// Macros

define macro with-cgi-stream
  { with-cgi-stream( ?:expression ) ?:body end }
=>{ let old-stream :: <stream> = *cgi-output-stream*;
			*cgi-output-stream* = ?expression;
			?body;
			*cgi-output-stream* = old-stream;	}
end macro with-cgi-stream;


define macro with-cgi-header
  { with-cgi-header() ?:body end }
=>{ write-html-content-type-header();
			?body;
			write-end-header();	}
  { with-header( ?major:expression, ?minor:expression ) ?:body end }
=>{ write-content-type-header( ?major, minor );
			?body;
			write-end-header();	}
end macro with-cgi-header;


// Utilities

define method maybe-integer( in :: false-or( <string> ) ) => ( out :: false-or( <integer> ) )
	if( in  )
		string-to-integer( in );
	else
		#f;
	end if;
end method maybe-integer;
