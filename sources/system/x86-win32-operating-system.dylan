Module:       system-internals
Author:       Jonathan Bachrach, Gary Palter, Peter Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
              Additional changes Copyright 2009 Dylan Hackers
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $architecture-little-endian? :: <boolean> = #t;

define constant $machine-name 	= #"x86";
define constant $os-name 	= #"win32";

define constant $DWORD_SIZE = raw-as-integer(primitive-word-size());

define constant $OSVERSIONINFO-SIZE = 5 * $DWORD_SIZE + 128;

define constant $VER_PLATFORM_WIN32s        = 0;
define constant $VER_PLATFORM_WIN32_WINDOWS = 1;
define constant $VER_PLATFORM_WIN32_NT      = 2;

define constant $command-line-option-prefix = '/';

define macro with-stack-dword
  { with-stack-dword (?dword:name) ?:body end }
  => { begin
         let ?dword = primitive-wrap-machine-word(integer-as-raw(0));
	 block ()
	   ?dword
             := primitive-wrap-machine-word
                  (primitive-cast-pointer-as-raw
                     (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
                           (flags :: <raw-c-unsigned-int>,
                            bytes :: <raw-c-unsigned-int>)
                        => (pointer :: <raw-c-pointer>)
                        (integer-as-raw(0), integer-as-raw($DWORD_SIZE))
                      end));
	   if (primitive-machine-word-equal?
                 (primitive-unwrap-machine-word(?dword), integer-as-raw(0)))
	     error("Can't allocate space for a DWORD")
	   end;
	   ?body
	 cleanup
	   if (primitive-machine-word-not-equal?
                 (primitive-unwrap-machine-word(?dword), integer-as-raw(0)))
	     %call-c-function ("LocalFree", c-modifiers: "__stdcall")
               (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
	       (primitive-cast-raw-as-pointer
                  (primitive-unwrap-machine-word(?dword)))
	     end
	   end
         end
       end }
end macro with-stack-dword;

define constant $osversioninfo
  = method ()
      let buffer :: <byte-string> = make(<byte-string>, size: $OSVERSIONINFO-SIZE,
					                fill: '\0');
      primitive-c-unsigned-long-at
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)),
	   integer-as-raw(0), integer-as-raw(0))
	:= integer-as-raw(size(buffer));
      %call-c-function ("GetVersionExA", c-modifiers: "__stdcall")
  	  (lpOSVersionInfo :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
	(primitive-cast-raw-as-pointer(primitive-string-as-raw(buffer)))
      end;
      buffer
    end
    ();

define inline-only function os-platform () => (platform :: <integer>)
  raw-as-integer(primitive-c-unsigned-long-at
		   (primitive-cast-raw-as-pointer
		      (primitive-string-as-raw($osversioninfo)),
		    integer-as-raw(4), integer-as-raw(0)))
end function os-platform;

define constant $os-variant 
  = method ()
      select (os-platform())
	$VER_PLATFORM_WIN32s => #"win3.1";
	$VER_PLATFORM_WIN32_WINDOWS =>
	  begin
	    let minorversion
	      = raw-as-integer(primitive-c-unsigned-long-at
				 (primitive-cast-raw-as-pointer
				    (primitive-string-as-raw($osversioninfo)),
				  integer-as-raw(2), integer-as-raw(0)));
	    if (minorversion = 0)
	      #"win95"
	    elseif (minorversion = 10)
	      #"win98"
            else /* if (minorversion = 90) */
              #"winme"
	    end
	  end;
	$VER_PLATFORM_WIN32_NT =>
	  begin
	    let majorversion
	      = raw-as-integer(primitive-c-unsigned-long-at
				 (primitive-cast-raw-as-pointer
				    (primitive-string-as-raw($osversioninfo)),
				  integer-as-raw(1), integer-as-raw(0)));
	    let minorversion
	      = raw-as-integer(primitive-c-unsigned-long-at
				 (primitive-cast-raw-as-pointer
				    (primitive-string-as-raw($osversioninfo)),
				  integer-as-raw(2), integer-as-raw(0)));
            if (majorversion < 5)
              #"winnt";
            elseif (minorversion = 0)
              #"win2000"
            else /* if (minorversion = 1) */
              #"winxp"
            end
          end
      end
    end
    ();

define constant $os-version
  = method ()
      let majorversion
	= raw-as-integer(primitive-c-unsigned-long-at
			   (primitive-cast-raw-as-pointer
			      (primitive-string-as-raw($osversioninfo)),
			    integer-as-raw(1), integer-as-raw(0)));
      let minorversion
	= raw-as-integer(primitive-c-unsigned-long-at
			   (primitive-cast-raw-as-pointer
			      (primitive-string-as-raw($osversioninfo)),
			    integer-as-raw(2), integer-as-raw(0)));
      let buildnumber
	= raw-as-integer(primitive-c-unsigned-long-at
			   (primitive-cast-raw-as-pointer
			      (primitive-string-as-raw($osversioninfo)),
			    integer-as-raw(3), integer-as-raw(0)));
      if (os-platform() == $VER_PLATFORM_WIN32_WINDOWS)
        buildnumber := logand(buildnumber, #xFFFF)
      end;
      let additionalinfo
	= begin
	    let buffer = make(<stretchy-vector>);
	    block (return)
	      for (i :: <integer> from 0 below 128)
		let c
		  = raw-as-integer(primitive-c-unsigned-char-at
				     (primitive-cast-raw-as-pointer
					(primitive-string-as-raw($osversioninfo)),
				      integer-as-raw(i),
				      integer-as-raw(5 * $DWORD_SIZE)));
		if (c = 0)
		  return (as(<string>, buffer))
		else
		  add!(buffer, as(<character>, c))
		end
	      end;
	      as(<string>, buffer)
	    end
	  end;
      let version = concatenate-as(<byte-string>,
				   integer-to-string(majorversion), ".",
				   integer-to-string(minorversion), ".",
				   integer-to-string(buildnumber));
      if (size(additionalinfo) > 0)
	concatenate-as(<byte-string>, version, " ", additionalinfo)
      else
	version
      end
    end
    ();

define constant $UNLEN = 256;		// Maximum username length

define function command-line-option-prefix
    () => (prefix :: <character>)
  $command-line-option-prefix
end function command-line-option-prefix;

define function login-name () => (name :: false-or(<string>))
  let buffer :: <byte-string> = make(<byte-string>, size: $UNLEN + 1, fill: '\0');
  let length :: <byte-string> = make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
  primitive-c-unsigned-long-at(primitive-cast-raw-as-pointer(primitive-string-as-raw(length)),
			       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw($UNLEN + 1);
  if (primitive-raw-as-boolean(%call-c-function ("GetUserNameA", c-modifiers: "__stdcall")
				   (lpBuffer :: <raw-c-pointer>, nSize :: <raw-c-pointer>)
				=> (success? :: <raw-c-signed-int>)
				 (primitive-cast-raw-as-pointer
				    (primitive-string-as-raw(buffer)),
				  primitive-cast-raw-as-pointer
				    (primitive-string-as-raw(length)))
			       end))
    let length
      = raw-as-integer(primitive-c-unsigned-long-at
			 (primitive-cast-raw-as-pointer(primitive-string-as-raw(length)),
			  integer-as-raw(0), integer-as-raw(0)));
    copy-sequence(buffer, end: length - 1)
  else
    #f
  end
end function login-name;

define function login-group () => (group :: false-or(<string>))
  if (os-platform() == $VER_PLATFORM_WIN32_NT)
    let name = login-name();
    if (name)
      let sid-buffer :: <byte-string> = make(<byte-string>, size: 1024, fill: '\0');
      let sid-buffer-length :: <byte-string>
	= make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
      let sid-use :: <byte-string> = make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
      let domain-name :: <byte-string> = make(<byte-string>, size: 1024, fill: '\0');
      let domain-name-length :: <byte-string>
	= make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
      primitive-c-unsigned-long-at
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(sid-buffer-length)),
	   integer-as-raw(0), integer-as-raw(0))
	:= integer-as-raw(size(sid-buffer));
      primitive-c-unsigned-long-at
	  (primitive-cast-raw-as-pointer(primitive-string-as-raw(domain-name-length)),
	   integer-as-raw(0), integer-as-raw(0))
	:= integer-as-raw(size(domain-name));
      if (primitive-raw-as-boolean
	    (%call-c-function ("LookupAccountNameA", c-modifiers: "__stdcall")
	         (lpSystemName :: <raw-byte-string>,
		  lpAccountName :: <raw-byte-string>,
		  Sid :: <raw-c-pointer>,
		  cbSid :: <raw-c-pointer>,
		  ReferencedDomainName :: <raw-byte-string>,
		  cbReferencedDomainName :: <raw-c-pointer>,
		  peUse :: <raw-c-pointer>)
	      => (success? :: <raw-c-signed-int>)
	       (primitive-cast-raw-as-pointer(integer-as-raw(0)),
		primitive-string-as-raw(name),
		primitive-cast-raw-as-pointer(primitive-string-as-raw(sid-buffer)),
		primitive-cast-raw-as-pointer(primitive-string-as-raw(sid-buffer-length)),
		primitive-string-as-raw(domain-name),
		primitive-cast-raw-as-pointer(primitive-string-as-raw(domain-name-length)),
		primitive-cast-raw-as-pointer(primitive-string-as-raw(sid-use)))
	    end))
	let domain-name-length 
	  = raw-as-integer
	      (primitive-c-unsigned-long-at
		 (primitive-cast-raw-as-pointer(primitive-string-as-raw(domain-name-length)),
		  integer-as-raw(0), integer-as-raw(0)));
	if (~zero?(domain-name-length))
	  copy-sequence(domain-name, end: domain-name-length)
	else
	  #f
	end
      else
	#f
      end
    else
      #f
    end
  else
    //---*** Always return #f until we can find an API (or a registry entry)
    //---*** that returns the user's workgroup/domain under Windows 95/98.
    #f
  end
end function login-group;

define constant $HKEY_LOCAL_MACHINE :: <machine-word> = as(<machine-word>, #x80000002);
define constant $KEY_QUERY_VALUE = 1;

define constant $ERROR_SUCCESS   = 0;
define constant $ERROR_HANDLE_EOF = 38;
define constant $ERROR_BROKEN_PIPE = 109;

define inline-only function current-version-key (name :: <byte-string>)
 => (value :: false-or(<string>))
  block (return)
    local method doit (key :: <machine-word>, subKey :: <byte-string>, f :: <function>) => ()
	    let handle :: <byte-string> = make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
	    let valid? :: <boolean> = #f;
	    block ()
	      let status
		= raw-as-integer(%call-c-function ("RegOpenKeyExA", c-modifiers: "__stdcall")
				     (hKey :: <raw-c-pointer>,
				      lpSubKey :: <raw-byte-string>,
				      ulOptions :: <raw-c-unsigned-long>,
				      samDesired :: <raw-c-unsigned-long>,
				      phkResult :: <raw-c-pointer>)
				  => (success? :: <raw-c-signed-long>)
				   (primitive-cast-raw-as-pointer
				      (primitive-unwrap-machine-word(key)),
				    primitive-string-as-raw(subKey),
				    integer-as-raw(0),
				    integer-as-raw($KEY_QUERY_VALUE),
				    primitive-cast-raw-as-pointer
				      (primitive-string-as-raw(handle)))
				 end);
	      if (status = $ERROR_SUCCESS)
		valid? := #t;
		f(primitive-wrap-machine-word
		    (primitive-c-unsigned-long-at
		       (primitive-cast-raw-as-pointer(primitive-string-as-raw(handle)),
			integer-as-raw(0), integer-as-raw(0))))
	      else
		return(#f)
	      end;
	    cleanup
	      if (valid?)
		%call-c-function ("RegCloseKey", c-modifiers: "__stdcall")
		    (hKey :: <raw-c-pointer>) => (success? :: <raw-c-signed-long>)
		  (primitive-cast-raw-as-pointer
		     (primitive-c-unsigned-long-at
			(primitive-cast-raw-as-pointer(primitive-string-as-raw(handle)),
			 integer-as-raw(0), integer-as-raw(0))))
                end
              end
            end
	  end method doit;
    doit($HKEY_LOCAL_MACHINE,
	 "Software",
	 method (handle :: <machine-word>) => ()
	   doit(handle,
		"Microsoft",
		method (handle :: <machine-word>) => ()
		  doit(handle,
		       if (os-platform() == $VER_PLATFORM_WIN32_NT)
			 "Windows NT"
		       else
			 "Windows"
		       end,
		       method (handle :: <machine-word>) => ()
			 doit(handle,
			      "CurrentVersion",
			      method (handle :: <machine-word>) => ()
				let type-buffer :: <byte-string>
				  = make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
				let buffer-size-buffer :: <byte-string>
				  = make(<byte-string>, size: $DWORD_SIZE, fill: '\0');
				let status
				  = raw-as-integer
				      (%call-c-function ("RegQueryValueExA",
							 c-modifiers: "__stdcall")
					   (hKey :: <raw-c-pointer>,
					    lpValueName :: <raw-byte-string>, 
					    lpReserved :: <raw-c-pointer>,
					    lpType :: <raw-c-pointer>,
					    lpData :: <raw-c-pointer>,
					    lpcbData :: <raw-c-pointer>)
					=> (success? :: <raw-c-signed-long>)
					 (primitive-cast-raw-as-pointer
					    (primitive-unwrap-machine-word(handle)),
					  primitive-string-as-raw(name),
					  primitive-cast-raw-as-pointer(integer-as-raw(0)),
					  primitive-cast-raw-as-pointer
					    (primitive-string-as-raw(type-buffer)),
					  primitive-cast-raw-as-pointer(integer-as-raw(0)),
					  primitive-cast-raw-as-pointer
					    (primitive-string-as-raw(buffer-size-buffer)))
				       end);
				if (status = $ERROR_SUCCESS)
				  let buffer-size
				    = raw-as-integer
				        (primitive-c-unsigned-long-at
					   (primitive-cast-raw-as-pointer
					      (primitive-string-as-raw(buffer-size-buffer)),
					    integer-as-raw(0), integer-as-raw(0)));
				  // NOTE: For registry entries, the returned buffer-size 
				  //       includes the trailing NUL character ...
				  let buffer :: <byte-string>
				    = make(<byte-string>, size: buffer-size, fill: '\0');
				  let status
				    = raw-as-integer
					(%call-c-function ("RegQueryValueExA",
							   c-modifiers: "__stdcall")
					     (hKey :: <raw-c-pointer>,
					      lpValueName :: <raw-byte-string>, 
					      lpReserved :: <raw-c-pointer>,
					      lpType :: <raw-c-pointer>,
					      lpData :: <raw-c-pointer>,
					      lpcbData :: <raw-c-pointer>)
					  => (success? :: <raw-c-signed-long>)
					   (primitive-cast-raw-as-pointer
					      (primitive-unwrap-machine-word(handle)),
					    primitive-string-as-raw(name),
					    primitive-cast-raw-as-pointer(integer-as-raw(0)),
					    primitive-cast-raw-as-pointer
					      (primitive-string-as-raw(type-buffer)),
					    primitive-cast-raw-as-pointer
					      (primitive-string-as-raw(buffer)),
					    primitive-cast-raw-as-pointer
					      (primitive-string-as-raw(buffer-size-buffer)))
					 end);
				  if (status = $ERROR_SUCCESS)
				    return(copy-sequence(buffer, end: buffer-size - 1))
				  else
				    return(#f)
				  end
				else
				  return(#f)
				end
			      end)
		       end)
		end)
	 end)
  end
end function current-version-key;

define function owner-name () => (name :: false-or(<string>))
  current-version-key("RegisteredOwner")
end function owner-name;

define function owner-organization () => (organization :: false-or(<string>))
  current-version-key("RegisteredOrganization")
end function owner-organization;

define constant $environment-variable-delimiter = ';';

define function environment-variable
    (name :: <byte-string>) => (value :: false-or(<byte-string>))
  let eb-size :: <integer> = 1024;
  let envvar-buffer :: <byte-string> = make(<byte-string>, size: eb-size, fill: '\0');
  let envvar-size :: <integer>
    = raw-as-integer(%call-c-function ("GetEnvironmentVariableA", c-modifiers: "__stdcall")
		         (lpName :: <raw-byte-string>,
			  lpBuffer :: <raw-byte-string>,
			  nSize :: <raw-c-unsigned-long>)
		      => (value-size :: <raw-c-unsigned-long>)
		       (primitive-string-as-raw(name),
			primitive-string-as-raw(envvar-buffer),
			integer-as-raw(eb-size))
		     end);
  if (envvar-size > eb-size)
    // Value was too large to fit in our initial buffer but GetEnvironmentVariableA
    // tells us how long it actually is so we can just make a buffer large enough
    let eb-size :: <integer> = envvar-size + 1;
    envvar-buffer := make(<byte-string>, size: eb-size, fill: '\0');
    envvar-size :=
      raw-as-integer(%call-c-function ("GetEnvironmentVariableA", c-modifiers: "__stdcall")
			 (lpName :: <raw-byte-string>,
			  lpBuffer :: <raw-byte-string>,
			  nSize :: <raw-c-unsigned-long>)
		      => (value-size :: <raw-c-unsigned-long>)
		       (primitive-string-as-raw(name),
			primitive-string-as-raw(envvar-buffer),
			integer-as-raw(eb-size))
		     end)
  end;
  if (envvar-size > 0)
    copy-sequence(envvar-buffer, end: envvar-size)
  else
    %call-c-function ("SetLastError", c-modifiers: "__stdcall")
	(dwErrorCode :: <raw-c-unsigned-long>) => (nothing :: <raw-c-void>)
      (integer-as-raw(0))
    end;
    #f
  end
end function environment-variable;

define function environment-variable-setter
    (new-value :: false-or(<byte-string>), name :: <byte-string>)
 => (new-value :: false-or(<byte-string>))
  //---*** Should we signal an error here if this call fails?
  %call-c-function ("SetEnvironmentVariableA", c-modifiers: "__stdcall")
      (lpName :: <raw-byte-string>, lpValue :: <raw-byte-string>)
   => (success? :: <raw-c-signed-int>)
    (primitive-string-as-raw(name), if (new-value)
				      primitive-string-as-raw(new-value)
				    else
				      integer-as-raw(0)
				    end)
  end;
  new-value
end function environment-variable-setter;

define class <application-process> (<object>)
  constant slot application-process-handle :: <machine-word>,
    required-init-keyword: process-handle:;
  slot %application-process-state :: one-of(#"running", #"exited"),
    init-value: #"running";
  slot %application-process-status-code :: <integer>,
    init-value: 0;
end class;

define constant $STARTUPINFO_SIZE = 16 * $DWORD_SIZE;
define constant $STARTF_USESHOWWINDOW = 1;
define constant $STARTF_USESTDHANDLES = #x00000100;

define constant $SW-HIDE                            =    0;
define constant $SW-SHOWNORMAL                      =    1;
define constant $SW-SHOWMINIMIZED                   =    2;
define constant $SW-SHOWNOACTIVATE                  =    4;
define constant $SW-SHOWMINNOACTIVE                 =    7;

define constant $BUFFER-MAX = 4096;
define constant $SECURITY_ATTRIBUTES_SIZE = 3 * $DWORD_SIZE;

define constant $STD_INPUT_HANDLE  = -10;
define constant $STD_OUTPUT_HANDLE = -11;
define constant $STD_ERROR_HANDLE  = -12;

define constant $HANDLE_FLAG_INHERIT = #x00000001;

define constant $WAIT_FAILED   = -1;
define constant $WAIT_OBJECT_0 = 0;
// define constant $WAIT_TIMEOUT  = #x102;

define inline-only function startupinfo-cb-setter
    (cb :: <integer>, startupinfo :: <byte-string>) => (cb :: <integer>)
  primitive-c-unsigned-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo)),
       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw(cb);
  cb
end function startupinfo-cb-setter;

define inline-only function startupinfo-dwFlags
    (startupinfo :: <byte-string>) => (dwFlags :: <integer>)
  raw-as-integer
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo)),
	integer-as-raw(11), integer-as-raw(0)))
end function startupinfo-dwFlags;

define inline-only function startupinfo-dwFlags-setter
    (dwFlags :: <integer>, startupinfo :: <byte-string>) => (dwFlags :: <integer>)
  primitive-c-unsigned-long-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo)),
       integer-as-raw(11), integer-as-raw(0))
    := integer-as-raw(dwFlags);
  dwFlags
end function startupinfo-dwFlags-setter;

define inline-only function startupinfo-wShowWindow-setter
    (wShowWindow :: <integer>, startupinfo :: <byte-string>) => (wShowWindow :: <integer>)
  primitive-c-unsigned-short-at
      (primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo)),
       integer-as-raw(24), integer-as-raw(0))
    := integer-as-raw(wShowWindow);
  wShowWindow
end function startupinfo-wShowWindow-setter;

define function startupinfo-StdInput-setter
    (input-pipe :: <machine-word>, startupinfo :: <byte-string>) => ();
  let input-pipe-ptr
    = primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(input-pipe));
  let startupinfo-ptr
    = primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo));
  primitive-c-unsigned-long-at
      (startupinfo-ptr, integer-as-raw(14), integer-as-raw(0))
    := input-pipe-ptr;
end function startupinfo-StdInput-setter;

define function startupinfo-StdOutput-setter
    (output-pipe :: <machine-word>, startupinfo :: <byte-string>) => ();
  let output-pipe-ptr
    = primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(output-pipe));
  let startupinfo-ptr
    = primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo));
  primitive-c-unsigned-long-at
    (startupinfo-ptr, integer-as-raw(15), integer-as-raw(0))
    := output-pipe-ptr;
end function startupinfo-StdOutput-setter;

define function startupinfo-StdError-setter
    (error-pipe :: <machine-word>, startupinfo :: <byte-string>) => ();
  let error-pipe-ptr
    = primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(error-pipe));
  let startupinfo-ptr
    = primitive-cast-raw-as-pointer(primitive-string-as-raw(startupinfo));
  primitive-c-unsigned-long-at
      (startupinfo-ptr, integer-as-raw(16), integer-as-raw(0))
    := error-pipe-ptr;
end function startupinfo-StdError-setter;

define constant $PROCESS_INFORMATION_SIZE = 4 * $DWORD_SIZE;

define inline-only function process-information-hProcess
    (process-information :: <byte-string>) => (hProcess :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer
          (primitive-string-as-raw(process-information)),
	integer-as-raw(0), integer-as-raw(0)))
end function process-information-hProcess;

define inline-only function process-information-hThread
    (process-information :: <byte-string>) => (hThread :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-c-unsigned-long-at
       (primitive-cast-raw-as-pointer
          (primitive-string-as-raw(process-information)),
	integer-as-raw(1), integer-as-raw(0)))
end function process-information-hThread;

/// Masks out all but the result code to enable easier comparisions.
define constant $HRESULT_CODE_MASK = #x0000FFFF;

define inline-only function win32-last-error () => (status :: <integer>)
  raw-as-integer
    (primitive-machine-word-logand
       (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
	    () => (status :: <raw-c-unsigned-long>) ()
	end,
	integer-as-raw($HRESULT_CODE_MASK)))
end function win32-last-error;

define constant $null-device = "NUL:";

define function run-application
    (command :: type-union(<string>, limited(<sequence>, of: <string>)),
     #key under-shell? = #f,
          inherit-console? = #t,
          activate? = #t,
          minimize? = #f,
          hide? = #f,
          outputter :: false-or(<function>) = #f,
          asynchronous? = #f,

          environment :: false-or(<explicit-key-collection>),
          working-directory :: false-or(<pathname>) = #f,
     
          input :: type-union(one-of(#"inherit", #"null", #"stream"),
                              <pathname>) = #"inherit",
          if-input-does-not-exist :: one-of(#"signal", #"create") = #"signal",
          output :: type-union(one-of(#"inherit", #"null", #"stream"),
                               <pathname>) = #"inherit",
          if-output-exists :: one-of(#"signal", #"new-version", #"replace",
                                     #"overwrite", #"append",
                                     #"truncate") = #"replace",
          error: _error :: type-union(one-of(#"inherit", #"null", #"stream", #"output"),
                              <pathname>) = #"inherit",
          if-error-exists :: one-of(#"signal", #"new-version", #"replace",
                                    #"overwrite", #"append",
                                    #"truncate") = #"replace")
 => (exit-code :: <integer>, signal :: false-or(<integer>),
     child :: false-or(<application-process>), #rest streams);
  let startupInfo :: <byte-string>
    = make(<byte-string>, size: $STARTUPINFO_SIZE, fill: '\0');
  let processInfo :: <byte-string>
    = make(<byte-string>, size: $PROCESS_INFORMATION_SIZE, fill: '\0');
  let command
    = if (under-shell?)
        concatenate-as(<string>, environment-variable("COMSPEC"), " /c ",
                       command)
      else
        command
      end;
  startupinfo-cb(startupInfo) := $STARTUPINFO_SIZE;
  startupinfo-dwFlags(startupInfo) := $STARTF_USESHOWWINDOW;
  startupinfo-wShowWindow(startupInfo)
    := case
         hide?                   => $SW-HIDE;
	 activate?  & minimize?  => $SW-SHOWMINIMIZED;
	 activate?  & ~minimize? => $SW-SHOWNORMAL;
	 ~activate? & minimize?  => $SW-SHOWMINNOACTIVE;
	 ~activate? & ~minimize? => $SW-SHOWNOACTIVATE;
	 // compiler can't figure out that above covers all cases, so thinks
	 // there is a chance of returning #f.  Disabuse it of that notion.
	 otherwise => -1;
       end;

  let streams :: <list> = #();
  let close-handles :: <list> = #();

  startupinfo-StdInput(startupInfo)
    := select (input)
         #"inherit" =>
           win32-std-handle($STD_INPUT_HANDLE);
         #"null" =>
           win32-open/create($null-device, $GENERIC_READ,
                             $FILE_SHARE_READ, $OPEN_EXISTING);
         #"stream" =>
           let (input-p, output-p) = Win32CreatePipe();
           streams := add(streams, make(<file-stream>,
                                        locator: output-p,
                                        file-descriptor: output-p,
                                        direction: #"output"));
           Win32SetHandleInformation(output-p,
                                     as(<machine-word>, $HANDLE_FLAG_INHERIT),
                                     as(<machine-word>, 0));
           close-handles := add(close-handles, input-p);
           input-p;
         otherwise =>
           let pathstring = as(<byte-string>, expand-pathname(input));
           let fdwCreate
             = if (if-input-does-not-exist == #"create")
                 $CREATE_ALWAYS
               else
                 $OPEN_EXISTING
               end if;
           win32-open/create(pathstring, $GENERIC_READ, $FILE_SHARE_READ,
                             fdwCreate);
       end select;
  
  let input-pipe :: <machine-word> = as(<machine-word>, 0);
  let output-pipe :: <machine-word> = as(<machine-word>, 0);
  if (outputter)
    inherit-console? := #t;
    let (input-p, output-p) = Win32CreatePipe();
    input-pipe := input-p;
    output-pipe := output-p;
    startupinfo-StdOutput(startupInfo) := output-p;
    startupinfo-StdError(startupInfo) := output-p;

    Win32SetHandleInformation(input-p,
                              as(<machine-word>, $HANDLE_FLAG_INHERIT),
                              as(<machine-word>, 0));
    close-handles := add(close-handles, output-p);
  else
    local
      method open-output
          (key, if-exists, std-handle, output-handle)
       => (handle :: <machine-word>);
        select (key)
          #"inherit" =>
            win32-std-handle(std-handle);
          #"null" =>
            win32-open/create($null-device, $GENERIC_WRITE,
                              $FILE_SHARE_WRITE, $OPEN_EXISTING);
          #"stream" =>
            let (input-p, output-p) = Win32CreatePipe();
            streams := add(streams, make(<file-stream>,
                                         locator: input-p,
                                         file-descriptor: input-p,
                                         direction: #"input"));
            Win32SetHandleInformation(input-p,
                                      as(<machine-word>, $HANDLE_FLAG_INHERIT),
                                      as(<machine-word>, 0));
            close-handles := add(close-handles, output-p);
            output-p;
          #"output" =>
            output-handle;
          otherwise =>
            let pathstring = as(<byte-string>, expand-pathname(key));
            let fdwCreate
              = select (if-exists)
                  #"signal" =>
                    error("not yet");
                  #"new-version", #"replace" =>
                    $CREATE_ALWAYS;
                  #"overwrite", #"append" =>
                    $OPEN_EXISTING;
                  #"truncate" =>
                    $TRUNCATE_EXISTING;
                end select;
            let handle
              = win32-open/create(pathstring, $GENERIC_WRITE, $FILE_SHARE_WRITE,
                                  fdwCreate);
            if (if-output-exists == #"append")
              win32-set-file-position(handle, 0, $FILE_END);
            end if;
            handle;
        end select;
      end method;

    // FIXME
    let output-handle
      = open-output(output, if-output-exists, $STD_OUTPUT_HANDLE, #f);
    startupinfo-StdOutput(startupInfo)
      := output-handle;
    startupinfo-StdError(startupInfo)
      := open-output(_error, if-error-exists, $STD_ERROR_HANDLE, output-handle);
  end if;

  if (outputter
        | input ~== #"inherit" | output ~== #"inherit" | _error ~== #"inherit")
    startupinfo-dwFlags(startupInfo)
      := logior(startupinfo-dwFlags(startupInfo), $STARTF_USESTDHANDLES);
  end if;


  if (primitive-raw-as-boolean
	(%call-c-function ("CreateProcessA", c-modifiers: "__stdcall")
	     (lpApplicationName :: <raw-byte-string>,
	      lpCommandLine :: <raw-byte-string>,
	      lpProcessAttributes :: <raw-c-pointer>,
	      lpThreadAttributes :: <raw-c-pointer>,
	      bInheritHandles :: <raw-c-signed-int>,
	      dwCreationFlags :: <raw-c-unsigned-long>,
	      lpEnvironment :: <raw-c-pointer>,
	      lpCurrentDirectory :: <raw-byte-string>,
	      lpStartupInfo :: <raw-c-pointer>,
	      lpProcessInformation :: <raw-c-pointer>)
	  => (success? :: <raw-c-signed-int>)
	   (integer-as-raw(0),
	    primitive-string-as-raw(command),
	    integer-as-raw(0), integer-as-raw(0),
	    if (inherit-console?) integer-as-raw(1) else integer-as-raw(0) end,
	    integer-as-raw(0),
            integer-as-raw(0),
            if (working-directory)
              primitive-string-as-raw(as(<byte-string>, working-directory))
            else
              integer-as-raw(0)
            end,
	    primitive-cast-raw-as-pointer(primitive-string-as-raw(startupInfo)),
	    primitive-cast-raw-as-pointer(primitive-string-as-raw(processInfo)))
	 end))
    block ()
      if (asynchronous?)
        win32-close-handle(process-information-hThread(processInfo));
        let child
          = make(<application-process>,
                 process-handle: process-information-hProcess(processInfo));
        apply(values, 0, #f, child, reverse!(streams))
      else
        if (outputter)
          win32-close-handle(output-pipe);
          run-outputter(outputter, input-pipe);
        end if;
        
        let wait-result
          = win32-wait-for-single-object
              (process-information-hProcess(processInfo), $INFINITE_TIMEOUT);
        if (primitive-machine-word-equal?(wait-result,
                                          integer-as-raw($WAIT_FAILED)))
          error("wait for process failed: %s", win32-last-error-message());
        end;
        let (success?, code)
          = win32-get-exit-code-process
              (process-information-hProcess(processInfo));
        if (~success?)
          error("get exit code failed: %s", win32-last-error-message());
        end;
	let exit-code
          = raw-as-integer
              (primitive-machine-word-logand
                 (primitive-unwrap-machine-word(code),
                  integer-as-raw($HRESULT_CODE_MASK)));
        let signal-code = 0;    // FIXME
        apply(values, exit-code, (signal-code ~= 0) & signal-code, #f,
              reverse!(streams))
      end
    cleanup
      do(win32-close-handle, close-handles);
      
      unless (asynchronous?)
        win32-close-handle(process-information-hProcess(processInfo));
        win32-close-handle(process-information-hThread(processInfo));
      end
    end
  else
    error("create process failed: %s", win32-last-error-message());
  end
end function run-application;

define function run-outputter
    (outputter :: <function>, input-pipe :: <machine-word>) => ();
  let dylan-win32-buffer = make(<byte-string>, size: $BUFFER-MAX, fill: '\0');
  let win32-buffer
    = primitive-wrap-machine-word
        (primitive-string-as-raw(dylan-win32-buffer));
  with-stack-dword (actual-transfer)
    iterate loop ()
      if (Win32ReadFile(input-pipe, win32-buffer, actual-transfer))
        let count
          = raw-as-integer(primitive-c-unsigned-long-at
                             (primitive-cast-raw-as-pointer
                                (primitive-unwrap-machine-word
                                   (actual-transfer)),
                              integer-as-raw(0), integer-as-raw(0)));
        if (count)
          outputter(dylan-win32-buffer, end: count);
          loop();
        end if;
      else
        let last-error = win32-raw-last-error();
        if (last-error ~= $ERROR_HANDLE_EOF & last-error ~= $ERROR_BROKEN_PIPE)
          win32-last-error()
        end if;
      end if;
    end iterate;
  end;
end function;

define inline function win32-close-handle
    (handle :: <machine-word>) => (success? :: <boolean>);
  primitive-raw-as-boolean
    (%call-c-function ("CloseHandle", c-modifiers: "__stdcall")
       (hHandle :: <raw-c-pointer>) => (closed? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
    end)
end function;

define inline-only function Win32CreatePipe()
 => (input-pipe :: <machine-word>, output-pipe :: <machine-word>)
  let PipeSecurity :: <byte-string> = make(<byte-string>, size: $SECURITY_ATTRIBUTES_SIZE, fill: '\0');
  let PipeSecurity-pointer = primitive-cast-raw-as-pointer(primitive-string-as-raw(PipeSecurity));

  primitive-c-unsigned-long-at
      (PipeSecurity-pointer,
       integer-as-raw(0), integer-as-raw(0))
    := integer-as-raw($SECURITY_ATTRIBUTES_SIZE);

  primitive-c-unsigned-long-at
      (PipeSecurity-pointer,
       integer-as-raw(2), integer-as-raw(0))
    := integer-as-raw(1);

  let input-pipe = 
    primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(Win32LocalAlloc()));
  let output-pipe =
    primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(Win32LocalAlloc()));

  %call-c-function ("CreatePipe", c-modifiers: "__stdcall")
    (OutPipeRead :: <raw-c-pointer>,
     OutPipeWrite :: <raw-c-pointer>,
     PipeSecurity :: <raw-c-pointer>,
     unknown :: <raw-c-unsigned-long>) => (created? :: <raw-c-signed-int>)
    (input-pipe,
     output-pipe,
     PipeSecurity-pointer,
     integer-as-raw($BUFFER-MAX))
  end;
  values(primitive-wrap-machine-word
	   (primitive-c-unsigned-long-at
	      (input-pipe, integer-as-raw(0), integer-as-raw(0))),
	 primitive-wrap-machine-word
	   (primitive-c-unsigned-long-at
	      (output-pipe, integer-as-raw(0), integer-as-raw(0))))
end function;

define inline-only function Win32SetHandleInformation
    (handle :: <machine-word>, mask :: <machine-word>, flags :: <machine-word>)
 => (success? :: <boolean>);
  primitive-raw-as-boolean
    (%call-c-function ("SetHandleInformation", c-modifiers: "__stdcall")
       (hObject :: <raw-c-pointer>, dwMask :: <raw-c-unsigned-long>,
        dwFlags :: <raw-c-unsigned-long>) => (success? :: <raw-c-signed-int>)
     (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
      primitive-unwrap-machine-word(mask),
      primitive-unwrap-machine-word(flags))
     end)
end function;

// Can be used to see if there's input available on an anonymous pipe ...
define inline-only function Win32PeekNamedPipe
    (input-pipe :: <machine-word>, bytes-available :: <machine-word>)
 => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("PeekNamedPipe", c-modifiers: "__stdcall")
         (hNamedPipe :: <raw-c-pointer>, lpBuffer :: <raw-c-pointer>,
	  nBufferSize :: <raw-c-unsigned-long>, lpBytesRead :: <raw-c-pointer>,
	  lpTotalBytesAvail :: <raw-c-pointer>, lpBytesLeftThisMessage :: <raw-c-pointer>)
      => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(input-pipe)),
	primitive-cast-raw-as-pointer(integer-as-raw(0)),
	integer-as-raw(0),
	primitive-cast-raw-as-pointer(integer-as-raw(0)),
	primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(bytes-available)),
	primitive-cast-raw-as-pointer(integer-as-raw(0)))
     end)
end function Win32PeekNamedPipe;

define inline-only function Win32LocalAlloc()
 => (c-pointer :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
           (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
        => (pointer :: <raw-c-pointer>)
         (integer-as-raw(0), integer-as-raw(4))
        end));
end function;

define inline-only function Win32ReadFile
    (handle :: <machine-word>, buffer :: <machine-word>, actual-transfer :: <machine-word>)
 => (success? :: <boolean>)
  primitive-raw-as-boolean
  (%call-c-function ("ReadFile", c-modifiers: "__stdcall")
     (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
      count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
      lpOverlapped :: <raw-c-pointer>)
     => (success? :: <raw-c-signed-int>)
     (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
      primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(buffer)),
      integer-as-raw($BUFFER-MAX),
      primitive-cast-raw-as-pointer
	(primitive-unwrap-machine-word(actual-transfer)),
      primitive-cast-raw-as-pointer(integer-as-raw(0)))
  end);
end function;

/*
define inline-only function Win32WriteFile
    (handle :: false-or(<machine-word>), buffer :: <machine-word>,
     actual-transfer :: <machine-word>, dummy-transfer :: <machine-word>)
 => (success? :: <boolean>)
  if (handle)
  primitive-raw-as-boolean
  (%call-c-function ("WriteFile", c-modifiers: "__stdcall")
     (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
      count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
      lpOverlapped :: <raw-c-pointer>)
     => (success? :: <raw-c-signed-int>)
     (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
      primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(buffer)),
      primitive-c-unsigned-long-at
	(primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(actual-transfer)),
	 integer-as-raw(0), integer-as-raw(0)),
      primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(dummy-transfer)),
      primitive-cast-raw-as-pointer(integer-as-raw(0)))
  end);
  end if;
end function;
*/

define function wait-for-application-process
    (process :: <application-process>)
 => (exit-code :: <integer>, signal :: false-or(<integer>));
  if (process.%application-process-state == #"running")
    let wait-result
      = win32-wait-for-single-object(process.application-process-handle,
                                     $INFINITE_TIMEOUT);
    if (primitive-machine-word-equal?
          (wait-result, integer-as-raw($WAIT_FAILED)))
      win32-last-error();
    end;
    
    let (success?, return-status)
      = win32-get-exit-code-process(process.application-process-handle);

    process.%application-process-status-code
      := if (success?)
           raw-as-integer
             (primitive-machine-word-logand
                (primitive-unwrap-machine-word(return-status),
                 integer-as-raw($HRESULT_CODE_MASK)))
         else
           win32-last-error()
         end;
    process.%application-process-state := #"exited";
  end if;
  let status-code = process.%application-process-status-code;
  //let signal-code = logand(status-code, #o177);
  let exit-code = status-code;  // FIXME
  let signal-code = 0;          // FIXME
  values(exit-code, (signal-code ~= 0) & signal-code);
end function;

define function win32-wait-for-single-object
    (handle :: <machine-word>, timeout :: <integer>)
 => (result :: <machine-word>);
  primitive-wrap-machine-word
    (%call-c-function ("WaitForSingleObject", c-modifiers: "__stdcall")
          (hHandle :: <raw-c-pointer>, dwMilliseconds :: <raw-c-unsigned-long>)
       => (result :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer
	  (primitive-unwrap-machine-word(handle)),
	integer-as-raw(timeout))
     end)
end function;

define function win32-get-exit-code-process
    (handle :: <machine-word>)
 => (success? :: <boolean>, code :: <machine-word>);
  with-stack-dword (status-word)
    let success?
      = primitive-raw-as-boolean
          (%call-c-function ("GetExitCodeProcess", c-modifiers: "__stdcall")
                (hProcess :: <raw-c-pointer>, lpExitCode :: <raw-c-pointer>)
             => (success? :: <raw-c-signed-int>)
             (primitive-cast-raw-as-pointer
                (primitive-unwrap-machine-word(handle)),
              primitive-cast-raw-as-pointer
                (primitive-unwrap-machine-word(status-word)))
           end);
    let code
      = primitive-wrap-machine-word
          (primitive-c-unsigned-long-at
             (primitive-cast-raw-as-pointer
                (primitive-unwrap-machine-word(status-word)),
              integer-as-raw(0), integer-as-raw(0)));
    values(success?, code)
  end
end function;

/// Inter-Process Synchronization tools


define function create-application-event
    (event :: <string>) => (event-object :: <machine-word>)
    primitive-wrap-machine-word
    (%call-c-function ("CreateEventA", c-modifiers: "__stdcall")
       (lpEventAttributes :: <raw-c-pointer>,
	bManualReset :: <raw-c-signed-int>,
	bInitialState :: <raw-c-signed-int>,
	lpName :: <raw-byte-string>)
       => (handle :: <raw-c-pointer>)
       (primitive-cast-raw-as-pointer(integer-as-raw(0)),
	integer-as-raw(0),
	integer-as-raw(0),
	primitive-cast-raw-as-pointer(primitive-string-as-raw(event)))
    end);
end function;

define constant $INFINITE_TIMEOUT = -1;

define function wait-for-application-event
    (event-object :: <machine-word>,
     #key timeout :: <integer> = $INFINITE_TIMEOUT)
 => (success? :: <boolean>)
  let wait-result = win32-wait-for-single-object(event-object, timeout);
  win32-close-handle(event-object);
  wait-result ~== $WAIT_FAILED
end function;

define constant $STANDARD_RIGHTS_REQUIRED = #x000F0000;
define constant $SYNCHRONIZE              = #x00100000;
define constant $EVENT_ALL_ACCESS =
  logior($STANDARD_RIGHTS_REQUIRED, $SYNCHRONIZE, #x03);

define function signal-application-event
    (event :: <string>)
 => (success? :: <boolean>)
  let event-object =
    primitive-wrap-machine-word
    (%call-c-function ("OpenEventA", c-modifiers: "__stdcall")
       (dwDesiredAccess :: <raw-c-unsigned-long>,
	bInheritHandle :: <raw-c-signed-int>,
	lpName :: <raw-byte-string>)
       => (handle :: <raw-c-pointer>)
       (integer-as-raw($EVENT_ALL_ACCESS),
	integer-as-raw(0),
	primitive-cast-raw-as-pointer(primitive-string-as-raw(event)))
    end);
  
  let success? :: <boolean> =
    primitive-raw-as-boolean
    (%call-c-function ("SetEvent", c-modifiers: "__stdcall")
       (hHandle :: <raw-c-pointer>)
       => (result :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer
	  (primitive-unwrap-machine-word(event-object)))
    end);

  win32-close-handle(event-object);

  success?
end function;


define function load-library
    (name :: <string>)
 => (module)
  let module =
    primitive-wrap-machine-word
    (%call-c-function ("LoadLibraryA", c-modifiers: "__stdcall")
       (lpName :: <raw-byte-string>)
       => (handle :: <raw-c-pointer>)
       (primitive-cast-raw-as-pointer(primitive-string-as-raw(name)))
    end);
  
  module

end function;


