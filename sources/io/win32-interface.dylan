Module:       io-internals
Synopsis:     An interface to file-related win32 API calls.
Author:       Eliot Miranda, Scott McKay, Marc Ferguson, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Uses the low-level FFI rather than the real C FFI and should,
// eventually, be rewritten.

// GetFileType results
//define constant $FILE_TYPE_UNKNOWN = #x0000;
define constant $FILE_TYPE_DISK    = #x0001;
//define constant $FILE_TYPE_CHAR    = #x0002;
//define constant $FILE_TYPE_PIPE    = #x0003;
//define constant $FILE_TYPE_REMOTE  = #x8000;

// SetFilePointer
// whence argument
define constant $FILE_BEGIN   = 0;
define constant $FILE_CURRENT = 1;
define constant $FILE_END     = 2;
ignorable($FILE_BEGIN, $FILE_CURRENT, $FILE_END);

define constant $ERROR_HANDLE_EOF = 38;

define constant $FORMAT_MESSAGE_FLAGS         = #x00001100;
define constant $FORMAT_MESSAGE_LANGUAGE      = #x00000400;

// A useful utility ...

define function call-succeeded? (result :: <machine-word>) => (success :: <boolean>)
  primitive-machine-word-not-equal?
    (primitive-unwrap-machine-word(result),
     integer-as-raw(-1))
end function call-succeeded?;
 

// Now the actual interfaces ...

define function win32-close (handle :: <machine-word>) => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("CloseHandle", c-modifiers: "__stdcall")
         (handle :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
     end)
end function win32-close;

define function win32-file-positionable?
    (handle :: <machine-word>)
 => (positionable? :: <boolean>)
  let type = raw-as-integer
                (%call-c-function ("GetFileType", c-modifiers: "__stdcall")
                     (handle :: <raw-c-pointer>) 
                  => (eof :: <raw-c-unsigned-long>)
                   (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
                 end);
  type = $FILE_TYPE_DISK
end function win32-file-positionable?;

define function win32-file-size (handle :: <machine-word>)
 => (fsize :: false-or(<integer>))
  let fsize = primitive-wrap-machine-word
                (%call-c-function ("GetFileSize", c-modifiers: "__stdcall")
                     (handle :: <raw-c-pointer>, lpFileSizeHigh :: <raw-c-pointer>) 
                  => (eof :: <raw-c-unsigned-long>)
                   (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                    primitive-cast-raw-as-pointer(integer-as-raw(0)))
                 end);
  call-succeeded?(fsize) & raw-as-integer(primitive-unwrap-machine-word(fsize))
end function win32-file-size;

define function win32-set-file-position (handle :: <machine-word>, position :: <integer>, mode :: <integer>)
 => (newpos :: false-or(<integer>))
  let newpos = primitive-wrap-machine-word
                (%call-c-function ("SetFilePointer", c-modifiers: "__stdcall")
                     (handle :: <raw-c-pointer>,
                      distance-to-move :: <raw-c-signed-long>,
                      lpDistanceToMoveHigh :: <raw-c-pointer>,
                      move-method :: <raw-c-unsigned-long>)
                  => (newpos :: <raw-c-unsigned-long>)
                   (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                    integer-as-raw(position),
                    primitive-cast-raw-as-pointer(integer-as-raw(0)),
                    integer-as-raw(mode))
                 end);
  call-succeeded?(newpos)
    & raw-as-integer(primitive-unwrap-machine-word(newpos))
end function win32-set-file-position;

// NOTE -- Should probably have one of these per thread, possibly per stream ...
define variable actual-count-ptr =
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
           (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
        => (pointer :: <raw-c-pointer>)
         (integer-as-raw(0), integer-as-raw(4))
        end));

define function win32-read
    (handle :: <machine-word>, data :: <buffer>, data-offset :: <integer>, 
     count :: <integer> /* , overlapped :: false-or(<win32-overlapped>) */ )
 => (nread :: false-or(<integer>))
  let success?
    = primitive-raw-as-boolean
        (%call-c-function ("ReadFile", c-modifiers: "__stdcall")
	     (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
	      count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
	      lpOverlapped :: <raw-c-pointer>)
	  => (success? :: <raw-c-signed-int>)
	   (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
	    primitive-cast-raw-as-pointer
	      (primitive-machine-word-add
		 (primitive-cast-pointer-as-raw
		    (primitive-repeated-slot-as-raw(data,
						    primitive-repeated-slot-offset(data))),
		  integer-as-raw(data-offset))),
	    integer-as-raw(count),
	    primitive-cast-raw-as-pointer
	      (primitive-unwrap-machine-word(actual-count-ptr)),
	    primitive-cast-raw-as-pointer(integer-as-raw(0)))
	 end);
  success? := success? | (win32-raw-last-error() = $ERROR_HANDLE_EOF);
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
		  (primitive-unwrap-machine-word(actual-count-ptr),
		   integer-as-raw(0),
		   integer-as-raw(0)))
end function win32-read;

define function win32-write
    (handle :: <machine-word>, data :: <buffer>, data-offset :: <integer>, 
     count :: <integer> /* , overlapped :: false-or(<win32-overlapped>) */ )
 => (nwritten :: false-or(<integer>))
  let success?
    = primitive-raw-as-boolean
        (%call-c-function ("WriteFile", c-modifiers: "__stdcall")
	     (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
	      count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
	      lpOverlapped :: <raw-c-pointer>)
	  => (success? :: <raw-c-signed-int>)
	     (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
	      primitive-cast-raw-as-pointer
		(primitive-machine-word-add
		   (primitive-cast-pointer-as-raw
		      (primitive-repeated-slot-as-raw(data,
						      primitive-repeated-slot-offset(data))),
		    integer-as-raw(data-offset))),
	      integer-as-raw(count),
	      primitive-cast-raw-as-pointer
		(primitive-unwrap-machine-word(actual-count-ptr)),
	      primitive-cast-raw-as-pointer(integer-as-raw(0)))
	 end);
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
                  (primitive-unwrap-machine-word(actual-count-ptr),
                   integer-as-raw(0),
                   integer-as-raw(0)))
end function win32-write;

define function win32-force-output (handle :: <machine-word>) => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("FlushFileBuffers", c-modifiers: "__stdcall")
         (handle :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
      end)
end function win32-force-output;

//---*** andrewa: ideally we should merge the use of this with win32-last-error
//---*** but until then I've renamed this as win32-raw-last-error.
define function win32-raw-last-error () => (status :: <machine-word>)
  primitive-wrap-machine-word
    (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
         () => (status :: <raw-c-unsigned-long>)
       ()
     end)
end function win32-raw-last-error;

// NOTE -- Should probably have one of these per thread ...
define variable message-buffer-ptr =
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw
       (%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
           (flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>)
        => (pointer :: <raw-c-pointer>)
         (integer-as-raw(0), integer-as-raw(4))
        end));

define function win32-last-error-message () => (message :: <string>)
  let status = primitive-wrap-machine-word
		 (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
		      () => (status :: <raw-c-unsigned-long>)
		    ()
		  end);
  %call-c-function ("FormatMessageA", c-modifiers: "__stdcall")
      (flags :: <raw-c-unsigned-long>, lpSource :: <raw-c-pointer>,
       message-id :: <raw-c-unsigned-long>, language-id :: <raw-c-unsigned-long>,
       lpBuffer :: <raw-c-pointer>, bytes :: <raw-c-unsigned-long>,
       lpArguments :: <raw-c-pointer>)
   => (count :: <raw-c-unsigned-long>)
    (integer-as-raw($FORMAT_MESSAGE_FLAGS),
     primitive-cast-raw-as-pointer(integer-as-raw(0)),
     primitive-unwrap-machine-word(status),
     integer-as-raw($FORMAT_MESSAGE_LANGUAGE),
     primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(message-buffer-ptr)),
     integer-as-raw(0),
     primitive-cast-raw-as-pointer(integer-as-raw(0)))
  end;
  let message = primitive-raw-as-string
                  (primitive-c-pointer-at
                     (primitive-unwrap-machine-word(message-buffer-ptr),
                      integer-as-raw(0),
                      integer-as-raw(0)));
  %call-c-function ("LocalFree", c-modifiers: "__stdcall")
     (pointer :: <raw-c-pointer>) => (null-pointer :: <raw-c-pointer>)
    (primitive-c-pointer-at(primitive-unwrap-machine-word(message-buffer-ptr),
                            integer-as-raw(0),
                            integer-as-raw(0)))
  end;
  message
end function win32-last-error-message;
