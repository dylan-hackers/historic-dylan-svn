Module:       streams-internals
Synopsis:     An interface to file-related win32 API calls.
Author:       Eliot Miranda, Scott McKay, Marc Ferguson, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
              All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Uses the low-level FFI rather than the real C FFI and should, eventually, be rewritten.
// As it stands now, this code requires the use of glue code written in C.  (Sigh)

// fdwAccess argument
define constant $GENERIC_READ  = #x8000; // shifted right 16 bits
define constant $GENERIC_WRITE = #x4000; // shifted right 16 bits
ignorable($GENERIC_READ, $GENERIC_WRITE);

// fdwShareMode argument
define constant $FILE_SHARE_READ  = 1;
define constant $FILE_SHARE_WRITE = 2;
ignorable($FILE_SHARE_READ, $FILE_SHARE_WRITE);

// fdwCreate argument
define constant $CREATE_NEW        = 1;
define constant $CREATE_ALWAYS     = 2;
define constant $OPEN_EXISTING     = 3;
define constant $OPEN_ALWAYS       = 4;
define constant $TRUNCATE_EXISTING = 5;
ignorable($CREATE_NEW, $CREATE_ALWAYS, 
	  $OPEN_EXISTING, $OPEN_ALWAYS, $TRUNCATE_EXISTING);

// fdwAttrsAndFlags
define constant $FILE_ATTRIBUTE_NORMAL = #x80;
define constant $FILE_FLAG_OVERLAPPED  = #x4000; // shifted right 16 bits
ignorable($FILE_ATTRIBUTE_NORMAL, $FILE_FLAG_OVERLAPPED);

// SetFilePointer
// whence argument
define constant $FILE_BEGIN   = 0;
define constant $FILE_CURRENT = 1;
define constant $FILE_END     = 2;
ignorable($FILE_BEGIN, $FILE_CURRENT, $FILE_END);

// FormatMessage
define constant FORMAT_MESSAGE_FLAGS    = #x00001100;
define constant FORMAT_MESSAGE_LANGUAGE = #x00000400;
ignorable(FORMAT_MESSAGE_FLAGS, FORMAT_MESSAGE_LANGUAGE);


// A useful utility ...

define function call-succeeded? (result :: <machine-word>) => (success :: <boolean>)
  primitive-machine-word-not-equal?
    (primitive-unwrap-machine-word(result),
     integer-as-raw(-1))
end function call-succeeded?;
 

// Now the actual interfaces ...

define function win32-file-exists? (path :: <byte-string>) => (exists? :: <boolean>)
  let attributes = primitive-wrap-machine-word
		     (%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
                          (path :: <raw-byte-string>)
                       => (exists? :: <raw-c-unsigned-long>)
                        (primitive-string-as-raw(path))
                      end);
  call-succeeded?(attributes)
end function win32-file-exists?;

define method win32-open/create
    (path :: <byte-string>, access :: <integer>, share-mode :: <integer>,
     create-mode :: <integer>, #key overlapped? :: <boolean> = #f)
 => (handle :: false-or(<machine-word>))
  let attributes-high-bits :: <integer> =
    if(overlapped?) $FILE_FLAG_OVERLAPPED else 0 end;
  let handle = primitive-wrap-machine-word
                 (primitive-cast-pointer-as-raw
                    (%call-c-function ("CreateFileA", c-modifiers: "__stdcall")
                         (path :: <raw-byte-string>,
                          access :: <raw-c-unsigned-long>,
                          share-mode :: <raw-c-unsigned-long>,
                          security-attrs :: <raw-c-pointer>,
                          create-mode :: <raw-c-unsigned-long>, 
                          file-attrs :: <raw-c-unsigned-long>,
                          template :: <raw-c-pointer>)
                      => (handle :: <raw-c-pointer>)
                       (primitive-string-as-raw(path), 
                        primitive-machine-word-shift-left-low
                          (integer-as-raw(access),
                           integer-as-raw(16)),
                        integer-as-raw(share-mode),
                        primitive-cast-raw-as-pointer(integer-as-raw(0)),
                        integer-as-raw(create-mode),
			primitive-machine-word-logior(
                          primitive-machine-word-shift-left-low
                            (integer-as-raw(attributes-high-bits),
                             integer-as-raw(16)),
			  integer-as-raw($FILE_ATTRIBUTE_NORMAL)),
                        primitive-cast-raw-as-pointer(integer-as-raw(0)))
                     end));
  call-succeeded?(handle) & handle
end method win32-open/create;

define function win32-close (handle :: <machine-word>) => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("CloseHandle", c-modifiers: "__stdcall")
         (handle :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
     end)
end function win32-close;

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

ignorable(win32-set-file-position);
define function win32-set-file-position (handle :: <machine-word>, position :: <integer>)
 => (success? :: <boolean>)
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
                    integer-as-raw($FILE_BEGIN))
                 end);
  call-succeeded?(newpos) 
    & primitive-machine-word-equal?
        (primitive-unwrap-machine-word(newpos),
	 integer-as-raw(position))
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

define function force-object-black
    (buffer :: type-union(<buffer>, <byte-vector>)) => ()
  // To force an object to turn black, read it's wrapper and write it back.
  let zero = integer-as-raw(0);
  primitive-element(buffer, zero, zero)
    := primitive-element(buffer, zero, zero)
end function;


// NOTE: win32-file-accessor used to use overlapped structures to atomically
// set file position for reads and writes, but it turns out that (contrary to
// MS documentation) this doesn't work for Windows 95.  So we don't need this:

/*
define constant $DWORD_SIZE = raw-as-integer(primitive-word-size());
define constant $win32-overlapped-size = $DWORD_SIZE * 5;
define constant <win32-overlapped> = <machine-word>;

define inline function win32-allocate-overlapped () => (r :: <win32-overlapped>)
  let result :: <raw-c-pointer> =
    primitive-wrap-machine-word(
      primitive-cast-pointer-as-raw(
	%call-c-function ("LocalAlloc", c-modifiers: "__stdcall")
		(flags :: <raw-c-unsigned-int>, bytes :: <raw-c-unsigned-int>) 
	     => (pointer :: <raw-c-pointer>)
	  (integer-as-raw(#x0040), 	// LMEM_FIXED + LMEM_ZEROINIT
	   integer-as-raw($win32-overlapped-size))
	end %call-c-function
      )
    );
  if (primitive-machine-word-equal?(primitive-unwrap-machine-word(result),
				    integer-as-raw(0)))
    error("error allocating OVERLAPPED");
  end if;
  result
end function win32-allocate-overlapped;

define inline function win32-free-overlapped (o :: <win32-overlapped>) => ()
  %call-c-function ("LocalFree", c-modifiers: "__stdcall")
          (pointer :: <raw-c-pointer>) => (null :: <raw-c-pointer>)
    (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(o)))
  end %call-c-function;
end function win32-free-overlapped;

define inline function win32-overlapped-offset-setter 
	(v :: <integer>, o :: <win32-overlapped>) => (r :: <integer>)
  primitive-c-unsigned-long-at(primitive-unwrap-machine-word(o),
			       integer-as-raw(2), integer-as-raw(0))
	  := integer-as-raw(v);
  v
end function win32-overlapped-offset-setter;
*/

/*
define function win32-read
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nread :: false-or(<integer>))
  // If the OS call fails, it might be because of a GC read/write barrier.
  // Try the operation again a couple of times after triggering the GC
  // to blacken the object, just in case.
  // THIS IS A TEMPORARY HACK!
  win32-read-internal(handle, data, offset, count)
    | begin force-object-black(data); win32-read-internal(handle, data, offset, count) end
    | begin force-object-black(data); win32-read-internal(handle, data, offset, count) end
end function;
*/

define function win32-read  //  -internal
    (handle :: <machine-word>, data :: <buffer>, data-offset :: <integer>, 
     count :: <integer> /* , overlapped :: false-or(<win32-overlapped>) */ )
 => (nread :: false-or(<integer>))
  let success? = primitive-raw-as-boolean
                   (%call-c-function ("ReadFile", c-modifiers: "__stdcall")
                        (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
                         count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
                         lpOverlapped :: <raw-c-pointer>)
                     => (success? :: <raw-c-signed-int>)
                      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                       primitive-cast-raw-as-pointer
                         (primitive-machine-word-add
			    (primitive-cast-pointer-as-raw
			       (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
                             primitive-cast-pointer-as-raw
                               (integer-as-raw(data-offset)))),
                       integer-as-raw(count),
                       primitive-cast-raw-as-pointer
                         (primitive-unwrap-machine-word(actual-count-ptr)),
		       primitive-cast-raw-as-pointer(
			 /*
			 if (overlapped)
			     primitive-unwrap-machine-word(overlapped)
			 else
			 */
			   integer-as-raw(0)
			 /*
			 end if
			 */
		       ))
                    end);
  success? := success? | win32-last-error() = 38;	// ERROR_HANDLE_EOF
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
                  (primitive-unwrap-machine-word(actual-count-ptr),
                   integer-as-raw(0),
                   integer-as-raw(0)))
end function win32-read; //  -internal;

/*
define function win32-write
    (handle :: <machine-word>, data :: <buffer>, offset :: <integer>, count :: <integer>)
 => (nwritten :: false-or(<integer>))
  // If the OS call fails, it might be because of a GC read/write barrier.
  // Try the operation again a couple of times after triggering the GC
  // to blacken the object, just in case.
  // THIS IS A TEMPORARY HACK!
  win32-write-internal(handle, data, offset, count)
    | begin force-object-black(data); win32-write-internal(handle, data, offset, count) end
    | begin force-object-black(data); win32-write-internal(handle, data, offset, count) end
end function win32-write;
*/

define function win32-write  // -internal
    (handle :: <machine-word>, data :: <buffer>, data-offset :: <integer>, 
     count :: <integer> /* , overlapped :: false-or(<win32-overlapped>) */ )
 => (nwritten :: false-or(<integer>))
  let success? = primitive-raw-as-boolean
                   (%call-c-function ("WriteFile", c-modifiers: "__stdcall")
                        (handle :: <raw-c-pointer>, buffer-ptr :: <raw-c-pointer>,
                         count :: <raw-c-unsigned-long>, actual-count :: <raw-c-pointer>,
                         lpOverlapped :: <raw-c-pointer>)
                     => (success? :: <raw-c-signed-int>)
                      (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)),
                       primitive-cast-raw-as-pointer
                         (primitive-machine-word-add
			    (primitive-cast-pointer-as-raw
			       (primitive-repeated-slot-as-raw(data, primitive-repeated-slot-offset(data))), 
                             primitive-cast-pointer-as-raw
                               (integer-as-raw(data-offset)))),
                       integer-as-raw(count),
                       primitive-cast-raw-as-pointer
                         (primitive-unwrap-machine-word(actual-count-ptr)),
		       primitive-cast-raw-as-pointer(
			 /*
			 if (overlapped)
			     primitive-unwrap-machine-word(overlapped)
			 else
			 */
			   integer-as-raw(0)
			 /*
			 end if
			 */
		       ))
                    end);
  success? & raw-as-integer
               (primitive-c-unsigned-long-at
                  (primitive-unwrap-machine-word(actual-count-ptr),
                   integer-as-raw(0),
                   integer-as-raw(0)))
end function win32-write; //  -internal;

define function win32-force-output (handle :: <machine-word>) => (success? :: <boolean>)
  primitive-raw-as-boolean
    (%call-c-function ("FlushFileBuffers", c-modifiers: "__stdcall")
         (handle :: <raw-c-pointer>) => (success? :: <raw-c-signed-int>)
       (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(handle)))
      end)
end function win32-force-output;

define function win32-last-error () => (status :: <machine-word>)
  primitive-wrap-machine-word
    (%call-c-function ("GetLastError", c-modifiers: "__stdcall")
         () => (status :: <raw-c-unsigned-long>)
       ()
     end)
end function win32-last-error;

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
  let status = win32-last-error();
  %call-c-function ("FormatMessageA", c-modifiers: "__stdcall")
      (flags :: <raw-c-unsigned-long>, lpSource :: <raw-c-pointer>,
       message-id :: <raw-c-unsigned-long>, language-id :: <raw-c-unsigned-long>,
       lpBuffer :: <raw-c-pointer>, bytes :: <raw-c-unsigned-long>,
       lpArguments :: <raw-c-pointer>)
   => (count :: <raw-c-unsigned-long>)
    (integer-as-raw(FORMAT_MESSAGE_FLAGS),
     primitive-cast-raw-as-pointer(integer-as-raw(0)),
     primitive-unwrap-machine-word(status),
     integer-as-raw(FORMAT_MESSAGE_LANGUAGE),
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

// What are the proper error codes for a file access error?
define function win32-access-error? () => (access-error? :: <boolean>)
  #f
end function win32-access-error?;
