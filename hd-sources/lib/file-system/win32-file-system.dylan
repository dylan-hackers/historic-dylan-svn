Module:	    file-system
Author:     Gary Palter
Synopsis:   Win32 implementation of the File System library API
Copyright:  1996 The Harlequin Group Limited.  All rights reserved.


define constant $pathname-separator :: <character> = '\\';
define constant $alternate-separator :: <character> = '/';

///---*** Explanation, please
define method expand-pathname (path :: <string>) => (expanded-path :: <byte-string>)
  let path = as(<byte-string>, path);
  // Convert all uses of slash (/) to backslash (\), the canonical Win32 pathname separator
  member?($alternate-separator, path)
    & (path := map-as(<byte-string>,
		      method (ch)
			if (ch = $alternate-separator) $pathname-separator
			else ch end
		      end,
		      path));
  with-stack-path (path-buffer)
    with-stack-dword (unused-address)
      let path-length = raw-as-integer
			  (%call-c-function ("GetFullPathNameA", c-modifiers: "__stdcall")
			       (fileName :: <raw-c-pointer>, 
				bufferSize :: <raw-c-unsigned-long>,
				bufferPtr :: <raw-c-pointer>,
				filePartPtrPtr :: <raw-c-pointer>)
			    => (bufferUsed :: <raw-c-unsigned-long>)
			     (primitive-string-as-raw(path),
			      integer-as-raw($MAX_PATH),
			      primitive-string-as-raw(path-buffer),
			      primitive-cast-raw-as-pointer
				(primitive-unwrap-machine-word(unused-address)))
			   end);
      if (path-length > $MAX_PATH | path-length = 0)
	win32-file-error("expand", "%s", path)
      else
	copy-sequence(path-buffer, end: path-length)
      end
    end
  end
end method expand-pathname;

define method shorten-pathname (path :: <string>) => (shortened-path :: <byte-string>)
  let path = as(<byte-string>, path);
  // Convert all uses of slash (/) to backslash (\), the canonical Win32 pathname separator
  member?($alternate-separator, path)
    & (path := map-as(<byte-string>,
		      method (ch)
			if (ch = $alternate-separator) $pathname-separator
			else ch end
		      end,
		      path));
  with-stack-path (path-buffer)
    let path-length = raw-as-integer
			(%call-c-function ("GetShortPathNameA", c-modifiers: "__stdcall")
			   (fileName :: <raw-c-pointer>, 
			    bufferPtr :: <raw-c-pointer>,
			    bufferSize :: <raw-c-unsigned-long>)
			   => (bufferUsed :: <raw-c-unsigned-long>)
			   (primitive-string-as-raw(path),
			    primitive-string-as-raw(path-buffer),
			    integer-as-raw($MAX_PATH))
			end);
    if (path-length = 0)
      if (win32-last-error() = $ERROR_NOT_SUPPORTED)
	path
      else
	win32-file-error("shorten", "%s", path)
      end
    elseif (path-length > $MAX_PATH)
      win32-file-error("shorten", "%s", path)
    else
      copy-sequence(path-buffer, end: path-length)
    end
  end
end method shorten-pathname;


///

define function file-exists? (file :: <pathname>) => (exists? :: <boolean>)
  let file = expand-pathname(file);
  if (primitive-machine-word-not-equal?
	(%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
	     (path :: <raw-byte-string>)
	  => (exists? :: <raw-c-unsigned-long>)
	   (primitive-string-as-raw(as(<byte-string>, file)))
	 end,
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    #t
  elseif (begin
	    let status = win32-last-error();
	    status = $ERROR_FILE_NOT_FOUND 
	      | status = $ERROR_PATH_NOT_FOUND
	      | status = $ERROR_NOT_READY
	  end)
    #f
  else
    #t
  end
end function file-exists?;

define inline-only function attributes-to-file-type (attributes :: <machine-word>)
 => (file-type :: <file-type>)
  //---*** How do we determine if a file is a shortcut?
  if (primitive-machine-word-logbit?
	(integer-as-raw($FILE_ATTRIBUTE_DIRECTORY_BIT),
	 primitive-unwrap-machine-word(attributes)))
    #"directory"
  else
    #"file"
  end
end function attributes-to-file-type;

define function file-type (file :: <pathname>) => (file-type :: <file-type>)
  let file = expand-pathname(file);
  with-file-attributes (file, fa)
    attributes-to-file-type(fa-attributes(fa))
  end
end function file-type;

/// link-target?

define function delete-file (file :: <pathname>) => ()
  let file = expand-pathname(file);
  // NOTE: Turn off the read-only flag or we won't be able to delete the file!
  file-property(file, #"writeable?") := #t;
  unless (primitive-raw-as-boolean
            (%call-c-function ("DeleteFileA", c-modifiers: "__stdcall")
                 (path :: <raw-byte-string>)
              => (deleted? :: <raw-c-signed-int>)
               (primitive-string-as-raw(as(<byte-string>, file)))
             end))
    win32-file-error("delete", "%s", file)
  end
end function delete-file;

define function copy-file
    (source :: <pathname>, destination :: <pathname>,
     #key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = expand-pathname(source);
  let destination = expand-pathname(destination);
  // NOTE: Contrary to the documentation, CopyFile won't copy over
  // an existing read-only file so we need to delete it manually.
  if (if-exists == #"replace" & file-exists?(destination))
    delete-file(destination)
  end;
  unless (primitive-raw-as-boolean
            (%call-c-function ("CopyFileA", c-modifiers: "__stdcall")
                 (sourcePath :: <raw-byte-string>, destPath :: <raw-byte-string>,
                  failIfExists :: <raw-c-signed-int>)
              => (copied? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, source)),
		primitive-string-as-raw(as(<byte-string>, destination)),
                integer-as-raw
		  (select (if-exists)
		     #"signal" => -1;
		     #"replace" => 0;
		   end))
	     end))
    win32-file-error("copy", "%s to %s", source, destination)
  end
end function copy-file;

define function rename-file
    (source :: <pathname>, destination :: <pathname>,
     #Key if-exists :: <copy/rename-disposition> = #"signal")
 => ()
  let source = expand-pathname(source);
  let destination = expand-pathname(destination);
  // NOTE: We can't use MoveFileEx which provides options to control
  // the move if the target exists because MoveFileEx isn't implemented
  // in Windows 95.  (When this code was originally written, the 
  // documentation for MoveFileEx failed to mention that fact.  Sigh)
  if (if-exists == #"replace" & file-exists?(destination))
    delete-file(destination)
  end;
  unless (primitive-raw-as-boolean
	    (%call-c-function ("MoveFileA", c-modifiers: "__stdcall")
		 (sourcePath :: <raw-byte-string>, destPath :: <raw-byte-string>)
	      => (moved? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, source)),
		primitive-string-as-raw(as(<byte-string>, destination)))
	     end))
    win32-file-error("rename", "%s to %s", source, destination)
  end
end function rename-file;


/// "Standard" properties not implemented on this platform:
///    author

/// "Standard" properties not settable on this platform:
///    size, creation-date, access-date, modification-date, readable?, executable?

/// Even though it's not implemented, we'll provide a stub just to be "nice"...
define method file-property
    (file :: <pathname>, key == #"author") => (author :: false-or(<string>))
  #f
end method file-property;

define method file-property (file :: <pathname>, key == #"size") => (file-size :: <integer>)
  with-file-attributes (file, fa)
    fa-size-low(fa)
  end
end method file-property;

define inline-only function date-if-valid (native-clock :: <machine-word>) 
 => (date :: false-or(<date>))
  filetime-valid?(native-clock) & make(<date>, native-clock: native-clock)
end function date-if-valid;

define method file-property
    (file :: <pathname>, key == #"creation-date") => (creation-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-creation-time(fa))
  end
end method file-property;

define method file-property
    (file :: <pathname>, key == #"access-date") => (access-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-access-time(fa))
  end
end method file-property;

define method file-property
    (file :: <pathname>, key == #"modification-date")
 => (modification-date :: false-or(<date>))
  with-file-attributes (file, fa)
    date-if-valid(fa-write-time(fa))
  end
end method file-property;

define method file-property
    (file :: <pathname>, key == #"readable?") => (readable? :: <boolean>)
  #t
end method file-property;

define inline-only function writeable? (attrs :: <machine-word>) => (writeable? :: <boolean>)
  ~ primitive-machine-word-logbit?
      (integer-as-raw($FILE_ATTRIBUTE_READONLY_BIT),
       primitive-unwrap-machine-word(attrs))
end function writeable?;

define method file-property
    (file :: <pathname>, key == #"writeable?") => (writeable? :: <boolean>)
  let file = expand-pathname(file);
  let attributes = primitive-wrap-machine-word
                     (%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
                          (path :: <raw-byte-string>)
                       => (attributes :: <raw-c-unsigned-long>)
                        (primitive-string-as-raw(as(<byte-string>, file)))
                      end);
  if (primitive-machine-word-not-equal?
	(primitive-unwrap-machine-word(attributes),
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    writeable?(attributes)
  else
    win32-file-error("get attributes of", "%s", file)
  end
end method file-property;

define method file-property-setter 
    (new-writeable? :: <boolean>, file :: <pathname>, key == #"writeable?")
 => (new-writeable? :: <boolean>)
  let file = expand-pathname(file);
  let attributes = primitive-wrap-machine-word
                     (%call-c-function ("GetFileAttributesA", c-modifiers: "__stdcall")
                          (path :: <raw-byte-string>)
                       => (attributes :: <raw-c-unsigned-long>)
                        (primitive-string-as-raw(as(<byte-string>, file)))
                      end);
  if (primitive-machine-word-not-equal?
	(primitive-unwrap-machine-word(attributes),
	 integer-as-raw($INVALID_HANDLE_VALUE)))
    let old-writeable? = writeable?(attributes);
    if (new-writeable? ~= old-writeable?)
      unless (primitive-raw-as-boolean
                (%call-c-function ("SetFileAttributesA", c-modifiers: "__stdcall")
		     (path :: <raw-byte-string>, attributes :: <raw-c-unsigned-long>)
		  => (success? :: <raw-c-signed-int>)
		   (primitive-string-as-raw(as(<byte-string>, file)),
                    primitive-machine-word-boole
                      (#"boole-xor",
                       primitive-unwrap-machine-word(attributes),
                       integer-as-raw($FILE_ATTRIBUTE_READONLY)))
		 end))
        win32-file-error("set attributes of", "%s", file)
      end
    end
  else
    win32-file-error("get attributes of", "%s", file)
  end;
  new-writeable?
end method file-property-setter;

define method file-property
    (file :: <pathname>, key == #"executable?") => (executable? :: <boolean>)
  let file = expand-pathname(file);
  let executable? = primitive-raw-as-boolean
                      (%call-c-function ("SHGetFileInfoA", c-modifiers: "__stdcall")
			   (pszPath :: <raw-byte-string>,
			    dwFileAttributes :: <raw-c-unsigned-long>,
			    psfi :: <raw-c-pointer>,
			    cbFileInfo :: <raw-c-unsigned-int>,
			    uFlags :: <raw-c-unsigned-int>)
			=> (result :: <raw-c-unsigned-long>)
			 (primitive-string-as-raw(as(<byte-string>, file)),
			  integer-as-raw(0),
			  primitive-cast-raw-as-pointer(integer-as-raw(0)),
			  integer-as-raw(0),
			  integer-as-raw($SHGFI_EXETYPE))
		      end);
  if (executable?)
    #t
  elseif (begin
	    //---*** NOTE: SHGetFileInfoA doesn't reset GetLastError to $NO_ERROR
	    //---*** if the file exists but isn't an executable.  Consequently, we
	    //---*** can't check to see if an error actually occured and signal it
	    //---*** appropriately; instead, we'll just always claim the file isn't
	    //---*** executable.  (Sigh)
	    // let status = win32-last-error();
	    // status = $ERROR_BAD_EXE_FORMAT
	    //   | status = $ERROR_ACCESS_DENIED
	    //   | status = $NO_ERROR
	    #t
	  end)
    #f
  else
    win32-file-error("get attributes of", "%s", file)
  end
end method file-property;

define function file-properties-internal
    (file :: <pathname>, properties :: <explicit-key-collection>) => ()
  let file = expand-pathname(file);
  with-file-attributes (file, fa)
    properties[#"size"] := fa-size-low(fa);
    properties[#"creation-date"] := date-if-valid(fa-creation-time(fa));
    properties[#"access-date"] := date-if-valid(fa-access-time(fa));
    properties[#"modification-date"] := date-if-valid(fa-write-time(fa));
    properties[#"readable?"] := #t;
    properties[#"writeable?"] := writeable?(fa-attributes(fa));
  end;
  properties[#"executable?"] := file-property(file, #"executable?");
end function file-properties-internal;


///

define function create-directory (parent :: <pathname>, name :: <string>)
 => (directory :: <pathname>)
  let directory = unsplit-pathname(parent, name);
  if (primitive-raw-as-boolean
        (%call-c-function ("CreateDirectoryA", c-modifiers: "__stdcall")
	     (dirPathname :: <raw-byte-string>, securityAttributes :: <raw-c-pointer>)
          => (created? :: <raw-c-signed-int>)
           (primitive-string-as-raw(directory),
            primitive-cast-raw-as-pointer(integer-as-raw(0)))
         end))
    unsplit-pathname(directory, "")	// Ensure return value has no filename component
  else
    win32-file-error("create the directory", "%s", directory)
  end
end function create-directory;

///---*** Should we add an 'if-not-empty?' keyword argument?
define function delete-directory (directory :: <pathname>) => ()
  let directory = expand-pathname(directory);
  unless (primitive-raw-as-boolean
	    (%call-c-function ("RemoveDirectoryA", c-modifiers: "__stdcall")
		 (dirPathname :: <raw-byte-string>)
	      => (deleted? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, directory)))
	     end))
    win32-file-error("delete", "%s", directory)
  end
end function delete-directory;

define function home-directory () => (home-directory :: <pathname>)
  let drive = environment-variable("HOMEDRIVE");
  let path = environment-variable("HOMEPATH");
  unsplit-pathname(as(<string>, concatenate(drive, path)), "")
end function home-directory;

define function working-directory () => (working-directory :: <pathname>)
  let cdb-size :: <integer> = 1024;
  let curdir-buffer :: <byte-string> = make(<byte-string>, size: cdb-size, fill: '\0');
  let curdir-size :: <integer>
    = raw-as-integer(%call-c-function ("GetCurrentDirectoryA", c-modifiers: "__stdcall")
		         (nBufferLength :: <raw-c-unsigned-long>,
			  lpBuffer :: <raw-byte-string>)
		      => (nCurDirsize :: <raw-c-unsigned-long>)
		       (integer-as-raw(cdb-size),
			primitive-string-as-raw(curdir-buffer))
		     end);
  if (curdir-size > cdb-size)
    // Value was too large to fit in our initial buffer but GetCurrentDirectoryA
    // tells us how long it actually is so we can just make a buffer large enough
    let cdb-size :: <integer> = curdir-size + 1;
    curdir-buffer := make(<byte-string>, size: cdb-size, fill: '\0');
    curdir-size :=
      raw-as-integer(%call-c-function ("GetCurrentDirectoryA", c-modifiers: "__stdcall")
		         (nBufferLength :: <raw-c-unsigned-long>,
			  lpBuffer :: <raw-byte-string>)
		      => (nCurDirsize :: <raw-c-unsigned-long>)
		       (integer-as-raw(cdb-size),
			primitive-string-as-raw(curdir-buffer))
		     end)
  end;
  if (curdir-size > 0)
    unsplit-pathname(copy-sequence(curdir-buffer, end: curdir-size), "")
  else
    win32-file-error("get the current directory", #f)
  end
end function working-directory;

//---*** Explanation of path\ vs. path ...
define function working-directory-setter (new-working-directory :: <pathname>)
 => (new-working-directory :: <pathname>)
  let (directory, filename) = split-pathname(new-working-directory);
  unless (primitive-raw-as-boolean
	    (%call-c-function ("SetCurrentDirectoryA", c-modifiers: "__stdcall")
		 (lpPathName :: <raw-byte-string>)
	      => (currentDirectorySet? :: <raw-c-signed-int>)
	       (primitive-string-as-raw(as(<byte-string>, directory)))
	     end))
    win32-file-error("set the current directory", "to %s", directory)
  end;
  directory
end function working-directory-setter;

define function temp-directory () => (temp-directory :: false-or(<pathname>))
  with-stack-path (path-buffer)
    let path-size = raw-as-integer
                      (%call-c-function ("GetTempPathA", c-modifiers: "__stdcall")
			   (bufferSize :: <raw-c-unsigned-long>,
			    bufferPtr :: <raw-c-pointer>)
			=> (bufferUsed :: <raw-c-unsigned-long>)
			 (integer-as-raw($MAX_PATH),
			  primitive-string-as-raw(path-buffer))
		       end);
    if (path-size ~= 0)
      copy-sequence(path-buffer, end: path-size)
    else
      win32-file-error("find temporary directory", #f)
    end
  end
end function temp-directory;

define function root-directories () => (roots :: <sequence>)
  with-stack-path (path-buffer)
    let path-size = raw-as-integer
		      (%call-c-function ("GetLogicalDriveStringsA", c-modifiers: "__stdcall")
			   (bufferSize :: <raw-c-unsigned-long>,
			    bufferPtr :: <raw-c-pointer>)
			=> (bufferUsed :: <raw-c-unsigned-long>)
			 (integer-as-raw($MAX_PATH),
			  primitive-string-as-raw(path-buffer))
		       end);
    if (path-size ~= 0)
      let roots = list();
      let start = 0;
      block (return)
	while (start < path-size)
	  let fini = start;
	  until (path-buffer[fini] = '\0')
	    fini := fini + 1
	  end;
	  if (start = fini)
	    return()
	  else
	    roots := add!(roots, copy-sequence(path-buffer, start: start, end: fini));
	    start := fini + 1
	  end
	end
      end;
      reverse(roots)
    else
      win32-file-error("find root directories", #f)
    end
  end
end function root-directories;


///

//--- Explanation, including explanation of path/ vs. path and arguments to F...
define function do-directory (f :: <function>, directory :: <pathname>) => ()
  let (directory, filename) = split-pathname(directory);
  let wild-pathname = unsplit-pathname(directory, "*.*");
  let find-handle = primitive-wrap-machine-word(integer-as-raw($INVALID_HANDLE_VALUE));
  with-stack-win32-find-data (wfd, directory)
    block ()
      find-handle := primitive-wrap-machine-word
		       (primitive-cast-pointer-as-raw
			  (%call-c-function ("FindFirstFileA", c-modifiers: "__stdcall")
			       (lpFileName :: <raw-byte-string>,
				lpFindFileData :: <raw-c-pointer>)
			    => (hFindFile :: <raw-c-pointer>)
			     (primitive-string-as-raw(wild-pathname),
			      primitive-cast-raw-as-pointer
				(primitive-unwrap-machine-word(wfd)))
			   end));
      if (primitive-machine-word-equal?
	    (primitive-unwrap-machine-word(find-handle),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	win32-file-error("start listing of", "%s", directory)
      end;
      let have-file? :: <boolean> = #t;
      while (have-file?)
	let attributes :: <machine-word> = win32-find-data-attributes(wfd);
	let filename :: <byte-string> = win32-find-data-filename(wfd);
	f(directory,
	  filename,
	  attributes-to-file-type(attributes));
	unless (primitive-raw-as-boolean
		  (%call-c-function ("FindNextFileA", c-modifiers: "__stdcall")
		       (hFindFile :: <raw-c-pointer>, lpFindFileData :: <raw-c-pointer>)
		    => (closed? :: <raw-c-signed-int>)
		     (primitive-cast-raw-as-pointer
			(primitive-unwrap-machine-word(find-handle)),
		      primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(wfd)))
		   end))
	  if (win32-last-error() = $ERROR_NO_MORE_FILES)
	    have-file? := #f
	  else
	    win32-file-error("continue listing of", "%s", directory)
	  end
	end
      end;
    cleanup
      if (primitive-machine-word-not-equal?
	    (primitive-unwrap-machine-word(find-handle),
	     integer-as-raw($INVALID_HANDLE_VALUE)))
	%call-c-function ("FindClose", c-modifiers: "__stdcall")
	    (hFindFile :: <raw-c-pointer>) => (closed? :: <raw-c-signed-int>)
	  (primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(find-handle)))
        end
      end
    end
  end
end function do-directory;
