Module:	    file-system
Author:     Gary Palter
Synopsis:   Platform independent portions of the File System library API
Copyright:  1996 The Harlequin Group Limited.  All rights reserved.

define constant $pathname-separator-as-string :: <byte-string> =
  make(<byte-string>, size: 1, fill: $pathname-separator);

/// Given a pathname, returns the short path form of it
define open generic shorten-pathname (path :: <pathname>) => (shortened-path :: <pathname>);

/// Only explicit uses of <locator> in the File System library
define method expand-pathname (path :: <locator>) => (expanded-path :: <byte-string>)
  expand-pathname(as(<string>, as(<physical-locator>, path)))
end method expand-pathname;

define method shorten-pathname (path :: <locator>) => (shortened-path :: <pathname>)
  shorten-pathname(as(<string>, as(<physical-locator>, path)))
end method shorten-pathname;

define sideways method supports-list-locator?
    (locator :: <directory-locator>) => (listable? :: <boolean>)
  ~locator.locator-relative?
end method supports-list-locator?;

define sideways method list-locator
    (locator :: <directory-locator>) => (locators :: <sequence>)
  let filename = as(<string>, locator);
  let locators :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-directory
    (method (directory :: <string>, filename :: <string>, type :: <file-type>)
       let new-locator
	 = select (type)
	     #"file", #"link" =>
	       make(<native-file-locator>,
		    directory: locator,
		    name:      filename);
	     #"directory" =>
	       subdirectory-locator(locator, filename);
	   end;
       add!(locators, new-locator)
     end,
     filename);
  locators
end method list-locator;

/// Given a pathname, returns the parent directory pathname and the filename
define function split-pathname (path :: <pathname>)
 => (parent :: <byte-string>, filename :: <byte-string>)
  let path = expand-pathname(path);
  block (return)
    for (i from size(path) above 0 by -1)
      if (path[i - 1] = $pathname-separator)
        return(copy-sequence(path, end: i),
               if (i = size(path))
                 ""
               else
                 copy-sequence(path, start: i)
               end)
      end
    end;
    values("", path)
  end
end function split-pathname;

/// Creates a new pathname given a parent pathname and a filename
define function unsplit-pathname (parent :: <pathname>, filename :: <string>)
 => (path :: <byte-string>)
  let parent = expand-pathname(parent);
  let filename = as(<byte-string>, filename);
  if (size(parent) = 0)
    filename
  elseif (parent[size(parent) - 1] = $pathname-separator)
    concatenate(parent, filename)
  else
    concatenate(parent, $pathname-separator-as-string, filename)
  end
end function unsplit-pathname;


///

define method file-property (file :: <pathname>, key == #"write-date")
 => (write-date :: false-or(<date>))
  file-property(file, #"modification-date")
end method file-property;

define method file-property-setter 
    (new-write-date :: false-or(<date>), file :: <pathname>, key == #"write-date")
 => (new-write-date :: false-or(<date>))
  file-property(file, #"modification-date") := new-write-date
end method file-property-setter;

define method file-property (file :: <pathname>, key) => (property :: <object>)
  error(make(<file-system-error>,
	     format-string: "Native file system does not implement the %s property",
	     format-arguments: list(key)))
end method file-property;

define method file-property-setter (new-property :: <object>, file :: <pathname>, key)
 => (new-property :: <object>)
  error(make(<file-system-error>,
	     format-string: "Native file system cannot set the %s property",
	     format-arguments: list(key)))
end file-property-setter;

define function file-properties
    (file :: <pathname>) => (properties :: <explicit-key-collection>)
  let properties = make(<table>);
  file-properties-internal(file, properties);
  properties[#"write-date"] := properties[#"modification-date"];
  properties
end function file-properties;


///

/// Verifies that all the directories in the path to the file exist, creating them if needed.
///---*** Add a note about "foo" vs. "foo\" at end of path ...
define function ensure-directories-exist (file :: <pathname>) => (created? :: <boolean>)
  local method doit (directory :: <byte-string>) => (created? :: <boolean>)
          if (file-exists?(directory))
            #f
          //---*** What about the root???
          else
            let (parent, myself) = split-pathname(copy-sequence(directory,
                                                                end: size(directory) - 1));
            doit(parent);
            create-directory(parent, myself);
            #t
          end
        end method doit;
  let (directory, filename) = split-pathname(file);
  doit(directory)
end function ensure-directories-exist;

///---*** FINISH ME!
define function directory-contents () => ()
  error("Not yet implemented")
end function directory-contents;
