Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Fundamental mode

define open class <fundamental-mode> (<major-mode>)
end class <fundamental-mode>;

begin
  gethash(*keyword->major-mode*,   #"fundamental") := <fundamental-mode>
end;

define method mode-name
    (mode :: <fundamental-mode>) => (name :: <byte-string>)
  "Fundamental"
end method mode-name;


/// Text mode

define open class <text-mode> (<fundamental-mode>)
end class <text-mode>;

begin
  gethash(*keyword->major-mode*,   #"text") := <text-mode>;
  gethash(*file-type->major-mode*, #"text") := <text-mode>
end;

define method mode-name
    (mode :: <text-mode>) => (name :: <byte-string>)
  "Text"
end method mode-name;

define method source-file-type
    (mode :: <text-mode>) => (file-type)
  #"text"
end method source-file-type;
