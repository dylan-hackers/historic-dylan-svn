Module:    httpi
Synopsis:  Some globals that don't belong anywhere else in particular.
           Most are configurable in the koala-config.xml file.
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// TODO: The follow 3 should probably be per vhost.

define variable *mime-type-map* :: <table> = make(<table>);


// Since logging is done on a per-vhost basis, this hack is needed
// to make logging work before vhosts are initialized.
define variable *temp-log-target*
  = make(<stream-log-target>, stream: *standard-output*);

// Command-line arguments parser.  The expectation is that libraries that use
// and extend koala (e.g., wiki) may want to add their own <option-parser>s to
// this before calling koala-main().
define variable *argument-list-parser* :: <argument-list-parser>
  = make(<argument-list-parser>);
