Module:       dylan-user
Synopsis:     Common Extensions library definition
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Version:      $HostName$

define library common-extensions
  use functional-extensions,
    export: { streams-protocol, 
	      locators-protocol,
	      finalization, 
	      simple-random,
              simple-profiling };
  use transcendentals, 
    export: { transcendentals };
  use byte-vector,
    export: { byte-vector };
  use machine-word,
    export: { machine-word };
  export common-extensions;
  export simple-io;
end library common-extensions;

define module common-extensions
  use functional-extensions,
    export: { <stream>,
              <bottom>,
              <format-string-condition>,
                <stack-overflow-error>,
                <integer-overflow-error>,
              <stretchy-object-vector>,
              <object-deque>,
              <simple-condition>,
              <stretchy-sequence>,
              <string-table>,
              false-or,
              ignorable,
              ignore,
              \iterate,
              one-of,
              remove-all-keys!,
              rest,
              subclass,
	      \when,

              $unsupplied, unsupplied, unsupplied?, supplied?,
	      $unfound,    unfound,    unfound?,    found?,
              true?, false?,
	      concatenate!,
	      condition-to-string,
	      difference,
	      position,
	      fill-table!,
	      find-element,
	      find-value,
	      float-to-string,
	      integer-to-string,
	      number-to-string,
	      string-to-integer,
	      machine-word-to-string,
	      string-to-machine-word,
	      \profiling, \profiling-keywords, \profiling-results,
	      do-with-profiling, profiling-type-result,
	      \table-definer,

              application-name,
              application-filename,
              application-arguments,
              tokenize-command-line,
              exit-application,
              register-application-exit-function };
  use simple-debugging,
    export: { \assert,
              \debug-assert,
              debug-message };
  use simple-format,
    export: { format-to-string };
  use byte-vector,
    export: { <byte-vector> };
end module common-extensions;

define module simple-io
  use simple-format,
    export: { format-out };
end module simple-io;
