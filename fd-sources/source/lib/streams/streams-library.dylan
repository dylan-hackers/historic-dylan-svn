Module:       dylan-user
Synopsis:     Define streams library and modules
Author:       Scott McKay, Marc Ferguson
Copyright:    Original Code is Copyright (c) 1994-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library streams
  use functional-dylan;
  use threads;
  export streams;
  export streams-internals;
end library streams;


define module streams
  use streams-protocol,
    export: all;
  use dylan-extensions,
    export: { <byte>, element-type };
  use functional-dylan,
    export: { <byte-character>, <unicode-character> };
  use byte-vector, 
    export: 
      { <byte-vector> };

  // Basic stream class
  create <basic-stream>;

  // Conditions
  create 
      <stream-position-error>,
      <stream-closed-error>,
      <stream-not-writable>,
      <stream-not-readable>,
    <file-error>,
      <file-exists-error>,
      <file-does-not-exist-error>,
      <invalid-file-permissions-error>;

  // Reading from streams
  create read-character,
         read-text,
         read-text-into!;

  // Writing to streams
  create write-text;

  // Convenience functions
  create read-to,
         read-through,
         read-to-end,
         skip-through;

  // Line-oriented functions
  create read-line,
         read-line-into!,
         write-line,
         new-line;

  // Positionable streams
  create 
    <basic-positionable-stream>,
    <position-type>,
    <stream-position>;

  // Locking streams
  create 
    \with-stream-locked,
    stream-lock, stream-lock-setter,
    // obsolete but still used, actually these don't really work but
    // code still apparently depends on them 
    lock-stream, unlock-stream, stream-locked?;

  // Buffers
  create <buffer>,
         <buffer-index>,
         buffer-start, buffer-start-setter,
         buffer-end, buffer-end-setter,
         buffer-next, buffer-next-setter,
         buffer-dirty?, buffer-dirty?-setter,
         buffer-position, buffer-position-setter,
         buffer-size,
         buffer-subsequence,
         copy-into-buffer!,
         copy-from-buffer!;

  // Buffered streams
  create <buffered-stream>,
         get-input-buffer,     do-get-input-buffer,
         release-input-buffer, do-release-input-buffer,
         next-input-buffer,    do-next-input-buffer,
         \with-input-buffer,
         input-available-at-source?, do-input-available-at-source?,
         get-output-buffer,     do-get-output-buffer,
         release-output-buffer, do-release-output-buffer,
         next-output-buffer,    do-next-output-buffer,
         \with-output-buffer,
         force-output-buffers,  do-force-output-buffers;

  // File streams
  create <file-stream>,
         type-for-file-stream,
         \with-open-file,
         stream-locator,
         writable-file-stream-position-setter;

  // Multi-buffered file streams
  create 
    <buffer-vector>,
    <multi-buffered-stream>,
    multi-buffered-stream-position-setter,
    write-4-aligned-bytes, write-8-aligned-bytes,
    read-4-aligned-bytes, read-8-aligned-bytes,
    multi-buffer-working-set,
    multi-buffer-reads,
    multi-buffer-bytes,
    multi-buffer-total-working-set,
    multi-buffer-total-reads,
    multi-buffer-total-bytes;

  // Sequence streams
  create \with-output-to-string,
         <sequence-stream>,
         <string-stream>,
         <byte-string-stream>,
         <unicode-string-stream>,
         type-for-sequence-stream,
         stream-limit;

  // Wrapper streams
  create <wrapper-stream>,
         inner-stream, inner-stream-setter,
         outer-stream, outer-stream-setter;
end module streams;


define module streams-internals
  use functional-dylan;
  use simple-format;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use byte-vector;
  use threads;

  use streams, export: all;

  // hacks
  export force-object-black;

  // Basic stream classes
  export <typed-stream>,
         <general-typed-stream>,
         <byte-element-stream>,
         <byte-char-element-stream>;

  // Efficient querying direction
  export 
    readable?, writable?, closed?, read-only?, write-only?, read-write?;
  // Conditions
  export end-of-stream-value,
         stream-error-requested-position,
         stream-error-size-of-stream,
	 ensure-readable, ensure-writable;

  // Querying streams
  export stream-sequence-class,
         stream-direction;

  // Positionable streams
  export current-position, current-position-setter,
	 initial-position,
         final-position;

  // Buffers and buffered streams
  export <single-buffered-stream>,
	 <double-buffered-stream>,
         ensure-input-buffer, ensure-output-buffer,
	 coerce-to-element,
	 coerce-from-element,
	 coerce-to-sequence,
	 coerce-from-sequence,
	 stream-input-buffer,  stream-input-buffer-setter,
	 stream-output-buffer, stream-output-buffer-setter,
         stream-shared-buffer, stream-shared-buffer-setter;

  // File streams
  export <general-file-stream>,
         <byte-file-stream>,
         <byte-char-file-stream>;

  // File streams
  export <general-multi-buffered-stream>,
         <byte-multi-buffered-stream>,
         <byte-char-multi-buffered-stream>;

  // 
  // Sequence streams
  export clear-contents,
         stream-limit-setter,
         stream-sequence;

  // Stream access paths
  export <external-stream-accessor>,
         <external-stream>,
         platform-accessor-class,
         new-accessor,
         accessor, accessor-setter,
	 accessor-open,
	 accessor-open?,
	 accessor-close,
         accessor-force-output,
         accessor-wait-for-completion,
	 accessor-newline-sequence,
         accessor-preferred-buffer-size,
         accessor-fd,
	 accessor-set-file-position,
         accessor-file-position,
         accessor-file-size,
	 accessor-synchronize,
	 accessor-read-into!,
	 accessor-write-from,
         *open-accessors*;

  export file-handle;

  // "High performance"
  export read-skip,
         write-fill;

  // Kludges for tcp-streams
  // emulator "inherited slot" wants to look at the setters, and
  // gets confused when they're not exported
  // TODO: ---- remove these
  export stream-direction-setter,
         stream-element-type-setter;
end module streams-internals;
