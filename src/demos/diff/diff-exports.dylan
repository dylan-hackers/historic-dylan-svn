module: dylan-user
rcs-header: $Header: /scm/cvs/src/demos/diff/diff-exports.dylan,v 1.1.1.1.26.1 2003/10/23 20:43:58 housel Exp $

define library diff
  use dylan;
  use io;
  use system;
  use collection-extensions;
end library diff;

// The module name "diff" is already used by the collection-extensions
// module that contains the actual differencing algorithm
//
define module diff-program
  use dylan;
  use extensions, import: {main, %main};
  use streams, import: {read-line, force-output};
  use file-system, import: {<file-stream>};
  use standard-io, import: {*standard-output*};
  use format, import: {format};
  use sequence-diff;
end module diff-program;
