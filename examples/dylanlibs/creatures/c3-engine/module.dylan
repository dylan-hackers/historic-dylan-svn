Module:    dylan-user
Synopsis:  Interface to the Creatures 3 engine
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define module c3-engine
  use functional-dylan;
  use machine-word;
  use machine-integer-user;
  use c-ffi;
  use win32-common, exclude: { <point> };
  use win32-kernel, exclude: { beep };

  // Add binding exports here.
  export c3-engine-version,
    <creatures-engine>,
    connect,
    disconnect,
    with-creatures-engine,
    raw-execute-caos,
    raw-c3-caos,
    raw-docker-caos,
    execute-caos,
    c3-caos,
    engine-memory-handle,
    engine-memory-pointer,
    engine-request-event,
    engine-result-event,
    engine-mutex,
    with-mutex;
end module c3-engine;
