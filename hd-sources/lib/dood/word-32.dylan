module: dood
author: jonathan bachrach

define inline method dood-compute-instance-size
    (dood :: <dood>, object == <machine-word>) => (address :: <address>)
  1 + 1 // 32 bits
end method;

//define constant dood-read-machine-word     = dood-read-word;
define constant dood-read-machine-word-at  = dood-read-word-at;
define constant dood-write-machine-word    = dood-write-word;
//define constant dood-write-machine-word-at = dood-write-word-at;

// eof
