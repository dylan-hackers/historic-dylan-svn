module: multi-thread-test


define variable *counter* :: <integer> = 1;

define constant $lock = make(<lock>);
define function counter () => (res :: <integer>)
  with-lock($lock)
    *counter* := *counter* + 1;
    *counter*;
  end;
end;


define class <thread-server> (<object>)
  slot lock = make(<lock>);
  slot threads :: <collection> = make(<stretchy-vector>);
end;

define function thread-worker (ts :: <thread-server>, count :: <integer>) => ()
  with-lock (ts.lock)
    add!(ts.threads, count)
  end;
  sleep(1);
  with-lock (ts.lock)
    remove!(ts.threads, count)
  end;
  collect-garbage()
end;

define function main () => ()
  let ts = make(<thread-server>);
  while(#t)
    let count = counter();
    make(<thread>, function: curry(thread-worker, ts, count));
    format-out("Started %d thread [%d]\n", count, ts.threads.size);
  end;
end;

begin
  main();
end;