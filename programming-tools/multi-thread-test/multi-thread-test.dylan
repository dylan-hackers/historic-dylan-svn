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
  format-out("Hello from %d\n",count);
  collect-garbage();
  //sleep(modulo(random(100000), 5));
  //format-out("Hello from %d\n",count);
  with-lock (ts.lock)
    remove!(ts.threads, count)
  end;
  format-out("End from %d\n",count);
end;

define function main () => ()
  let ts = make(<thread-server>);
  while(#t)
    block()
      let count = counter();
      make(<thread>, function: curry(thread-worker, ts, count));
      with-lock(ts.lock)
        format-out("Started %d thread [%d]\n", count, ts.threads.size);
      end;
    exception (c :: <condition>)
      format-out("Received condition %=\n", c);
      sleep(1);
    end;
  end;
end;

begin
  main();
end;
