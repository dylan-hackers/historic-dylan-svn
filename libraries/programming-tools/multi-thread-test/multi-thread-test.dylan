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
  //let barf = format-to-string("%s", "FOOOO");
  format-out("Hello from %d\n",count);
  //sleep(modulo(random(100000), 3));
  //format(*standard-output*, "Hello from\n");
  format-out("Hello from\n"); //,count);
  //write(*standard-output*, "foobar\n");
  //collect-garbage();
  //sleep(1);
  with-lock (ts.lock)
    remove!(ts.threads, count)
  end;
  //format-out("End from %d\n",count);
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
      //write(*standard-output*, "Received condition\n");
      sleep(1);
    end;
  end;
end;

begin
  main();
/*  for(i from 0 below 10000000)
    lock-stream(*standard-output*);
    unlock-stream(*standard-output*);
    end */
end;

define open generic spawn-thread(i);

define method spawn-thread(i) => ()
  make(<thread>, function: method() sleep(i) end);
  values();
end;
/*
begin
  while(#t)
    block()
      spawn-thread(1);
      collect-garbage();
    exception (c :: <condition>)
      sleep(1);
    end;
  end
//  main();
end;


*/
