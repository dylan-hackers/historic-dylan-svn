Module:    event-queue
Synopsis:  A queue for holding events that can be waited upon.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// An <event-queue> holds a list of events. Threads can push
// items on to the queue, and other threads can wait for items
// to exist in the queue and then pop them off. If the queue is
// empty callers will block waitiing for an item to exist.
define class <event-queue> (<object>)
  slot %internal-queue :: <deque> = make(<deque>);
  slot %queue-lock :: <lock> = make(<lock>);
  slot event-item-queued;
end class <event-queue>;

define method initialize(q :: <event-queue>, #key #all-keys)
  q.event-item-queued := make(<notification>, lock: q.%queue-lock);
end method initialize;

define method push-on-queue(q :: <event-queue>, object)
 => ()
  with-lock(q.%queue-lock)
    when(q.%internal-queue.empty?)
      release-all(q.event-item-queued)
    end when;

    push-last(q.%internal-queue, object);
  end with-lock;
end method push-on-queue;

define method get-from-queue(q :: <event-queue>)
 => (r :: <object>)
  with-lock(q.%queue-lock)
    while(q.%internal-queue.empty?)
      wait-for(q.event-item-queued)
    end while;

    pop(q.%internal-queue);
  end with-lock;
end method get-from-queue;

define method queue-empty?(q :: <event-queue>)
 => (r :: <boolean>)
  with-lock(q.%queue-lock)
    q.%internal-queue.empty?;
  end with-lock;  
end method queue-empty?;
