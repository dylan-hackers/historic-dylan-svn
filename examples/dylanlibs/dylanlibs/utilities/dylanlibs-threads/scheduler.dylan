Module:    scheduler
Synopsis:  Server for monitoring race event information
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// A <schedule> contains a sequence of items that need to be actioned
// in some manner. These items are scheduled to run at a particular
// time of day. 
//
// When an items scheduled time arrives the items function is pushed onto
// a function execution <blocking-queue>. Threads wait on this queue for
// items to be pushed. When an item exists on the queue it is run. Multiple
// threads can be waiting on this queue if desired.
//
// The scheduler is notified of changes in system time allowing it to 
// adjust its sleep period if it is adjusted.
//
define class <schedule> (<object>)
  constant slot schedule-lock :: <lock> = make(<lock>);

  // The internal list os scheduled items. It should never be
  // accessed by external code.
  slot %schedule-items :: <stretchy-vector> = make(<stretchy-vector>);

  // Returns a copy of the scheduled items, sorted in order of
  // the time that the items are supposed to run.
  virtual constant slot schedule-items :: <sequence>;
  
  // Set to #t if the schedule should abort. Internal method not to
  // be set by user code.
  slot %schedule-abort? :: <boolean> = #f;

  // User accessible indicator to abort. Ensures that the schedule lock
  // is available when setting.
  virtual slot schedule-abort? :: <boolean>;

  // A <semaphore> that keeps track of changes made to the
  // schedule. When a change is made the semaphore is incremented.
  // When all changes have been processed it is zero.
  constant slot schedule-changed-semaphore :: <semaphore> = make(<semaphore>);

  // The function execution queue. Items that are to be actioned
  // have their function pushed on this queue to be eventually run
  // by a thread.
  constant slot schedule-execution-queue :: <blocking-queue> = make(<blocking-queue>);
end class <schedule>;

define method schedule-abort?(s :: <schedule>)
 => (r :: <boolean>)
  with-lock(s.schedule-lock)
    s.%schedule-abort?
  end with-lock;
end method schedule-abort?;

define method schedule-abort?-setter(value :: <boolean>, s :: <schedule>)
 => (r :: <boolean>)
  with-lock(s.schedule-lock)
    s.%schedule-abort? := value;
  end with-lock;
end method schedule-abort?-setter;

// Increment the semaphore count so the scheduler loop can react
// to changes made to the schedule.
define method notify-schedule-changed(s :: <schedule>)
 => ()
  release(s.schedule-changed-semaphore);
end method notify-schedule-changed;

// Wait for the schedule to change
define method wait-for-schedule-changed(s :: <schedule>, #key timeout)
  wait-for(s.schedule-changed-semaphore, timeout: timeout); 
end method wait-for-schedule-changed;

define class <schedule-item> (<object>)
  // Date item is to be actioned
  constant slot schedule-item-action-date :: <date>, required-init-keyword: action-date:;

  // Function to be executed when scheduled time comes around
  constant slot schedule-item-function :: <function>, required-init-keyword: function:;
end class <schedule-item>;

// Return a duration for the time between now and when the 
// scheduled item is to be actioned.
define method duration-until-action(item :: <schedule-item>)
 => (r :: <duration>)
  item.schedule-item-action-date - current-date();
end method duration-until-action;

// Add a scheduled item to the schedule.
define method add-schedule-item!(s :: <schedule>, item :: <schedule-item>)
 => ()
  with-lock(s.schedule-lock)
    s.%schedule-items := add!(s.%schedule-items, item);
    notify-schedule-changed(s);
  end with-lock;
end method add-schedule-item!;

// Remove a scheduled item from the schedule
define method remove-schedule-item!(s :: <schedule>, item :: <schedule-item>)
 => ()
  with-lock(s.schedule-lock)
    s.%schedule-items := remove!(s.%schedule-items, item);
    notify-schedule-changed(s);
  end with-lock;
end method remove-schedule-item!;

define method remove-schedule-item-no-notify!(s :: <schedule>, item :: <schedule-item>)
 => ()
  with-lock(s.schedule-lock)
    s.%schedule-items := remove!(s.%schedule-items, item);
  end with-lock;
end method remove-schedule-item-no-notify!;

// Return a copy of a sorted list of the scheduled items.
define method schedule-items(s :: <schedule>)
 => (r :: <sequence>)
  with-lock(s.schedule-lock)
    sort(s.%schedule-items,
         test: method(a :: <schedule-item>, b :: <schedule-item>)
                 a.duration-until-action < b.duration-until-action
               end);
  end with-lock;             
end method schedule-items;

// Schedule a function to be run at a given time.
define method schedule-at-time(s :: <schedule>,
                               function :: <function>, 
                               time :: <date>)
 => ()
  add-schedule-item!(s,
                     make(<schedule-item>,
                          function: function,
                          action-date: time));
end method schedule-at-time;
  
// Schedule a function to be run after a specified duration.
// Note that this causes the function to be scheduled to run at
// a time that is current-date() + the duration.
define method schedule-after-duration(s :: <schedule>,
                                      function :: <function>, 
                                      duration :: <day/time-duration>)
 => ()
  add-schedule-item!(s,
                     make(<schedule-item>,
                          function: function,
                          action-date: current-date() + duration));
end method schedule-after-duration;
                               
define constant $zero-duration = encode-day/time-duration(0, 0, 0, 0, 0);

// The main scheduler loop
define method scheduler(s :: <schedule>, #key execution-threads = 1)
 => ()
  for(num from 1 to execution-threads)
    make(<thread>, 
         name: format-to-string("Execution thread %s", num),
         function: curry(execute-queue-thread, s));
  end for;

  block(exit-scheduler)
    while(#t)
      with-abort-handler()
        let items = s.schedule-items;
        unless(empty?(items))
          for(item in items)
            let duration = item.duration-until-action;
            when(duration <= $zero-duration)
              push-on-queue(s.schedule-execution-queue, item.schedule-item-function);
              remove-schedule-item-no-notify!(s, item);
            end when;
          end for;
        end unless;

        when(s.schedule-abort?)
          exit-scheduler();
        end when;

        schedule-sleep(s);
      end with-abort-handler;
    end while;
  end block;

  // Cause execution threads to shut down.
  for(num from 1 to execution-threads)
    push-on-queue(s.schedule-execution-queue, #f);
  end for;
end method scheduler;

// Sleep for the number of seconds remaining until the next schedule item
// is due, or return when a change to the schedule is made, or the system
// time changes.
define method schedule-sleep(schedule :: <schedule>)
 => ()
  let items = schedule.schedule-items;
  let seconds = 
    if(empty?(items))
      #f
    else
      let duration = items.first.duration-until-action;
      let (days, hours, minutes, seconds) = decode-duration(duration);
      days * 24 * 60 * 60 + hours * 60 * 60 + minutes * 60 + seconds;      
    end if;
  when(~seconds | seconds > 0)
    wait-for-schedule(schedule, seconds);
  end when;
end method schedule-sleep;

// Wait for the given number of seconds or until notifications the
// scheduler is interested in are released.
define method wait-for-schedule(schedule :: <schedule>, 
                                seconds :: false-or(<integer>))
 => ()
//  debug-message("Waiting for %s seconds", seconds);

  unless(wait-for-schedule-changed(schedule, timeout: seconds))
//    debug-message("Timed out");
  end unless;
end method wait-for-schedule;

// Thread function that monitors the execution queue looking
// for items to execute. If the item on the queue is #f then the
// thread function exits.
define method execute-queue-thread(s :: <schedule>)
  let queue :: <blocking-queue> = s.schedule-execution-queue;
  block(exit)
    until(s.schedule-abort?)
      let function = get-from-queue(queue);
      if(function)
        function();
      else
        exit();
      end if;
    end until;
  end block;
end method execute-queue-thread;

// Start a schedule with the given number of execution threads.
define method start-schedule(s :: <schedule>, #key execution-threads = 1) 
 => ()  
  local method time-changed-function()
    while(#t)
      wait-for-time-change();
      notify-schedule-changed(s);
//      debug-message("Time changed");
    end while;
  end method;

  start-system-time-monitor();
  make(<thread>, name: "Time Changed Function", function: time-changed-function);

  scheduler(s, execution-threads: execution-threads);
end method start-schedule;

define variable *scheduler-count* = 0;
define variable *scheduler-count-lock* = make(<lock>);

define method get-next-scheduler-count()
 => (r :: <integer>)
  with-lock(*scheduler-count-lock*)
    *scheduler-count* := *scheduler-count* + 1;
  end with-lock;
end method get-next-scheduler-count;

define method start-schedule-on-thread(s :: <schedule>) 
 => (t :: <thread>)  
  make(<thread>, 
       function: curry(start-schedule, s), 
       name: format-to-string("Scheduler thread %s", get-next-scheduler-count()));
end method start-schedule-on-thread;

define method stop-schedule(s :: <schedule>)
 => ()
  s.schedule-abort? := #t;
  notify-schedule-changed(s);
end method stop-schedule;


