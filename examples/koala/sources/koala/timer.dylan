Module:    httpi
Author:    Gail Zacharias
Copyright: Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


//---TODO: might be useful as a separate library.

/* For now it's not useful at all...

define class <task> (<object>)
  constant slot task-function :: <function>,
    required-init-keyword: function:;
  constant slot task-time :: <integer>,
    required-init-keyword: time:;
  slot task-next :: false-or(<task>),
    required-init-keyword: next:;
end;

define constant *timer-lock* = make(<lock>);
define variable *timer-queue* :: false-or(<task>) = #f;
define variable *timer-thread* :: false-or(<thread>) = #f;
define variable *timer-time* :: <integer> = $maximum-integer;

define variable $start-days :: <integer>
  = begin
      let (time, days) = current-timestamp();
      ignore(time);
      days
    end;

// Could be improved, but it's best we can do using stock fns
// Return a number of seconds from some starting point, as an integer.
define function current-time () => (seconds :: <integer>)
  let (millisecs :: <integer>, days :: <integer>) = current-timestamp();
  let secs :: <integer> = floor/(millisecs, 1000);
  let days :: <integer> = days - $start-days;
  days * 60 * 60 * 24 + secs
end;

define function enqueue-task (fn :: <function>, seconds :: <integer>)
  let time :: <integer> = current-time() + seconds;
  with-lock (*timer-lock*)
    unless (*timer-thread*)
      *timer-thread* := make(<thread>,
                             name: "Timer", function: timer-top-level);
    end;
    for (t = *timer-queue* then t.task-next, prev = #f then t,
         while: t & t.task-time <= time)
    finally
      let task = make(<task>, function: fn, time: time, next: t);
      if (prev)
        prev.task-next := task;
      else
        *timer-queue* := task;
      end;
    end for;
    *timer-time* := *timer-queue*.task-time;
  end;
end enqueue-task;

//---TODO: wish could sleep longer and just get poked when enqueue a new task...
define function timer-top-level ()
  sleep(1);
  iterate loop ()
    let time :: <integer> = current-time();
    // don't bother with lock if not time yet.
    let fn = *timer-time* <= time &
             with-lock (*timer-lock*)
               let t = *timer-queue*;
               when (t & t.task-time <= time)
                 *timer-queue* := t.task-next;
                 *timer-time* := if (*timer-queue*)
                                   *timer-queue*.task-time;
                                 else
                                   $maximum-integer
                                 end;
                 t.task-function;
               end when;
             end with-lock;
    when (fn)
      block ()
        fn();
      exception (c :: <error>)
        #f
      end;
      loop();
    end;
  end iterate;
  timer-top-level();
end;

*/

