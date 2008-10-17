Logging Library
~~~~~~~~~~~~~~~

.. This file uses the reStructured Text markup language, which can be rendered
   as (X)HTML, LateX, etc.  See http://docutils.sourceforge.net for details.

.. contents:

Basic Usage
===========

Log to stdout::

  define constant $log
    = make(<logger>,
           name: "my-app",
           formatter: "%{millis} %{level} [%{thread}] - %{message}");
  add-target($log, $stdout-log-target);
  log-info($log, "My-app starting with args %s", application-arguments());

The above results in log lines like this::

  12345 INFO [Main Thread] - My-app starting with args blah

Make another logger for debugging server requests::

  define constant $request-log
    = make(<logger>, name: "my-app.debug.request");

There are several things to notice about the above setup:

  * Loggers have no log targets by default.  The simplest way to add a
    target is to add a pre-existing target such as $stdout-log-target or 
    $stderr-log-target.

  * Different loggers are associated by name.  In this example the logger
    named "my-app" is an ancestor of the one named "my-app.debug.request"
    because the first dotted name component matches.

  * No targets were added to the my-app.debug.request logger.  Since all
    log messages sent to a child are also sent to its ancestors (but see
    logger-additive?-setter), anything logged to the my-app.debug.request
    logger will be passed along to the my-app logger.

    So what's the benefit of having both loggers?  You can enabled/disable
    them separately at runtime.  Also, if for example you wanted to log
    debug messages to a separate file  you could add a target to the
    my-app.debug logger.

Log to a file::

  add-target($log, make(<rolling-file-log-target>,
                        pathname: "/var/log/my-app.log"));

The log file will be rolled immediately if it exists and is not zero length.
If you don't want it to be rolled on startup, pass ``roll: #f`` to ``make``
in the above call.

Loggers may be disabled with ``logger-enabled?(logger) := #f``.

Formatters
==========

Each ``<logger>`` has a ``<log-formatter>`` that determines how to format
each line in the logging stream.  Make one like this::

  make(<log-formatter>, pattern: "...");

The log formatter pattern is similar to a format control string except it
has a short and long form for each format directive.  Here are the defined
format directives:

=====  ===========  ===================================================
Short  Long         Description
=====  ===========  ===================================================
%d     %{date:fmt}  Current date.  In the long form, fmt is any string
                    acceptable as the first argument to format-date.
%l     %{level}     Log level.  e.g., INFO, DEBUG, ERROR, etc
%m     %{message}   Log message, as passed to log-info, log-debug etc.
%p     %{pid}       Current process ID.  (Not yet implemented.)
%r     %{millis}    Milliseconds since application started.
%t     %{thread}    Current thread name.
%%     None         The % character.
=====  ===========  ===================================================

All format directives, in either short or long form, accept a numeric
argument immediately following the % character.  If provided, the numeric
argument specifies the minimum width of the field.  If the numeric argument
is positive then the displayed value will be left justified and padded
with spaces on the right if necessary.  If negative, the displayed value
will be right justified and padded with spaces on the left if needed.

Logging non-<string> Objects
============================

TBD

Creating a Specialized Log Target
=================================

TBD


