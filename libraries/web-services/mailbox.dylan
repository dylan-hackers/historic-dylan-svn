Module:    mailbox
Synopsis:  Conversion of Dylan to XML and vice versa.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

// An abstraction that concurrent services can use to exchange data.
//
define open abstract class <mailbox> (<object>)
end class <mailbox>;

define open class <mailbox-condition> (<simple-condition>)
end class <mailbox-condition>;

define open class <mailbox-warning> (<simple-warning>, <mailbox-condition>)
end class <mailbox-warning>;

define open class <mailbox-error> (<simple-error>, <mailbox-condition>)
end class <mailbox-error>;

// The error signalled when Deliver times out.
//
define open class <mailbox-delivery-timeout> (<mailbox-error>)
end class <mailbox-delivery-timeout>;

// The error signalled when Read-Data times out.
//
define open class <mailbox-read-data-timeout> (<mailbox-error>)
end class <mailbox-read-data-timeout>;

// Deliver Data to Mailbox.  Throws an error, if Data cannot be
// delivered before Timeout.
//
define open generic deliver
    (mailbox :: <mailbox>, data, #key timeout) => ();


// Wait until data arrives in Mailbox that satisfies Predicate,
// and return this data.  Throws a <mailbox-delivery-timeout> if
// the Timeout expires before data can be returned.
//
define open generic read-data
    (mailbox :: <mailbox>, predicate, #key timeout) => (object);

define open generic message-exists?
    (mailbox :: <mailbox>, predicate) => (object);

