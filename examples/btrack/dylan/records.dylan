Module: btrack
Author: Carl Gay
Synopsis: Database record read/write/cache code.


// ---TODO: slot initializers (e.g., "slot foo = 0") don't work.
//          use the init-value: workaround for now.
// ---TODO: Make column-name default to slot name with any dashes converted
//          to underscores.
// ---TODO: Figure a convenient way to allow subclasses to override the
//          column name/index for slots defined in superclasses.
// ---TODO: Allow non-database slots.  Right now the save-record method
//          assumes all slots are stored in the database.


// -------------------base record machinery--------------------------

// ---TODO: required?: #t, max-length: 30, etc.
// ---TODO: pretty-name: "Foo", for use in displaying error messages about
//          a specific field.

define macro record-definer
  { define ?modifiers:* record ?:name (?superclasses) ?slots:* end }
  => { define ?modifiers record-class ?name (?superclasses) ?slots end;
       define record-slots ?name (?superclasses) ?slots end;
       define record-extras ?name (?superclasses) ?slots end; }
superclasses:
  { } => { }
  { ?superclass:expression, ... } => { ?superclass, ... }
end;

// This is for any extra non-slot slots like "table-name: foo".
//
define macro record-extras-definer
  { define record-extras ?:name (?superclasses:*) end }  // stop recursion
  => { }
  { define record-extras ?:name (?superclasses:*) 
      table-name: ?table-name:expression;
      ?more-slots:*
    end }
  => { define method record-table-name (record :: ?name) => (table-name :: <string>)
         ?table-name
       end;
       define record-extras ?name (?superclasses) ?more-slots end;  // recurse
     }
  { define record-extras ?:name (?superclasses:*) 
      ?slot-spec:*;
      ?more-slots:*
    end }
  => { define record-extras ?name (?superclasses) ?more-slots end; }  // recurse
end;

/*
define record <account> (<named-record>)
  table-name: "tbl_account";
  database slot password :: <string>,
    init-keyword: #"password",
    column-number: 5,
    column-name: "password";
end;
*/

// This basically strips out everything that "define class" doesn't handle.
//
define macro record-class-definer
  { define ?modifiers:* record-class ?:name (?superclasses:*) ?slots end }
  => { define ?modifiers class ?name (?superclasses) ?slots end }
slots:
  { } => { }
  { ?slot ; ... } => { ?slot ... }
slot:
  { table-name: ?foo:expression } => { }             // strip
  { ?modifiers slot ?slot-name:variable }
  => { ?modifiers slot ?slot-name ; }
  { ?modifiers slot ?slot-name:variable , ?keys-and-vals }
  => { ?modifiers slot ?slot-name, ?keys-and-vals ; }
modifiers:
  { } => { }
  { database ... } => { ... }                        // strip
  { ?modifier:name ... } => { ?modifier ... }
keys-and-vals:
  { } => { }
  { column-name:  ?foo:expression, ... } => { ... }  // strip
  { column-number: ?foo:expression, ... } => { ... }  // strip
  { database-type: ?foo:expression, ... } => { ... }  // strip
  { ?key:token ?foo:expression, ... } => { ?key ?foo, ... }
end macro record-class-definer;


define macro record-slots-definer
  { define ?unused:* record-slots ?:name (?superclasses:*) ?slots end }
  =>{ // This needs to be referenced from auxiliary rule sets below.
      let _record-name = ?name;
      ?slots }
slots:
  { } => { }
  { ?slot ; ... } => { ?slot ... }
slot:
  { table-name: ?foo:expression } => { }             // strip
  { ?modifiers:* slot ?slot-name:variable , ?keys-and-vals }
  => { begin
         //let _modifiers = list(?modifiers);
         // There's got to be a better way to do this...
         let (_getter, _setter, _type) = decode-slot-name(?modifiers ?slot-name);
         let _args = list(?keys-and-vals);
         add-slot-descriptor(_record-name,
                             apply(make, <slot-descriptor>,
                                   getter: _getter,
                                   setter: _setter,
                                   type: _type,
                                   _args));
       end ; }
modifiers:
  { } => { }
  { constant ... } => { constant }
  { ?other:name ... } => { ... }
keys-and-vals:
  { } => { }
  { column-name:  ?x:expression, ... } => { column-name: ?x, ... }
  { column-number: ?x:expression, ... } => { column-number: ?x, ... }
  { init-keyword: ?x:expression, ... } => { init-keyword: ?x, ... }
  { database-type: ?x:expression, ... } => { database-type: ?x, ... }
  // Note this maps to init-keyword...
  { required-init-keyword: ?x:expression, ... } => { init-keyword: ?x, ... }
  { ?key:token ?x:expression, ... } => { ... }  // remove everything else
end macro record-slots-definer;

define macro decode-slot-name
  { decode-slot-name(?:name) } => { values(?name, ?name ## "-setter", <object>) }
  { decode-slot-name(constant ?:name) } => { values(?name, #f, <object>) }
  { decode-slot-name(?:name :: ?type:expression) } => { values(?name, ?name ## "-setter", ?type) }
  { decode-slot-name(constant ?:name :: ?type:expression) } => { values(?name, #f, ?type) }
end;

// Maps classes to sequences of slot descriptors.
//
define constant $slot-descriptor-table = make(<table>);

// Describes attributes of a slot that relate to database records.
//
define class <slot-descriptor> (<object>)
  constant slot slot-getter :: <function>,
    required-init-keyword: #"getter";
  constant slot slot-setter :: false-or(<function>) = #f,
    init-keyword: #"setter";

  // Sadly, since the SQL-ODBC library doesn't allow for accessing column
  // data by name, only by position, we have to keep track of the database
  // column here.
  constant slot slot-column-number :: <integer>,
    required-init-keyword: #"column-number";

  // Must match the name of the column in the database schema where this
  // record is stored.  Is also used as the input field name in the web
  // page where this record is edited, since Dylan identifiers won't always
  // play well with web tools like Javascript.  (i.e., underscore vs dash).
  constant slot slot-column-name :: false-or(<string>) = #f,
    init-keyword: #"column-name";

  // The dylan type for values stored in the dylan record object.
  constant slot slot-type :: <type> = <object>,
    init-keyword: #"type";

  constant slot slot-init-keyword :: false-or(<symbol>) = #f,
    init-keyword: #"init-keyword";

  // A symbol representing a database type, e.g. #"varchar".  This is just
  // used for method dispatch in a few cases where the dylan type isn't specific
  // enough.  e.g., <string> maps to both VARCHAR and CHAR(n).
  constant slot slot-database-type :: false-or(<symbol>) = #f,
    init-keyword: #"database-type";

  // Whether a value must be specified for this slot/input field in the web <form>.
  constant slot slot-required? :: <boolean> = #f,
    init-keyword: #"required?";

end class <slot-descriptor>;


define method add-slot-descriptor
    (class :: <class>, desc :: <slot-descriptor>)
  let descriptors = element($slot-descriptor-table, class, default: list());
  $slot-descriptor-table[class] := add-new(descriptors, desc);
end;

// Get all slot descriptors for the given class, including those for its supers.
//
define method slot-descriptors
    (class :: <class>) => (descriptors :: <sequence>)
  let descriptors = list();
  for (c in all-superclasses(class))
    let descs = element($slot-descriptor-table, c, default: #());
    descriptors := concatenate(descs, descriptors);
  end;
  descriptors
end;


// Base class of all record types (account, bug-reports, etc).
//
define primary record <database-record> (<object>)
  // The record-id is unique across all database objects.
  database slot record-id :: <integer>,
    required-init-keyword: #"id",
    column-name: "account_id",   // ---TODO: change to record_id
    column-number: 0;
end;

define method new?
    (record :: <database-record>) => (new? :: <boolean>)
  record-id(record) = 0
end;

define primary record <modifiable-record> (<database-record>)
  // Mod-count is used to determine whether the record has been modified
  // since it was last loaded into memory.  Whenever the record is saved
  // mod-count must be incremented.  A mod-count of 0 means the record
  // has never been saved.
  database slot mod-count :: <integer>,
    init-keyword: #"mod-count",
    column-name: "mod_count",
    column-number: 1;
  database slot date-entered :: <date>,
    init-value: current-date(),
    init-keyword: #"date-entered",
    column-name: "date_entered",
    column-number: 2;
  database slot date-modified :: <date>,
    init-value: current-date(),
    init-keyword: #"date-modified",
    column-name: "date_modified",
    column-number: 3;
end;

define primary record <named-record> (<modifiable-record>)
  database slot name :: <string>,
    init-keyword: #"name",
    column-name: "name",
    column-number: 4;
  // ---TODO: This doesn't really belong here.
  database slot status :: <character>,
    init-keyword: #"status",
    column-name: "status",
    // ---TODO: Fix this!  This will only work for <account>s.
    //          Would be nice if not all records had to have the same column layout.
    column-number: 10;
end;



////
//// Loading/caching
////

define constant $record-cache = make(<table>);

define method load-record
    (class :: <class>, query :: <string>) => (record :: false-or(<database-record>))
  with-database-connection (conn)
    let statement = make(sql$<sql-statement>, text: query);
    let result-set = sql$execute(statement);
    select (size(result-set))
      0 => #f;
      1 =>
        let row = result-set[0];
        create-or-update-record(class, row);
      otherwise =>
        // ---TODO: Signal a reasonable class of error.
        error("More than one account found for query %=.", query);
    end;
  end;
end;

// Load all records of the given class that are returned by the given query.
//
define method load-records
    (class :: <class>, query :: <string>)
  with-database-connection (conn)
    let statement = make(sql$<sql-statement>, text: query);
    let result-set = sql$execute(statement);
    map-as(<vector>,
           curry(create-or-update-record, class),
           result-set)
  end;
end;

define method create-or-update-record
    (class :: <class>, row :: <sequence>)
 => (record :: <database-record>)
  let index = get-column-number(class, record-id);
  let record-id = row[index];
  assert(record-id,
         "Loaded record of class %= has no %s in element %d of its database row.",
         class, get-column-name(class, record-id), index);
  let existing-record = element($record-cache, record-id, default: #f);
  if (existing-record)
    existing-record
    //---TODO: update-record!
    //update-record(existing-record)
  else
    let new-record :: <database-record>
      = apply(make, class, compute-init-args(class, row));
    $record-cache[record-id] := new-record
  end;
end;

define method find-slot-attribute
    (class :: <class>, getter :: <function>, attribute :: <function>)
 => (attr :: <object>)
  block (return)
    for (desc in slot-descriptors(class))
      if (slot-getter(desc) == getter)
        return(attribute(desc)) end end end
end;

define method get-column-number
    (class :: <class>, getter :: <function>) => (name :: false-or(<integer>))
  find-slot-attribute(class, getter, slot-column-number)
end;

define method get-column-name
    (class :: <class>, getter :: <function>) => (name :: false-or(<string>))
  find-slot-attribute(class, getter, slot-column-name)
end;

// Generates a sequence of init args that can be passed to make for the
// creation of a database record.  Each slot descriptor specifies whether
// or not it has an init-keyword, and if it does it is included here.
// 
define method compute-init-args
    (class :: <class>, row :: <sequence>) => (init-args :: <sequence>)
  let args = make(<stretchy-vector>);
  for (desc in slot-descriptors(class))
    let init-keyword = slot-init-keyword(desc);
    if (init-keyword)
      let init-value = row[slot-column-number(desc)];
      add!(args, init-keyword);
      add!(args, db-type-to-slot-type(slot-database-type(desc) | slot-type(desc),
                                      init-value));
    end;
  end;
  args
end;

// For converting cell data read from the database to the correct type for record
// slots.  The obvious one is converting CHAR(1) to type <character>.  There may
// be a way to do this inside SQL-ODBC before it ever gets turned into a string.
//
define open generic db-type-to-slot-type
    (type :: <object>, cell-data :: <object>) => (data :: <object>);

// Default method just returns the database data unchanged.
//
define method db-type-to-slot-type
    (type :: <type>, init-value :: <object>) => (init-value :: <object>)
  init-value
end;

define method db-type-to-slot-type
    (type :: subclass(<character>), init-value :: <string>) => (init-value :: <character>)
  assert(init-value.size == 1,
         "May only convert a string of length 1 to type <character>.  String = %=.",
         init-value);
  init-value[0]
end;

// This takes precedence over the method for <string>, which trims space
// chars off the right.
//
define method db-type-to-slot-type
    (type == #"varchar", init-value :: <string>) => (init-value :: <object>)
  init-value
end;

// This forwarding method is necessary because sometimes a slot's type is
// specified as false-or(<string>) or something else that isn't a subclass
// of <string>, even though the database type is CHAR(n).  In that case
// database-type: #"char" must be used.
//
define method db-type-to-slot-type
    (type == #"char", init-value :: <object>) => (init-value :: <object>)
  db-type-to-slot-type(<string>, init-value)
end;

// Remove spaces from the end of strings, which is appropriate for CHAR(n) data,
// because the database (or ODBC?) pads strings that way.  This isn't always
// appropriate for VARCHAR or VARCHAR2 data, so in that case your "define record"
// should specify database-type: #"varchar".
//
define method db-type-to-slot-type
    (type :: subclass(<string>), init-value :: <string>) => (init-value :: <string>)
  block (return)
    let epos = init-value.size - 1;
    for (i from epos to 0 by -1)
      if (init-value[i] ~== ' ')
        return(iff(i == epos,
                   init-value,
                   copy-sequence(init-value, end: i + 1)))
      end;
    end;
    // Didn't find any non-blank characters.
    ""
  end;
end;



// --------------------------accounts------------------------------

/*
create table tbl_account (
  account_id         INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  password           CHAR(30) NOT NULL,
  email_address      CHAR(100) NOT NULL,
  email_prefs        INTEGER NOT NULL,     -- encoded bits for now
  permissions        INTEGER NOT NULL,     -- encoded bits for now
  role               CHAR(1),              -- 'D' = developer, 'Q' = QA
  status             CHAR NOT NULL         -- 'O' = Obsolete, 'A' = active
);
*/

define record <account> (<named-record>)
  table-name: "tbl_account";
  database slot password :: <string>,
    init-keyword: #"password",
    column-number: 5,
    column-name: "password";
  slot email-address :: <string>,
    init-keyword: #"email-address",
    column-number: 6,
    column-name: "email_address";
  slot email-prefs :: <integer>,
    init-keyword: #"email-prefs",
    column-number: 7,
    column-name: "email_prefs";
  slot permissions :: <integer>,
    init-keyword: #"permissions",
    column-number: 8,
    column-name: "permissions";
  slot role :: <character>,
    init-keyword: #"role",
    column-number: 9,
    column-name: "role";
end;


// This needs to come after <account>.
//
define primary class <owned-record> (<named-record>)
  slot owner :: <account>,
    init-keyword: #"account";
  slot description :: <string>,
    init-keyword: #"description";
end;


define method load-account
    (id :: <integer>) => (account :: false-or(<account>))
  // ---TODO: Caching.
  load-account(format-to-string("select * from tbl_account where account_id = %d", id));
end;

define method load-account
    (query :: <string>) => (account :: false-or(<account>))
  load-record(<account>, query)
end;

define method load-account-named
    (name :: <string>, #key password) => (account :: <account>)
  assert(size(name) > 0,
         "Attempt to load an account by name using the empty string as the name.");
  load-record(<account>,
              iff(password,
                  format-to-string("select * from tbl_account where name = '%s' and password = '%s'",
                                   name, password),
                  format-to-string("select * from tbl_account where name = '%s'",
                                   name)))
end;

define method load-all-accounts
    () => (accounts :: <sequence>)
  load-records(<account>, "select * from tbl_account");
end;



define method save-record
    (record :: <database-record>)
  // ---TODO: build the SQL string once and cache it in an each-subclass slot.
  //          (unless we're only going to write changed fields.)
  let slots = slot-descriptors(object-class(record));
  local method updater-string (slot)
          format-to-string("%s = ?", slot-column-name(slot))
        end;
  let query
    = if (new?(record))
        format-to-string("insert into %s (%s) values (%s) where account_id = %d", // ---TODO
                         record-table-name(record),
                         join(slots, ",", key: slot-column-name),
                         join(slots, ",", key: method (x) "?" end),
                         record-id(record))
      else
        format-to-string("update %s set %s where account_id = %d", //---TODO
                         record-table-name(record),
                         join(slots, ",", key: updater-string),
                         record-id(record))
      end;
  with-database-connection (conn)
    let statement = make(sql$<sql-statement>, text: query);
    local method database-slot-values (record)
            let slot-values = make(<stretchy-vector>);
            // ---TODO: This assumes that all slots get stored in the database.
            //          Need to allow for non-database slots.
            map-into(slot-values, method (slot) slot-getter(slot)(record) end, slots);
          end;
    sql$execute(statement, parameters: database-slot-values(record));
  end;
end;

// Subclasses must implement this for each "top-level" record type.
// This can be done with the table-name argument to "define record".
//
define generic record-table-name
    (record :: <database-record>) => (table-name :: <string>);

define method record-table-name
    (record :: <database-record>) => (table-name :: <string>)
  error("No record-table-name method is defined for this record's class. (%=)",
        record);
end;


