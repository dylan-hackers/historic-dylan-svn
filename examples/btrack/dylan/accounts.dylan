Module: btrack
Author: Carl Gay
Synopsis:  btrack account records


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
    (id :: <integer>) => (account :: <account>)
  // ---TODO: Caching.
  load-account(format-to-string("select * from tbl_account where account_id = %d", id));
end;

define method load-account
    (query :: <string>) => (account :: <account>)
  load-record(<account>, query)
end;

define method load-account-named
    (name :: <string>) => (account :: <account>)
  assert(size(name) > 0,
         "Attempt to load an account by name using the empty string as the name.");
  load-record(<account>,
              format-to-string("select * from tbl_account where name = '%s'",
                               name))
end;



