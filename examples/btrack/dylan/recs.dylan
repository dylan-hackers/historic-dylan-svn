Module: btrack
Author: Carl Gay

define primary record <named-record> (<modifiable-record>)
  database slot name :: <string>,
    init-value: " ",
    init-keyword: #"name",
    column-name: "name",
    column-number: 4;
  // ---TODO: This doesn't really belong here.
  // 'O' = Obsolete, 'A' = Active
  database slot status :: <character>,
    init-value: 'A',
    init-keyword: #"status",
    column-name: "status",
    column-number: 5;
end;


define record <account> (<named-record>)
  type-name: "account";
  pretty-name: "account";
  table-name: "tbl_account";
  database slot password :: <string>,
    init-value: " ",      //---TODO: what?
    init-keyword: #"password",
    column-number: 6,
    column-name: "password";
  slot email-address :: <string>,
    init-value: " ",      //---TODO: what?
    init-keyword: #"email-address",
    column-number: 7,
    column-name: "email_address";
  slot email-prefs :: <integer>,
    init-value: 0,       //---TODO: nyi
    init-keyword: #"email-prefs",
    column-number: 8,
    column-name: "email_prefs";
  slot permissions :: <integer>,
    init-value: 0,       //---TODO: nyi
    init-keyword: #"permissions",
    column-number: 9,
    column-name: "permissions";
  // 'D' = Developer, 'Q' = QA (should make it a bitmask so can be both)
  slot role :: <character>,
    init-value: 'D',       //---TODO: nyi
    init-keyword: #"role",
    column-number: 10,
    column-name: "role";
end;


define primary record <browser> (<named-record>)
    type-name: "browser";
    pretty-name: "browser";
    table-name: "tbl_browser";
end;

define primary record <platform> (<named-record>)
    type-name: "platform";
    pretty-name: "platform";
    table-name: "tbl_platform";
end;

define primary record <operating-system> (<named-record>)
    type-name: "operating-system";
    pretty-name: "operating system";
    table-name: "tbl_operating_system";
end;

// This needs to come after <account>.
//
define primary record <owned-record> (<named-record>)
  slot description :: <string>,
    init-value: " ",       //---TODO: nyi
    init-keyword: #"description",
    column-number: 6,
    column-name: "description";
  slot owner :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"owner",
    column-number: 7,
    column-name: "owner";
end;

define primary record <product> (<owned-record>)
  type-name: "product";
  pretty-name: "product";
  table-name: "tbl_product";
end;

define primary record <module> (<owned-record>)
  type-name: "module";
  pretty-name: "module";
  table-name: "tbl_module";
  slot product :: <product>,
    init-function: curry(class-prototype, <product>),
    init-keyword: #"product",
    column-number: 8,
    column-name: "product_id";
end;

define primary record <version> (<named-record>)
  type-name: "version";
  pretty-name: "version";
  table-name: "tbl_version";
  slot product :: <product>,
    init-function: curry(class-prototype, <product>),
    init-keyword: #"product",
    column-number: 6,
    column-name: "product_id";
  slot release-date :: <date>,
    init-value: current-date(),
    init-keyword: #"release-date",
    column-number: 7,
    column-name: "release_date";
  slot comment :: <string>,
    init-value: " ",
    init-keyword: #"comment",
    column-number: 8,
    column-name: "comment",
    database-type: #"varchar";
end;

define constant $bug-priority-none = 0;
define constant $bug-status-new = 0;
define constant $bug-severity-none = 0;

define primary record <bug-report> (<modifiable-record>)
  type-name: "bug-report";
  pretty-name: "bug report";
  table-name: "tbl_bug_report";
  slot bug-number :: <integer>,
    init-value: 0,
    init-keyword: #"bug-number",
    column-number: 4,
    column-name: "bug_number";
  slot status :: <integer>,
    init-value: $bug-status-new,
    init-keyword: #"status",
    column-number: 5,
    column-name: "status";
  slot synopsis :: <string>,
    init-value: " ",
    init-keyword: #"synopsis",
    column-number: 6,
    column-name: "synopsis",
    database-type: #"varchar";
  slot description :: <string>,
    init-value: " ",
    init-keyword: #"description",
    column-number: 7,
    column-name: "description",
    database-type: #"varchar";
  slot product :: <product>,
    init-function: curry(class-prototype, <product>),
    init-keyword: #"product",
    column-number: 8,
    column-name: "product";
  slot module :: <module>,
    init-function: curry(class-prototype, <module>),
    init-keyword: #"module",
    column-number: 9,
    column-name: "module";
  slot version :: <version>,
    init-function: curry(class-prototype, <version>),
    init-keyword: #"version",
    column-number: 10,
    column-name: "version";
  slot reported-by :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"reported-by",
    column-number: 11,
    column-name: "reported_by";
  slot fixed-by :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"fixed-by",
    column-number: 12,
    column-name: "fixed_by";
  slot fixed-in :: <version>,
    init-function: curry(class-prototype, <version>),
    init-keyword: #"fixed-in",
    column-number: 13,
    column-name: "fixed_in";
  slot target-version :: <version>,
    init-function: curry(class-prototype, <version>),
    init-keyword: #"target-version",
    column-number: 14,
    column-name: "target_version";
  slot operating-system :: <operating-system>,
    init-function: curry(class-prototype, <operating-system>),
    init-keyword: #"operating-system",
    column-number: 15,
    column-name: "operating_system";
  slot platform :: <platform>,
    init-function: curry(class-prototype, <platform>),
    init-keyword: #"platform",
    column-number: 16,
    column-name: "platform";
  slot browser :: <browser>,
    init-function: curry(class-prototype, <browser>),
    init-keyword: #"browser",
    column-number: 17,
    column-name: "browser";
  slot location :: <string>,  // a URL
    init-value: " ",
    init-keyword: #"location",
    column-number: 18,
    column-name: "location";
  slot priority :: <integer>,
    init-value: $bug-priority-none,
    init-keyword: #"priority",
    column-number: 19,
    column-name: "priority";
  slot severity :: <integer>,
    init-value: $bug-severity-none,
    init-keyword: #"severity",
    column-number: 20,
    column-name: "severity";
  slot dev-assigned :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"dev-assigned",
    column-number: 21,
    column-name: "dev_assigned";
  slot qa-assigned :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"qa-assigned",
    column-number: 22,
    column-name: "qa_assigned";
  // This creates a circularity if we try to initialize it to a bug report prototype
  slot duplicate-of :: <bug-report>,
    //init-function: curry(class-prototype, <bug-report>),
    init-keyword: #"duplicate-of",
    column-number: 23,
    column-name: "duplicate_of";
end record <bug-report>;

define method initialize-record
    (record :: <bug-report>)
  duplicate-of(record) := class-prototype(<bug-report>);
  next-method();
end;

define primary record <comment> (<modifiable-record>)
  type-name: "comment";
  pretty-name: "comment";
  table-name: "tbl_comment";
  slot bug-report :: <bug-report>,
    init-function: curry(class-prototype, <bug-report>),
    init-keyword: #"bug-report",
    column-number: 4,
    column-name: "bug_report_id";
  slot account :: <account>,       // not the owner, just the commenter.
    init-function: curry(class-prototype, <account>),
    init-keyword: #"account",
    column-number: 5,
    column-name: "account_id";
  slot comment :: <string>,
    init-value: " ",
    init-keyword: #"comment",
    column-number: 6,
    column-name: "comment",
    database-type: #"varchar";
end;

define primary record <log-entry> (<database-record>)
  type-name: "log-entry";
  pretty-name: "log entry";
  table-name: "tbl_log";
  slot date-entered :: <date>,
    init-value: current-date(),
    init-keyword: #"date-entered",
    column-number: 1,
    column-name: "date_entered";
  slot modified-by :: <account>,
    init-function: curry(class-prototype, <account>),
    init-keyword: #"modified-by",
    column-number: 2,
    column-name: "modified_by";
  slot description :: <string>,
    init-value: " ",
    init-keyword: #"description",
    column-number: 3,
    column-name: "description",
    database-type: #"varchar";
end;

define method save-record
    (record :: <bug-report>)
  bug-number(record) := next-bug-number();
  reported-by(record) := current-account(get-session(*request*));
  next-method();
end;

define method save-record
    (record :: type-union(<module>, <version>))
  product(record) := get-query-value("product_id", as: <product>);
  next-method();
end;


define function create-admin-account
    ()
  let now = current-date();
  //---TODO: fix some of these init values.
  let admin-account = make(<account>,
                           id: next-record-id(),
                           mod-count: -1,
                           date-entered: now,
                           date-modified: now,
                           name: "admin",
                           status: 'A',              // correct?
                           password: "admin",
                           email-address: "foo@bar", // fix me
                           email-prefs: 0,
                           permissions: 0,
                           role: 'A');               // correct?
  save-record(admin-account);
end;

define method initialize-database
    (conn :: sql$<connection>)
  let accounts = query-integer(conn, "select count(1) from tbl_account");
  if (accounts = 0)
    // initialize database
    create-admin-account();
  end;
end;

define constant $bug-number-lock = make(<lock>);

define function next-bug-number
    () => (bugnum :: <integer>)
  with-lock ($bug-number-lock)
    with-database-connection (conn)
      let bugnum = query-integer(conn, "select next_bug_number from tbl_config");
      update-db(conn, "update tbl_config set next_bug_number = next_bug_number + 1");
      bugnum
    end
  end
end;

