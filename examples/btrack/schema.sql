-- To do:
-- * Implement the (commented out) status field for many objects.
--   Basically, obsolete products, modules, accounts, etc shouldn't be
--   available when records are edited (unless they are the current value).
-- * Figure out long descriptions.  BLOBs?  Extension tables?
-- * Figure out attachments.
-- * Add email policy table
--   when to send email:
--     when new bugs are created in a product you own?
--     when new bugs are created in a module you own?
--     when any bug to which you are assigned is created/modified?
--     occasionally send reminders as bugs age?  (requires background process)
-- * Figure out permissions.

-- Note: Almost no columns allow NULL values.  Rather than having to
--       special case NULL/null all the time, which is painful in Java,
--       many columns use 0 to indicate "no value".

-- status field is as follows:
-- new - not yet looked at by anyone.  may have been assigned to the default
--       owner for the set module.
-- investigating - Development is investigating
-- fixed         - Development has fixed the problem.  It's now in QA's hands.
-- not a bug     - Development determined it's not a bug.
drop   table tbl_bug_report;
create table tbl_bug_report (
  bug_report_id      INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  bug_number         INTEGER NOT NULL,      -- user-visible bug number
  status             INTEGER NOT NULL,      -- new, investigating, fixed, open, invalid (not a bug),
                                            --   testing, reopened, verified, closed
  synopsis           VARCHAR(100) NOT NULL,
  description        VARCHAR(4000) NOT NULL, -- needs extension table
  product            INTEGER NOT NULL,      -- a product_id or 0
  module             INTEGER NOT NULL,      -- a module_id or 0
  version            INTEGER NOT NULL,      -- a version_id or 0
  reported_by        INTEGER NOT NULL,      -- an account_id  (NEVER 0)
  fixed_by           INTEGER NOT NULL,      -- an account_id or 0
  fixed_in           INTEGER NOT NULL,      -- a version_id or 0
  target_version     INTEGER NOT NULL,      -- a version_id or 0
  operating_system   INTEGER NOT NULL,      -- an operating_system_id or 0
  platform           INTEGER NOT NULL,      -- a platform_id or 0
  browser            INTEGER NOT NULL,      -- a browser_id or 0
  location           VARCHAR(400) NULL,     -- e.g., URL
  priority           INTEGER NOT NULL,      -- 0 = not prioritized, 1 = highest
  severity           INTEGER NOT NULL,      -- 0 = not severitized, 1 = highest
  dev_assigned       INTEGER NOT NULL,      -- an account_id or 0
  qa_assigned        INTEGER NOT NULL,      -- an account_id or 0
  duplicate_of       INTEGER NOT NULL       -- a bug_report_id or 0
);
create unique index XPtbl_bug_report on tbl_bug_report ( bug_report_id );


drop table tbl_bug_dependencies;
create table tbl_bug_dependencies (
  bug_report_id      INTEGER NOT NULL,
  depends_on         INTEGER NOT NULL       -- a bug_report_id
);


-- Additional comments added to another record.
drop   table tbl_comment;
create table tbl_comment (
  comment_id         INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  bug_report_id      INTEGER NOT NULL,      -- references tbl_bug_report.bug_report_id
  account_id         INTEGER NOT NULL,      -- references tbl_account.account_id
  comment            VARCHAR(4000)
);
create unique index XPtbl_comment on tbl_comment ( comment_id );


drop   table tbl_version;
create table tbl_version (
  version_id         INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  product            INTEGER NOT NULL,      -- a product_id
  release_date       DATETIME,
  comment            VARCHAR(4000),
  status             CHAR NOT NULL          -- 'O' = Obsolete, 'A' = active
);
create unique index XPtbl_version on tbl_version ( version_id );



drop   table tbl_operating_system;
create table tbl_operating_system (
  opsys_id           INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  status             CHAR NOT NULL          -- 'O' = Obsolete, 'A' = active
);
create unique index XPtbl_operating_system on tbl_operating_system ( opsys_id );
-- Add or remove whatever you want from here. Just make sure the IDs are unique.
-- This doesn't try to be a complete list.
--insert into tbl_operating_system (opsys_id, name) values (1, 'Windows 95/98/Me');
--insert into tbl_operating_system (opsys_id, name) values (2, 'Windows NT4');
--insert into tbl_operating_system (opsys_id, name) values (3, 'Windows 2000');
--insert into tbl_operating_system (opsys_id, name) values (4, 'Windows XP');
--insert into tbl_operating_system (opsys_id, name) values (5, 'Linux');
--insert into tbl_operating_system (opsys_id, name) values (6, 'MacOS 8');
--insert into tbl_operating_system (opsys_id, name) values (7, 'MacOS 9');
--insert into tbl_operating_system (opsys_id, name) values (8, 'MacOS X');
--insert into tbl_operating_system (opsys_id, name) values (9, 'PS/2');
--insert into tbl_operating_system (opsys_id, name) values (10, 'Solaris');


drop   table tbl_browser;
create table tbl_browser (
  browser_id         INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  status             CHAR NOT NULL          -- 'O' = Obsolete, 'A' = active
);
create unique index XPtbl_browser on tbl_browser ( browser_id );
-- Add or remove whatever you want from here.  Just make sure the IDs are unique.
-- This doesn't try to be a complete list.
--insert into tbl_browser (browser_id, name) values (1, 'Internet Explorer 3');
--insert into tbl_browser (browser_id, name) values (2, 'Internet Explorer 4');
--insert into tbl_browser (browser_id, name) values (3, 'Internet Explorer 5');
--insert into tbl_browser (browser_id, name) values (4, 'Internet Explorer 6');
--insert into tbl_browser (browser_id, name) values (5, 'Netscape 3');
--insert into tbl_browser (browser_id, name) values (6, 'Netscape 4');
--insert into tbl_browser (browser_id, name) values (7, 'Netscape 6');
--insert into tbl_browser (browser_id, name) values (8, 'Opera');
--insert into tbl_browser (browser_id, name) values (9, 'ICab');


drop   table tbl_platform;
create table tbl_platform (
  platform_id        INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  status             CHAR NOT NULL          -- 'O' = Obsolete, 'A' = active
);


drop   table tbl_product;
create table tbl_product (
  product_id         INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  description        VARCHAR(4000),
  owner              INTEGER NOT NULL,      -- The account_id of the user with primary responsibility.
  status             CHAR NOT NULL          -- 'O' = Obsolete, 'A' = active
);
create unique index XPtbl_product on tbl_product ( product_id );


drop   table tbl_module;
create table tbl_module (
  module_id          INTEGER NOT NULL,
  mod_count          INTEGER NOT NULL,
  date_entered       DATETIME NOT NULL,
  date_modified      DATETIME NOT NULL,
  name               CHAR(30) NOT NULL,
  description        VARCHAR(4000),
  owner              INTEGER NOT NULL,      -- The account_id of the user with primary responsibility.
  status             CHAR NOT NULL,         -- 'O' = Obsolete, 'A' = active
  product            INTEGER NOT NULL       -- a product_id
);
create unique index XPtbl_module on tbl_module ( module_id );


-- Map products and modules.  A product may have any number of modules and
-- a module may belong to more than one product.
-- ---TODO: This isn't used yet.  A simplifying assumption was made that
--          each module will only belong to a single product for now.
drop   table tbl_product_module;
create table tbl_product_module (
  product_id         INTEGER NOT NULL,
  module_id          INTEGER NOT NULL
);


drop   table tbl_account;
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
create unique index xpktbl_account_id on tbl_account ( account_id );
create unique index xpktbl_account_name on tbl_account ( name );
create unique index xpktbl_account_email on tbl_account ( email_address );


drop   table tbl_log;
create table tbl_log (
  log_id          INTEGER NOT NULL,
  date_entered    DATETIME NOT NULL,
  modified_by     INTEGER NOT NULL,        -- references tbl_account.account_id.  use string instead?
  description     VARCHAR(400) NOT NULL
);
create unique index XPtbl_log on tbl_log ( log_id );


-- For configuration variables.  Should only ever have one row.
drop   table tbl_config;
create table tbl_config (
  -- This column is used to generate unique IDs for all the other objects
  -- in the database.  In Oracle there is special support for this, with:
  --   create sequence object_id start with 0 increment by 200 nocycle
  --   select object_id.nextval from dual
  next_unique_id INTEGER NOT NULL,

  -- This is the next user-visible bug number to be assigned when a new bug
  -- report is submitted.
  next_bug_number INTEGER NOT NULL
);
-- Saving first 100 values for future use, just in case.
insert into tbl_config ( next_unique_id, next_bug_number ) values ( 100, 1 );
