Module: btrack

/*
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

define class <bug-report> (<database-record>)
  slot status :: <integer>, init-keyword: #"status";
  slot synopsis :: <string>, init-keyword: #"synopsis";
  slot description :: <string>, init-keyword: #"description";
  slot product :: <integer>, init-keyword: #"product";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";
  slot, init-keyword: #"";

*/

