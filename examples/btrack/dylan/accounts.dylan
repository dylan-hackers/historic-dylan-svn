Module: btrack
Author: Carl Gay
Synopsis:  account-related pages and tags (including login/logout)

define method load-account
    (id :: <integer>) => (account :: false-or(<account>))
  load-record(<account>, id)
end;

define method load-account
    (query :: <string>) => (account :: false-or(<account>))
  load-record(<account>, query)
end;

define method load-account-named
    (name :: <string>, #key password) => (account :: <account>)
  assert(size(name) > 0,
         "Attempt to load an account by name using the empty string as the name.");
  let query
    = iff(password,
          format-to-string("select * from tbl_account where name = '%s' and password = '%s'",
                           name, password),
          format-to-string("select * from tbl_account where name = '%s'",
                           name));
  load-record(<account>, query)
    | error("Couldn't find an account matching this query: %=", query);
end;


define page list-accounts-page (<btrack-page>)
    (url: "/list-accounts.dsp",
     source: document-location("dsp/list-accounts.dsp"))
end;

define page edit-account-page (<edit-record-page>)
    (url: "/edit-account.dsp",
     source: document-location("dsp/edit-account.dsp"))
end;

define method validate-record-field
    (page :: <edit-account-page>, account :: <account>, slot :: <slot-descriptor>, input)
  let column-name = slot-column-name(slot);
  select (column-name by \=)
    "password", "username" =>
      input.size < 4
        & note-field-error(column-name, "Must be at least 4 characters long.");
    otherwise => ;
  end;
end;

define named-method list-accounts in btrack
    (page)
  load-records(<account>, "select * from tbl_account")
end;

define tag show-email-address in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  format(output-stream(response), "%s", email-address(current-row() | *record*));
end;

define tag show-current-username in btrack
    (page :: <btrack-page>, response :: <response>)
    ()
  let account = current-account(response);
  format(output-stream(response), "%s",
         iff(account,
             name(account),
             "(not logged in)"));
end;


