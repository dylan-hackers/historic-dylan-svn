Module: btrack
Author: Carl Gay
Synopsis:  account-related pages and tags (including login/logout)

define method load-account
    (id :: <integer>) => (account :: false-or(<account>))
  load-record(id)
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


define page list-accounts-page (<btrack-page>)
    (url: "/list-accounts.dsp",
     source: document-location("dsp/list-accounts.dsp"))
end;

define page edit-account-page (<btrack-page>)
    (url: "/edit-account.dsp",
     source: document-location("dsp/edit-account.dsp"))
end;

// ---TODO: abstract this out to a mixin class, <require-login-mixin>
//
define method respond-to-get (page :: <edit-account-page>,
                              request :: <request>,
                              response :: <response>)
  let session = get-session(request);
  let record-id = get-query-value("id", as: <integer>)
                    | error("Link contained invalid 'id' parameter.  This is a bug.");
  let record = iff(zero?(record-id),  // indicates new record being created.
                   make(<account>, id: next-record-id()),
                   load-account(record-id)
                     | get-attribute(session, $edit-record-key));
  // Store the record in the session in case we need to redirect to the
  // login page before editing.
  set-attribute(session, $edit-record-key, record);
  if (logged-in?(page, request))
    dynamic-bind (*record* = record)
      next-method();
    end;
  else
    force-login(*edit-account-page*, request, response)
  end;
end;

define method respond-to-post (page :: <edit-account-page>,
                               request :: <request>,
                               response :: <response>)
  let descs = slot-descriptors(<account>);  // <record-page> should know what class
  let bindings = make(<string-table>); // maps input name to parsed value
  let field-name = #f;
  let field-value = #f;
  block ()
    let record :: <account> = get-edit-record(request);
    // Update the record with values from the page.
    for (desc in descs)
      field-name := slot-column-name(desc);
      field-value := get-form-value(field-name, as: slot-type(desc));
      when (field-value)
        validate-form-field(page, desc, field-value);
        let f = slot-setter(desc);
        // ---TODO: determine whether any slots changed.  If not, no need to save the record.
        f & f(field-value, record);
      end;
    end;
    save-record(record);
    // ---TODO: temporary
    note-form-error("Account modified.");
    respond-to-get(*list-accounts-page*, request, response);
  exception (err :: <invalid-form-field-exception>)
    note-form-error("Error while processing the %= field.  %=",
                    field-name, err);
    next-method();
  end;
end;

define method validate-form-field
    (page :: <edit-account-page>, slot :: <slot-descriptor>, input)
  let column-name = slot-column-name(slot);
  select (column-name by \=)
    "password" =>
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


