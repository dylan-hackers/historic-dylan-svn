module: turboblog
author: turbo24prg

define object-tests (participant) in turboblog end;

define class <blog-participant> (<object>)
  slot participant-blog :: <blog>,
    init-keyword: participant-blog:;
  slot participant-user :: <blog-user>,
    required-init-keyword: participant-user:;
  slot participant-role :: <symbol>,
    required-init-keyword: participant-role:;
end;

define action-tests
 (add-participant, edit-participant, remove-participant, list-participants)
in turboblog end;

define method add-participant (blog :: <blog>, participant :: <blog-participant>)
 => (participant :: <blog-participant>);
  blog.participants[participant.participant-user.username] := participant;
  participant.participant-blog := blog;
  participant;
end;

define method find-participant (blog :: <blog>, username :: <string>)
 => (participant :: false-or(<blog-participant>));
  element(blog.participants, username, default: #f)
end;

define method remove-participant (blog :: <blog>, participant :: <blog-participant>)
 => (participant :: <blog-participant>);
  remove-key!(blog.participants, participant.participant-user.username);
  participant;
end;

define tag show-participant-role in turboblog (page :: <turboblog-page>)
 ()
  if (*participant*)
    output("%s", escape-xml(*participant*.participant-role));
  end if;
end;

define tag show-participant-permanent-link in turboblog (page :: <turboblog-page>) 
 ()  
  if (*participant*)
    output("%s", permanent-link(*participant*));
  end if;
end;

define body tag with-participant-user in turboblog
 (page :: <turboblog-page>, do-body :: <function>)
 ()
  dynamic-bind(*user* = *participant*.participant-user)
    do-body();  
  end;
end;

define variable *edit-participant-page* =
  make(<turboblog-page>, source: "edit-participant.dsp");

define variable *view-participant-page* =
  make(<turboblog-page>, source: "view-participant.dsp");
  
define variable *view-participants-page* =
  make(<turboblog-page>, source: "view-participants.dsp");

define variable *remove-participant-page* =
  make(<turboblog-page>, source: "remove-participant.dsp");

define variable *non-participating-user-page* =
  make(<turboblog-page>, source: "non-participating-user.dsp");


define method respond-to (type == #"post", page :: <edit-participant-page>)

// action
  let edit? :: <boolean> = (*action* = #"edit-participant");
  let add? :: <boolean> = (*action* = #"add-participant");

// form
  let user = get-query-value("user");
  let role = get-query-value("role");

// errors
  let errors = #();

// checks
  if (add?)
    case
      ~ instance?(user, <string>) | user = "" =>
        errors := add!(errors, #"user");
      ~ find-user(user) =>
        errors := add!(errors, #"user-doesnt-exist");
      find-participant(*blog*, user) =>
        errors := add!(errors, #"participant-exists-already");
    end case;
  end if;

  if (~ instance?(role, <string>) | role = "")
    errors := add!(errors, #"role");
  else
    role := as(<symbol>, role);
  end if;

  unless (member?(role, $roles))
    errors := add!(errors, #"unknown-role");
  end unless;

  case
    ~ empty?(errors) =>
      dynamic-bind (*errors* = errors, *form* = current-request().request-query-values)
        next-method();
      end;
    edit? => begin
        role & (*participant*.participant-role := role);
        redirect-to(*participant*);
      end;
    add? => begin
        let participant = make(<blog-participant>, participant-role: role,
          participant-user: find-user(user));
        add-participant(*blog*, participant);
        redirect-to(participant);
      end;
  end case;
end;

define method participants-responder (blog :: <blog>)
  let request = current-request();

  authenticate();

  let add? :: <boolean> = (get-query-value("add") = #t);
  let edit? :: <boolean> = (get-query-value("edit") = #t);
  let remove? :: <boolean> = (get-query-value("remove") = #t);
  let view? :: <boolean> = (request.request-url-tail ~= "");

  let action :: <symbol> = case
      add? => #"add-participant";
      edit? => #"edit-participant";
      remove? => #"remove-participant";
      view? => #"view-participant";
      otherwise => #"list-participants";
    end case;

  let participant = if (edit? | view? | remove?)
      find-participant(blog, request.request-url-tail);
    end if;

  if ((edit? | view? | remove?) & ~ participant)
    respond-to(#"get", *non-participating-user-page*);
  else
    with-permission(action, blog: blog, participant: participant)
      dynamic-bind(*action* = action, *blog* = blog, *participant* = participant)
        select (request.request-method)
          #"get" => case
              add? | edit? => respond-to(#"get", *edit-participant-page*);
              remove? => respond-to(#"get", *remove-participant-page*);
              view? => respond-to(#"get", *view-participant-page*);
              otherwise => respond-to(#"get", *view-participants-page*);
            end case;
          #"post" => case
              add? | edit? => respond-to(#"post", *edit-participant-page*);
              remove? => begin
                  remove-participant(blog, participant);
                  redirect-to(blog);
                end;
            end case;
        end select;
      end;
    end;
  end if;
end;

