module: turboblog
author: turbo24prg

define method permission-error (action, #key)
  respond-to(#"get", *unprivileged-page*);
end;

define method authentication-error (action, #key)
  respond-to(#"get", *not-logged-in-page*);
end;

define constant $roles :: <vector> = #[#"contributor", #"editor"];

define method role-at-least? (user :: <blog-user>, required-role :: <symbol>, blog :: <blog>)
 => (at-least? :: <boolean>);
  case
    user.administrator? => #t;
    find-participant(blog, user.username) =>
      find-key($roles, method (role)
          role = find-participant(blog, user.username).participant-role
        end) >=
      find-key($roles, method (role)
          role = required-role
        end);
    otherwise => #f;
  end case;
end;

define method permitted? (action :: one-of(#"administrate", #"add-blog"), #key
 affected-user :: false-or(<blog-user>) = authenticated-user())
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (affected-user.administrator?)
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action :: one-of(#"configure-blog", #"remove-blog", 
  #"add-participant", #"edit-participant", #"remove-participant",
  #"add-link", #"edit-link", #"remove-link",
  #"add-file", #"edit-file", #"remove-file"), #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 blog :: false-or(<blog>) = *blog*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (role-at-least?(affected-user, #"editor", blog))
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action :: one-of(#"edit-user", #"remove-user"), #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 user :: false-or(<blog-user>) = *user*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (affected-user.administrator? | affected-user = user)
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action == #"add-entry", #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 blog :: false-or(<blog>) = *blog*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (role-at-least?(affected-user, #"contributor", blog))
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action :: one-of(#"edit-entry", #"remove-entry"), #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 blog :: false-or(<blog>) = *blog*, entry :: false-or(<blog-entry>) = *entry*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (role-at-least?(affected-user, #"contributor", blog) &
      if (find-participant(blog, affected-user.username) & 
       find-participant(blog, affected-user.username).participant-role = #"contributor")
        let participant = find-participant(blog, affected-user.username);
        participant & member?(participant, entry.authors)
      else #t end if)
        error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action == #"edit-comment", #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 comment :: false-or(<blog-comment>) = *comment*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (comment.comment-user = affected-user)
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;

define method permitted? (action == #"remove-comment", #key
 affected-user :: false-or(<blog-user>) = authenticated-user(),
 entry :: false-or(<blog-entry>) = *entry*, 
 comment :: false-or(<blog-comment>) = *comment*)
 => (permitted? :: <boolean>);
  if (affected-user)
    unless (role-at-least?(affected-user, #"editor", entry.entry-blog))
      error(make(<permission-error>));
    end unless;
  else
    error(make(<authentication-error>));
  end if;
end;
