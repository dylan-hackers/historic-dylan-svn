module: xmpp
synopsis: 
author: 
copyright: 

define constant $presence-types = #(#"error", #"probe", #"subscribe", #"subscribed",
                                    #"unavailable", #"unsubscribe", #"unsubscribed");
define constant $presence-shows = #(#"away", #"chat", #"dnd", #"xa");
                                    
define class <presence> (<stanza>)
  inherited slot name-with-proper-capitalization = "presence";
  virtual slot show;
  virtual slot status;
  virtual slot priority;
end class <presence>;

define method initialize (presence :: <presence>, #key show, status, priority, #all-keys)
  next-method();

  if (show)
    show-setter(show, presence);
  end if;
  if (status)
    status-setter(status, presence);
  end if;
  if (priority)
    priority-setter(priority, presence);
  end if;
end method;

define method type-setter (type, presence :: <presence>)
 => (res);
  if (member?(type, $presence-types))
    next-method();
  end if;
  type;
end method type-setter;

define method type (presence :: <presence>)
 => (res :: false-or(<symbol>));
  let type = next-method();
  if (member?(as(<symbol>, type), $presence-types))
    as(<symbol>, type);
  else
    #f;
  end if;
end method type;

define method show (presence :: <presence>)
 => (res :: false-or(<string>));
  let show = elements(presence, "show");
  if (~ empty?(show))
    show := first(show).text;
    if (member?(as(<symbol>, show), $presence-shows))
      show;
    else
      #f;
    end if;
  else
    #f;
  end if;
end method show;

define method show-setter (show, presence :: <presence>)
 => (res);
  if (member?(as(<symbol>, show), $presence-shows))
    replace-element-text(presence, "show", as(<string>, show));
  end if;
  show;
end method show-setter;

define method show-setter (show == #f, presence :: <presence>)
 => (res);
  remove-element(presence, "show");
  show;
end method show-setter;
 
define method status (presence :: <presence>)
   => (res :: false-or(<string>));
  let status = elements(presence, "status");
  if (~ empty?(status))
    first(status).text;
  else
    #f;
  end if;
end method status;

define method status-setter (status :: <string>, presence :: <presence>)
 => (res :: <string>);
  replace-element-text(presence, "status", status);
  status;
end method status-setter;

define method status-setter (status == #f, presence :: <presence>)
 => (res);
  remove-element(presence, "status");
  status;
end method status-setter;

define method add-status (presence :: <presence>, status :: <string>, #key language: lang)
  let element = make(<element>, name: "status");
  element.text := status;
  element.language := lang;
  add-element(presence, element);
end method add-status;

define method priority (presence :: <presence>)
 => (res :: false-or(<integer>));
  let priorities = elements(presence, "priority");
  if (~ empty?(priorities))
    string-to-integer(first(priorities).text);
  else
    #f;
  end if;
end method priority;

define method priority-setter (priority :: <integer>, presence :: <presence>)
 => (res :: <integer>);
  replace-element-text(presence, "priority", integer-to-string(priority));
  priority;
end method priority-setter;

define method priority-setter (priority == #f, presence :: <presence>)
 => (res);
  remove-element(presence, "priority");
  priority;
end method priority-setter;

define method as (class == <presence>, element :: <element>)
 => (res :: <presence>)
  let presence = make(<presence>);
  import-element(presence, element);
  presence;
end method as;
