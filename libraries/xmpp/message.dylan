module: xmpp
synopsis: 
author: 
copyright: 

define constant $message-types = #(#"error", #"chat", #"groupchat", #"headline", #"normal");

define class <message> (<stanza>)
  inherited slot name-with-proper-capitalization = "message";
  virtual slot body;
  virtual slot subject;
  virtual slot thread;
end class <message>;

define method initialize (message :: <message>, #key body, subject, thread, #all-keys)
  next-method();

  if (body)
    body-setter(body, message);
  end if;
  if (subject)
    subject-setter(subject, message);
  end if;
  if (thread)
    thread-setter(thread, message);
  end if;
end method initialize;

define method type-setter (type, message :: <message>)
 => (res);
  if (member?(type, $message-types))
    next-method();
  end if;
  type;
end method type-setter;

define method type (message :: <message>)
 => (res :: false-or(<symbol>));
  let type = next-method();
  if (member?(as(<symbol>, type), $message-types))
    as(<symbol>, type);
  else
    #f;
  end if;
end method type;

define method body (message :: <message>)
 => (res :: false-or(<string>));
  let bodies = elements(message, "body");
  if (~ empty?(bodies))
    first(bodies).text;
  else
    #f;
  end if;
end method body;

define method body-setter (body :: <string>, message :: <message>)
 => (res :: <string>);
  replace-element-text(message, "body", body); 
  body;
end method body-setter;

define method body-setter (body == #f, message :: <message>)
 => (res);
  remove-element(message, "body");
  body;
end method body-setter;

define method add-body (message :: <message>, body :: <string>, #key language: lang)
  let element = make(<element>, name: "body");
  element.text := body;
  element.language := lang;
  add-element(message, element);
end method add-body;

define method subject (message :: <message>)
 => (res :: false-or(<string>));
  let subjects = elements(message, "subject");
  if (~ empty?(subjects))
    first(subjects).text;
  else
    #f;
  end if;
end method subject;

define method subject-setter (subject :: <string>, message :: <message>)
 => (res :: <string>);
  replace-element-text(message, "subject", subject);
  subject;
end method subject-setter;

define method subject-setter (subject == #f, message :: <message>)
 => (res);
  remove-element(message, "subject");
  subject;
end method subject-setter;

define method add-subject (message :: <message>, subject :: <string>, #key language: lang)
  let element = make(<element>, name: "subject");
  element.text := subject;
  element.language := lang;
  add-element(message, element);
end method add-subject;

define method thread (message :: <message>)
 => (res :: false-or(<string>));
  let threads = elements(message, "thread");
  if (~ empty?(threads))
    first(threads).text;
  else
    #f;
  end if;
end method thread;

define method thread-setter (thread :: false-or(<string>), message :: <message>)
 => (res :: <string>);
  replace-element-text(message, "thread", thread);
  thread;
end method thread-setter;

define method thread-setter (thread == #f, message :: <message>)
 => (res);
  remove-element(message, "thread");
  thread;
end method thread-setter;
 
define method normalize (message :: <message>)
  let tmp = #f;
  if (~ empty?(message.node-children))
    let subject = find-key(message.node-children, method (a)
                                                    a.name = #"subject";
                                                  end);
    if (subject)
      tmp := message.node-children[0];
      message.node-children[0] := message.node-children[subject];
      message.node-children[subject] := tmp;
    end if;
    let body = find-key(message.node-children, method (a)
                                                  a.name = #"body";
                                                end);
    if (body)
      tmp := message.node-children[1];
      message.node-children[1] := message.node-children[body];
      message.node-children[body] := tmp;
    end if;
  end if;
end method normalize;
