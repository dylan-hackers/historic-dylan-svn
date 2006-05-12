module: xmpp
synopsis: 
author: 
copyright: 

define class <iq> (<stanza>)
  inherited slot name-with-proper-capitalization = "iq";
  virtual slot query;
  virtual slot vcard;
end class <iq>;

define method initialize (iq :: <iq>, #key query, vcard, #all-keys)
  next-method();

  if (query)
    query-setter(query, iq);
  end if;
  if (vcard)
    vcard-setter(vcard, iq);
  end if;

end method;

define method type-setter (type, iq :: <iq>)
 => (res);
  if (member?(type, #(#"error", #"get", #"set", #"result")))
    next-method();
  end if;
  type;
end method type-setter;

define method type (iq :: <iq>)
 => (res :: false-or(<symbol>));
  let type = next-method();
  if (member?(as(<symbol>, type), #(#"error", #"get", #"set", #"result")))
    as(<symbol>, type);
  else
    #f;
  end if;
end method type;

define method vcard (iq :: <iq>)
 => (res :: false-or(<vcard>));
  let vcards = elements(iq, "vCard");
  if (~ empty?(vcards))
    first(vcards);
  else 
    #f;
  end if;
end method vcard;

define method vcard-setter (vcard :: <element>, iq :: <iq>)
 => (res);
  remove-element(iq, vcard.name);
  add-element(iq, vcard);
  vcard;
end method vcard-setter; 

define method query (iq :: <iq>)
 => (res :: false-or(<query>));
  let queries = elements(iq, "query");
  if (~ empty?(queries))
    first(queries);
  else 
    #f;
  end if;
end method query;

define method query-setter (query :: <element>, iq :: <iq>)
 => (res);
  remove-element(iq, query.name);
  add-element(iq, query);
  query;
end method query-setter;

define method make-query (#key type, to: jid)
 => (iq :: <iq>);
  let iq = make(<iq>, type: type, to: jid);
  add-element(iq, make(<query>));
  iq;
end method make-query;

define method make-vcard (#key type = #"get", to: jid)
 => (iq :: <iq>);
  let iq = make(<iq>, type: type, to: jid);
  add-element(iq, make(<vcard>));
  iq;
end method make-vcard;

define method make-authentication (jid :: <jid>, password :: <string>)
 => (iq :: <iq>);
  let iq = make(<iq>, type: #"set");
  let query = with-xml()
    query(xmlns => "jabber:iq:auth") {
      username(jid.node),
      password(password),
      do(if (jid.resource)
          with-xml() resource(jid.resource) end;
        end)
    }
  end;
  add-element(iq, query);
  iq;
end method make-authentication;

define method make-registration (#key username :: false-or(<string>), password :: false-or(<string>))
 => (iq :: <iq>);
  let iq = make(<iq>, type: #"set");
  let query = with-xml() query(xmlns => "jabber:iq:register") end;
  if (username) 
    add-element(query, with-xml() username(username) end);
  end if;
  if (password) 
    add-element(query, with-xml() password(password) end);
  end if;
  add-element(iq, query);
  iq;
end method make-registration;
