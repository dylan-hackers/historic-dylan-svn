module: xmpp
synopsis: 
author: 
copyright:

define constant $xmpp-stanza-error-types = #(#"cancel", #"continue", #"modify", #"auth", #"wait");
define constant $xmpp-stanza-error-conditions =
  #(#"bad-request", #"conflict", #"feature-not-implemented",
    #"forbidden", #"gone", #"internal-server-error",
    #"item-not-found", #"jid-malformed", #"not-acceptable",
    #"not-allowed", #"payment-required", #"recipient-unavailable",
    #"redirect registration-required", #"remote-server-not-found",
    #"remote-server-timeout", #"resource-constraint",
    #"service-unavailable", #"subscription-required",
    #"undefined-condition", #"unexpected-request");
  
define class <xmpp-stanza-error> (<xmpp-error>)
  inherited slot name-with-proper-capitalization = "error";
  virtual slot type;
end class <xmpp-stanza-error>;

define method initialize (error :: <xmpp-stanza-error>, #key type, #all-keys)
  next-method();
  
  if (type)
    type-setter(type, error);
  end if;
end method initialize;

define method type-setter (type, iq :: <xmpp-stanza-error>)
 => (res);
  if (member?(type, $xmpp-stanza-error-types))
    next-method();
  end if;
  type;
end method type-setter;

define method type (iq :: <xmpp-stanza-error>)
 => (res :: false-or(<symbol>));
  let type = next-method();
  if (member?(as(<symbol>, type), $xmpp-stanza-error-types))
    as(<symbol>, type);
  else
    #f;
  end if;
end method type;

define method description-setter (description :: <string>, error :: <xmpp-stanza-error>)
 => (res :: <string>);
  replace-element-text(error, "text", description);
  add-namespace(element(error, "text"), "urn:ietf:params:xml:ns:xmpp-stanzas");
  description;
end method description-setter;

define method condition-setter (condition :: <symbol>, error :: <xmpp-stanza-error>)
 => (res :: <symbol>);
 if (member?(condition, $xmpp-stanza-error-conditions))
    let defined-condition = make(<element>, name: as(<string>, condition)); 
    add-namespace(defined-condition, "urn:ietf:params:xml:ns:xmpp-stanzas");
    for (condition in $xmpp-stanza-error-conditions)
      remove-element(error, condition);
    end for;
    add-element(error, defined-condition);
    normalize(error);
  end if;
  condition;
end method condition-setter;

define method normalize (error :: <xmpp-stanza-error>)
  let tmp = #f;
  if (~ empty?(error.node-children))
    let condition = find-key(error.node-children, method (a)
                                                      member?(a.name, $xmpp-stanza-error-conditions);
                                                    end);
    if (condition)
      tmp := error.node-children[0];
      error.node-children[0] := error.node-children[condition];
      error.node-children[condition] := tmp;
    else
      // signal an error! (there should be an condition element!)
    end if;
    if (find-key(error.node-children, method (a)
                                          member?(a.name, $xmpp-stanza-error-conditions);
                                        end, skip: 1))
      // signal an error! (there shouldn't be a second condition!)
    end if;
    let description = find-key(error.node-children, method (a)
                                                        a.name = #"text";
                                                      end);
    if (description)
      tmp := error.node-children[1];
      error.node-children[1] := error.node-children[description];
      error.node-children[description] := tmp;
    end if;
  end if; 
end method normalize;
