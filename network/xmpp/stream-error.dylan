module: xmpp
synopsis: 
author: 
copyright:

define constant $xmpp-stream-error-conditions =
    #(#"bad-format", #"bad-namespace-prefix", #"conflict",
    #"connection-timeout", #"host-gone", #"host-unknown",
    #"improper-addressing", #"internal-server-error",
    #"invalid-from", #"invalid-id", #"invalid-namespace",
    #"invalid-xml", #"not-authorized", #"policy-violation",
    #"remote-connection-failed", #"resource-constraint",
    #"restricted-xml", #"see-other-host", #"system-shutdown",
    #"undefined-condition", #"unsupported-encoding",
    #"unsupported-stanza-type", #"unsupported-version",
    #"xml-not-well-formed");

define class <xmpp-stream-error> (<xmpp-error>)
  inherited slot name-with-proper-capitalization = "stream:error";
end class <xmpp-stream-error>;

define method description-setter (description :: <string>, error :: <xmpp-stream-error>)
 => (res :: <string>);
  replace-element-text(error, "text", description);
  add-namespace(element(error, "text"), "urn:ietf:params:xml:ns:xmpp-streams");
  description;
end method description-setter;

define method condition-setter (condition :: <symbol>, error :: <xmpp-stream-error>)
 => (res :: <symbol>);
 if (member?(condition, $xmpp-stream-error-conditions))
    let defined-condition = make(<element>, name: as(<string>, condition)); 
    add-namespace(defined-condition, "urn:ietf:params:xml:ns:xmpp-streams");
    for (condition in $xmpp-stream-error-conditions)
      remove-element(error, condition);
    end for;
    add-element(error, defined-condition);
    normalize(error);
  end if;
  condition;
end method condition-setter;

define method normalize (error :: <xmpp-stream-error>)
  let tmp = #f;
  if (~ empty?(error.node-children))
    let condition = find-key(error.node-children, method (a)
                                                      member?(a.name, $xmpp-stream-error-conditions);
                                                    end);
    if (condition)
      tmp := error.node-children[0];
      error.node-children[0] := error.node-children[condition];
      error.node-children[condition] := tmp;
    else
      // signal an error! (there should be an condition element!)
    end if;
    if (find-key(error.node-children, method (a)
                                          member?(a.name, $xmpp-stream-error-conditions);
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
