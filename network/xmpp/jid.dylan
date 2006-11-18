module: xmpp
synopsis: 
author: 
copyright:

define class <jid> (<object>)
  slot node :: false-or(<string>) = #f,
    init-keyword: node:;
  slot domain :: <string>,
    required-init-keyword: domain:;
  slot resource :: false-or(<string>) = #f,
    init-keyword: resource:;
end class <jid>;                  

define method initialize (jid :: <jid>, #key node, domain, resource, #all-keys)
  next-method();

/*
  if (size(jid.node | "") > 1023)
    signal("Node exceeds 1023 chars.");
  end if;
  if (~ jid.domain)
    signal
  else
     (size(jid.domain) > 1023)
    signal("Node exceeds 1023 chars.");
  end if;
  if (size(jid.resource | "") > 1023)
    signal("Node exceeds 1023 chars.");
  end if;
*/
end method initialize;

define method as (class == <string>, jid :: <jid>)
 => (res :: <string>)
  let res = "";
  if (jid.node)
    res := concatenate(jid.node, "@");
  end if;
  if (jid.domain)
    res := concatenate(res, jid.domain);
  end if;
  if (jid.resource)
    res := concatenate(res, "/", jid.resource);
  end if;
  res;
end method as;

define method as (class == <jid>, jid :: <string>)
 => (res :: <jid>)
  let node = #f;
  let domain = #f;
  let resource = #f;
  
  if (subsequence-position(jid, "@"))
    let splitted-node = split(jid, '@');
    node := splitted-node[0];
    domain := splitted-node[1];
    if (subsequence-position(domain, "/"))
      let splitted-domain = split(domain, '/');
      domain := splitted-domain[0];
      resource := splitted-domain[1];
    end if;
  elseif (subsequence-position(jid, "/"))
    let splitted-node = split(jid, '/');
    domain := splitted-node[0];
    resource := splitted-node[1];
  else
    domain := jid;
  end if;
  make(<jid>, node: node, domain: domain, resource: resource);
end method as;

define method print-object(jid :: <jid>, stream :: <stream>) => ()
  format(stream, "%s", as(<string>, jid));
end method print-object;

define method strip (jid :: <jid>)
 => (res :: <jid>)
  make(<jid>, node: jid.node, domain: jid.domain);
end method strip;

define method strip! (jid :: <jid>)
 => (res :: <jid>)
  jid.resource := #f;
  jid;
end method strip!;

define method \= (jid1 :: <jid>, jid2 :: <jid>)
   => (ans :: <boolean>);
     as(<string>, jid1) = as(<string>, jid2);
end method \=; 
