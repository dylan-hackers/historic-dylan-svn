module: xmpp
synopsis: 
author: 
copyright: 

define class <xmpp-stream> (<element>)
  inherited slot name-with-proper-capitalization = "stream:stream";
  inherited slot attributes = vector(make(<attribute>, name: "xmlns:stream", value: "http://etherx.jabber.org/streams"));
  virtual slot id,
    init-keyword: id:;
  virtual slot to,
    init-keyword: to:;
  virtual slot from,
    init-keyword: from:;
  virtual slot language,
    init-keyword: language:;
  virtual slot version,
    init-keyword: version:;
  virtual slot type,
    init-keyword: type:;
end class <xmpp-stream>;

define method initialize (stream :: <xmpp-stream>, #key id, to, from, language, version, type = #"client", #all-keys)
  next-method();

  if (id)
    id-setter(id, stream);
  end if;
  if (to)
    to-setter(to, stream);
  end if;
  if (from)
    from-setter(from, stream);
  end if;
  if (language)
    language-setter(language, stream);
  end if;
  if (version)
    version-setter(version, stream);
  end if;
  type-setter(type, stream);
end method initialize;

define method version (stream :: <xmpp-stream>)
 => (res :: false-or(<version>));
  let a = attribute(stream, "version");
  a & as(<version>, a.attribute-value);
end method version;

define method version-setter (version, stream :: <xmpp-stream>)
 => (res);
  add-attribute(stream, make(<attribute>, name: "version", value: as(<string>, version)));
  version;
end method version-setter;

define method type (stream :: <xmpp-stream>)
 => (res :: false-or(<symbol>));
  let type = namespace(stream);
  if (type = "jabber:server")
    #"server";
  elseif (type = "jabber:client")
    #"client";
  else
    #f;
  end if;
end method type;

define method type-setter (type, stream :: <xmpp-stream>)
 => (res);
  if (type = #"server")
    add-namespace(stream, "jabber:server");
  elseif (type = #"client")
    add-namespace(stream, "jabber:client");
  end if;
  type;
end method type-setter;
