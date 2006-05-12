module: xmpp
synopsis: 
author: 
copyright:

define class <xmpp-error> (<element>, <error>)
  virtual slot condition;
  virtual slot description;
end class <xmpp-error>;

define method initialize (error :: <xmpp-error>, #key condition, description, #all-keys)
  next-method();

  if (condition)
    condition-setter(condition, error);
  end if;
  if (description)
    description-setter(description, error);
  end if;
end method initialize;

define method description-setter (description == #f, error :: <xmpp-error>)
 => (res);
  remove-element(error, "text");
  description;
end method description-setter;

define method description (error :: <xmpp-error>)
 => (res :: false-or(<string>));
  let descriptions = elements(error, "text");
  if (~ empty?(descriptions))
    first(descriptions).text;
  else
    #f;
  end if;
end method description;
