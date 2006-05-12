module: xmpp
synopsis: 
author: 
copyright: 

define class <stanza> (<element>)
  virtual slot id;
  virtual slot to;
  virtual slot from;
  virtual slot type;
  virtual slot language;
  virtual slot x;
end class <stanza>;

define method initialize (stanza :: <stanza>, #key id, to, from, type, language, x, #all-keys)
  next-method();

  if (id)
    id-setter(id, stanza);
  end if;
  if (to)
    to-setter(to, stanza);
  end if;
  if (from)
    from-setter(from, stanza);
  end if;
  if (type)
    type-setter(type, stanza);
  end if;
  if (language)
    language-setter(language, stanza);
  end if;
/*  if (x)
    x-setter(x, stanza);
  end if;
*/
end method initialize;

/*
define method error (stanza :: <stanza>)
 => (res :: false-or(<attribute>));
  attribute(stanza, "error");
end method error;
*/

define method x (stanza :: <stanza>)
 => (res :: false-or(<x>));
  let xs = elements(stanza, "x");
  if (~ empty?(xs))
    first(xs);
  else
    #f;
  end if;
end method x;

define method answer (stanza :: <stanza>, #key import :: <boolean> = #t)
 => (res);
  let answer-stanza = make(object-class(stanza));
  if (import)
    import-element(answer-stanza, stanza);
  end if;
  answer-stanza.to := stanza.from;
  answer-stanza.from := stanza.to;
  answer-stanza.id := stanza.id;
  answer-stanza;
end method answer;

define method elements (stanza :: <stanza>, element-name :: <string>)
 => (res :: <sequence>)
  let res = next-method();
  let tmp = #f;
  let name = as(<symbol>, element-name);
  if (~ empty?(res) & element(*element-translation*, name, default: #f))
    if (object-class(first(res)) ~= *element-translation*[name])
      for (i from 0 below size(res))
        tmp := make(*element-translation*[name]);
        import-element(tmp, res[i]);
        res[i] := tmp;
      end for; 
    end if;
  end if;
  res;
end method elements;

define method add-element (stanza :: <stanza>, node :: <element>)  
  if (element(*element-translation*, node.name, default: #f) & object-class(node) ~= *element-translation*[node.name])
    let tmp = make(*element-translation*[node.name]);
    import-element(tmp, node);
    next-method(stanza, tmp);
  else
    next-method();
  end if;
end method add-element;

define macro element-definer 
  { define element ?identifier:name () ?body:* end } =>
  { define class "<" ## ?identifier ## ">" (<element>)
      inherited slot name-with-proper-capitalization = ?"identifier";
      ?body
    end;
    *element-translation*[?#"identifier"] := "<" ## ?identifier ## ">"; }
end macro element-definer;
