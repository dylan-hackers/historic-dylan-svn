Module:    %productions
Synopsis:  Reading XML using Meta.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
remodulized-by: Douglas M. Auclair


// 1
define method parse-document(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, prolog, element, misc);
    [parse-prolog(prolog),
     parse-element(element),
     loop(parse-misc(misc))];
    values(index, make(<document>, children: vector(element)));
  end with-meta-syntax;
end method parse-document;

// 2
define method parse-char(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, pi-target, char, space1);
    [type(<all-chars>, c)];
    values(index, #t);
  end with-meta-syntax;
end method parse-char;

// 3
define method parse-s(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c);
    [type(<space>, c), loop(type(<space>, c))];
    values(index);
  end with-meta-syntax;  
end method parse-s;

// 4
define method parse-namechar(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index) 
    variables (d);
    {type(<letter>, d), type(<digit>, d), type(<other-name-char>, d)};
    values(index, d);
  end with-meta-syntax;      
end method parse-namechar;

// 5
define method parse-name(string, #key start = 0, end: stop)
  with-collector into-vector vect, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, v);
      [[{type(<letter>, c), '_', ':'}, do(collect(c))],
       loop([parse-namechar(v), do(collect(v))])];
      values(index, as(<string>,vect));
    end with-meta-syntax;
  end with-collector;
end method parse-name;

// 6
define method parse-names(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, name, space1);
    [parse-name(name), 
     loop([parse-s(space1),
           parse-name(name)])];
    values(index, #t);
  end with-meta-syntax;
end method parse-names;

// 7
define method parse-nmtoken(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, namechar);
    [parse-namechar(namechar), loop(parse-namechar(namechar))];
    values(index, #t);
  end with-meta-syntax;
end method parse-nmtoken;

// 8
define method parse-nmtokens(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, nmtoken, space1);
    [parse-nmtoken(nmtoken), 
     loop([parse-s(space1),
           parse-nmtoken(nmtoken)])];
    values(index, #t);
  end with-meta-syntax;
end method parse-nmtokens;


// 10 - needs parse-reference??
define method parse-att-value(string, #key start = 0, end: stop)
  local method is-not-special?(char :: <character>)
      char ~= '<' & char ~= '&' & char ~= '\"';
  end method is-not-special?;

  with-collector into-vector vect, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c);
      ['\"',
       loop([test(is-not-special?, c), do(collect(c))]),
       '\"'];
      values(index, as(<string>, vect));
    end with-meta-syntax;
  end with-collector;
end method parse-att-value;

// 14
define method parse-char-data(string, #key start = 0, end: stop)
  local method is-not-special?(char :: <character>)
      char ~= '<' & char ~= '&';
  end method is-not-special?;

  local method first-match(string, #key start = 0, end: stop)
    with-collector into-vector vect, collect: collect;
      with-meta-syntax parse-string (string, start: start, pos: index)
        variables(c);
        [test(is-not-special?, c), do(collect(c)), 
         loop([test(is-not-special?, c), do(collect(c))])];
        values(index, as(<string>, vect));
      end with-meta-syntax;
    end with-collector;
  end method first-match;
/*
  local method second-match(string, #key start = 0, end: stop)
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, space1);
      [loop(test(is-not-special?, c)),
       "]]>",
       loop(test(is-not-special?, c))];
      values(index, make(<text-node>,
                         name: "text",
                         value: );
    end with-meta-syntax;
   end method second-match; */
/*
  let (m2-index, m2-result) = 
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, match2);
      [second-match(match2)];
      values(index, make(<text-node>,
                         name: "text",
                         value: );
    end with-meta-syntax;

  if(m2-index)
    format-out("matched m2: %= %=\n", m2-index, m2-result);
    values(start, #f)
  else
    let (m1-index, m1-result) = 
*/
      with-meta-syntax parse-string (string, start: start, pos: index)
        variables(c, match1);
        [first-match(match1)];
        values(index, make(<text-node>,
                           text: match1));      
      end with-meta-syntax;
/*
    format-out("matched m1: %= %=\n", m1-index, m1-result);
    values(m1-index, m1-result);

  end if;
*/
end method parse-char-data;

// 15
define method parse-comment(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, char);
    ["<!--",
     loop(parse-char(char)),
     "-->"];
    values(index, #t);
  end with-meta-syntax;
end method parse-comment;

// 16
define method parse-pi(string, #key start = 0, end: stop)
  let quirks = one-of('=', '\"', '/', '.', '_', '-');
  let chars =  type-union(<all-chars>, quirks);
  let any-chars? = rcurry(instance?, chars);

  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, pi-target, char, space1, eq);
    ["<?",
     parse-pi-target(pi-target),
     loop(test(any-chars?)),
     "?>"];
    values(index, #t);
  end with-meta-syntax;
end method parse-pi;

// 17
define method parse-pi-target(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, name);
    [parse-name(name)];
    values(index, #t);
  end with-meta-syntax;
end method parse-pi-target;

// 22
define method parse-prolog(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, xml-decl, misc, doc);
    [parse-xml-decl(xml-decl), 
     loop({parse-misc(misc), parse-doctype(doc)})];
    values(index, #t);
  end with-meta-syntax;
end method parse-prolog;

// 22 helper
define method parse-doctype(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(s, name, file);
    ["<!DOCTYPE", parse-s(s), parse-name(name), parse-s(s),
     "SYSTEM", parse-s(s), parse-att-value(file), ">"];
    values(index, #t);
  end with-meta-syntax;
end method parse-doctype;

// 23
define method parse-xml-decl(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, version-info, space1);
    ["<?xml",
     parse-version-info(version-info),
     loop(parse-s(space1)),
     "?>"];
    values(index, #t);
  end with-meta-syntax;
end method parse-xml-decl;

// 24
define method parse-version-info(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, space1, eq, version-num);
    [parse-s(space1),
     "version",
     parse-eq(eq),
     {['\'',
       parse-version-num(version-num),
       '\''],
      ['\"',
       parse-version-num(version-num),
       '\"']}];
    values(index, #t);
  end with-meta-syntax;
end method parse-version-info;

// 25
define method parse-eq(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, space1, space2);
    [loop(parse-s(space1)), 
     '=',
     loop(parse-s(space2))];
    values(index, #t);
  end with-meta-syntax;
end method parse-eq;

// 26
define method parse-version-num(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c);
    [type(<version-number>, c), loop(type(<version-number>, c))];
    values(index, #t);
  end with-meta-syntax;
end method parse-version-num;

// 27
define method parse-misc(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, pi, space1, nova);
    {parse-pi(pi), parse-s(space1), parse-comment(nova)};
    values(index, #t);
  end with-meta-syntax;
end method parse-misc;


// 39
define method parse-element(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, element, tag-name, attributes, content, e-tag);
    {[parse-empty-elem-tag(tag-name, attributes), set!(content, "")],
     [parse-s-tag(tag-name, attributes), 
      parse-content(content), 
      parse-e-tag(e-tag)]}; 
    values(index, make(<element>, 
                       children: content,
                       tag-name: tag-name,
                       attributes: attributes));
  end with-meta-syntax;
end method parse-element;

// 40
define method parse-s-tag(string, #key start = 0, end: stop)
  with-collector into-vector attributes, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, tag-name, space, attribute);
      ['<', 
       parse-name(tag-name), 
       loop([parse-s(space), parse-attribute(attribute), do(collect(attribute))]),
       loop(parse-s(space)),
       '>'];
      values(index, tag-name, attributes);
    end with-meta-syntax;
  end with-collector;
end method parse-s-tag;

// 41
define method parse-attribute(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, name, eq, att-value);
    [parse-name(name), 
     parse-eq(eq), 
     parse-att-value(att-value)];
    values(index, make(<attribute>, 
                       name: name,
                       value: att-value));
  end with-meta-syntax;
end method parse-attribute;

// 42
define method parse-e-tag(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, name, space1);
    ["</", 
     parse-name(name), 
     loop(parse-s(space1)),
     '>'];
    values(index, #t);
  end with-meta-syntax;
end method parse-e-tag;

// 43 - Need CDSect, PI, Comment
define method parse-content(string, #key start = 0, end: stop)
  with-collector into-vector contents, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, content);
      [loop([{parse-element(content),
              parse-char-data(content),
              parse-reference(content),
              parse-pi(content)}, do(collect(content))])];
      values(index, contents);
    end with-meta-syntax;
  end with-collector;
end method parse-content;


// 44
define method parse-empty-elem-tag(string, #key start = 0, end: stop)
  with-collector into-vector attributes, collect: collect;
    with-meta-syntax parse-string (string, start: start, pos: index)
      variables(c, tag-name, space, attribute);
      ['<', 
        parse-name(tag-name),
        loop([parse-s(space), parse-attribute(attribute), do(collect(attribute))]),
        loop(parse-s(space)),
        "/>"];
      values(index, tag-name, attributes);
    end with-meta-syntax;
  end with-collector;
end method parse-empty-elem-tag;


// 66
define method parse-char-ref(string, #key start = 0, end: stop)
  local method parse-1st-option(string, #key start = 0, end: stop)
    with-collector into-vector vect, collect: collect;
      with-meta-syntax parse-string (string, start: start, pos: index)
        variables(c);
        ["&#", 
         [type(<digit>, c), do(collect(c)), loop([type(<digit>, c), do(collect(c))])],
         ';'];
        values(index, make(<char-reference>, value: as(<string>, vect)));
      end with-meta-syntax;
    end with-collector;
  end method parse-1st-option;

  local method parse-2nd-option(string, #key start = 0, end: stop)
    with-collector into-vector vect, collect: collect;
      with-meta-syntax parse-string (string, start: start, pos: index)
        variables(c);
        ["&#x", 
         [type(<hex-digit>, c), do(collect(c)), loop([type(<hex-digit>, c), do(collect(c))])],
         ';'];
        values(index, make(<char-reference>, value: as(<string>, vect)));
      end with-meta-syntax;
    end with-collector;
  end method parse-2nd-option;

  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, result);
    {parse-1st-option(result), parse-2nd-option(result)};
    values(index, result);
  end with-meta-syntax;
end method parse-char-ref;



// 67
define method parse-reference(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, result);
    [{parse-entity-ref(result), parse-char-ref(result)}];
    values(index, result);
  end with-meta-syntax;
end method parse-reference;

// 68
define method parse-entity-ref(string, #key start = 0, end: stop)
  with-meta-syntax parse-string (string, start: start, pos: index)
    variables(c, name);
    ['&', parse-name(name), ';'];
    values(index, make(<entity-reference>, value: name));
  end with-meta-syntax;
end method parse-entity-ref;

