module: changes
author: Hannes Mehnert <hannes@mehnert.org>

define class <feed> (<object>)
  /* slot CommonAttributes */
  slot author :: <list>, init-keyword: author:;
  slot category :: <list> = #(), init-keyword: category:;
  slot contributor :: <list> = #(), init-keyword: contributor:;
  slot generator :: false-or(<generator>) = #f, init-keyword: generator:;
  slot icon :: false-or(<uri>) = #f, init-keyword: icon:;
  slot identifier :: <uri>, init-keyword: identifier:;
  slot link :: <list> = #(), init-keyword: link:;
  slot logo :: false-or(<uri>) = #f, init-keyword: logo:;
  slot rights :: false-or(<text>) = #f, init-keyword: rights:;
  slot subtitle :: false-or(<text>) = #f, init-keyword: subtitle:;
  slot title :: <text>, init-keyword: title:;
  slot updated :: <date>, init-keyword: updated:;
  /* repeated slot extensionElement */
  slot entry :: <list> = #(), init-keyword: entry:;
end;

define class <entry> (<object>)
  /* slot CommonAttributes */
  slot author :: <list> = #(), init-keyword: author:;
  slot category :: <list> = #(), init-keyword: category:;
  slot content :: false-or(<content>) = #f, init-keyword: content:;
  slot contributor :: <list> = #(), init-keyword: contributor:;
  slot identifier :: <uri>, init-keyword: identifier:;
  slot link :: <list> = #(), init-keyword: link:;
  slot published :: false-or(<date>) = #f, init-keyword: published:;
  slot rights :: false-or(<text>) = #f, init-keyword: rights:;
  slot source :: false-or(<source>) = #f, init-keyword: source:;
  slot summary :: false-or(<text>) = #f, init-keyword: summary:;
  slot title :: <text>, init-keyword: title:;
  slot updated :: <date>, init-keyword: updated:;
  /* repeated slot extensionElement */
end;

define abstract class <content> (<object>)
  /* slot CommonAttributes */
  slot type :: <string>, init-keyword: type:;
  slot content :: <text>, init-keyword: content:;
end;

define class <inline-text-content> (<content>)
  inherited slot content :: <text>, init-keyword: content:;
end;

define class <inline-xhtml-content> (<content>)
  inherited slot content :: <xhtml-div>, init-keyword: content:;
end;

define class <inline-other-content> (<content>)
  inherited slot content :: <text>, init-keyword: text:;
end;

define class <out-of-line-content> (<content>)
  inherited slot content :: <source>, init-keyword: content:;
end;

define class <person> (<object>)
  slot person-name :: <text>, init-keyword: name:;
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot email :: false-or(<email>) = #f, init-keyword: email:;
end;

define class <date> (<object>)
end; 

define class <category> (<object>)
  slot term :: <text>, init-keyword: term:;
  slot scheme :: false-or(<uri>) = #f, init-keyword: scheme:;
  slot label :: false-or(<text>) = #f, init-keyword: label:;
end;

define constant <text> = <string>;

define class <xhtml-div> (<object>)
end;

define constant <uri> = <string>;

define constant <email> = <string>;

define class <generator> (<object>)
  slot uri :: false-or(<uri>) = #f, init-keyword: uri:;
  slot version :: false-or(<text>) = #f, init-keyword: version:;
  slot text :: <text>, init-keyword: text:;
end;

define class <link> (<object>)
  slot href :: <uri>, init-keyword: uri:;
  slot rel :: <uri>, init-keyword: rel:;
  slot type :: <string>, init-keyword: type:;
  slot hreflang :: <string>, init-keyword: hreflang:;
  slot title :: false-or(<text>) = #f, init-keyword: title:;
  slot length :: false-or(<text>) = #f, init-keyword: length:;
end;
    
define constant <source> = <feed>;

define method generate-xhtml (feed :: <feed>)
  with-xml()
    div {
      h1(concatenate("Recent changes: ", feed.title)),
      text(feed.subtitle),
      ul { do(do(method(x) collect(generate-xhtml(x)) end, feed.entry)) }
    }
  end;
end;

define method generate-xhtml (entry :: <entry>)
  with-xml()
    li {
      do(collect(generate-xhtml(entry.updated))),
      text(entry.title), //link to content
      do(do(method(x) collect(generate-xhtml(x)) end, entry.author))
      //link to each author
    }
  end;
end;

define method generate-xhtml (person :: <person>)
end;

define method generate-xhtml (date :: <date>)
end;

define method generate-atom (feed :: <feed>)
  with-xml-builder()
    feed (xmlns => "http://www.w3.org/2005/Atom")
    {
      title(feed.title),
      subtitle(feed.subtitle),
      updated { do(collect(generate-atom(feed.updated))) },
      id(feed.identifier),
      do(do(method(x) collect(generate-atom(x)) end, feed.link)),
      rights(feed.rights),
      do(collect(generate-atom(feed.generator))),
      do(do(method(x) collect(generate-atom(x)) end, feed.entry))
    } //missing: category, contributor, icon, logo
  end; 
end;

define method generate-atom (link :: <link>)
  with-xml()
    link (rel => link.rel,
          type => link.type,
          href => link.href)
  end //missing: title, hreflang, length
end;

define method generate-atom (person :: <person>)
end;

define method generate-atom (date :: <date>)
//  with-xml()
//  end;
end;

define method generate-atom (generator :: <generator>)
  with-xml()
    generator (uri => generator.uri, version => generator.version)
    {
      text(generator.text)
    }
  end;
end;

define method generate-atom (entry :: <entry>)
  with-xml()
    entry
    {
      title(entry.title),
      do(do(method(x) collect(generate-atom(x)) end, entry.link)),
      id(entry.identifier),
      updated { do(collect(generate-atom(entry.updated))) },
      published { do(collect(generate-atom(entry.published))) },
      do(do(method(x) collect(generate-atom(x)) end, entry.author)),
      do(do(method(x) collect(generate-atom(x)) end, entry.contributor)),
      do(collect(generate-atom(entry.content))),
    } //missing: category, summary
  end;
end;

define method generate-atom (con :: <content>)
  with-xml()
    text(con.content)
  end;
end;